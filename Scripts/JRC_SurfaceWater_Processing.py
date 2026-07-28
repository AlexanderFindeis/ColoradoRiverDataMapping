import os
import requests
import numpy as np
import geopandas as gpd
import rasterio
from rasterio.merge import merge
from rasterio.mask import mask
from rasterio.features import shapes
from shapely.geometry import shape
import gc

# ── Setup ─────────────────────────────────────────────────────────────────────
data_date    = 1984
cache_dir    = "GIS_Data/JRC"
os.makedirs(cache_dir, exist_ok=True)

# Load clip geometry
reservoir_clip = gpd.read_file("GIS_Data/JRC/Reservoir_Clip.geojson")

# ── Helper: download file ─────────────────────────────────────────────────────
def download_file(url, dest_path):
    print(f"  Downloading: {url}")
    response = requests.get(url, stream=True, timeout=120)
    response.raise_for_status()
    with open(dest_path, "wb") as f:
        for chunk in response.iter_content(chunk_size=8192):
            f.write(chunk)

# ── Helper: polygonize raster ─────────────────────────────────────────────────
def raster_to_polygons(raster_path):
    with rasterio.open(raster_path) as src:
        image     = src.read(1)
        transform = src.transform
        crs       = src.crs

        # Mask to only value 3 (permanent water)
        water_mask = (image == 3)

        if not water_mask.any():
            print("  No water pixels found")
            return None

        # Polygonize - only process pixels where water_mask is True
        results = shapes(image, mask=water_mask.astype(np.uint8), transform=transform)
        geoms   = [{"geometry": shape(geom), "value": int(val)} for geom, val in results]

    if len(geoms) == 0:
        return None

    gdf = gpd.GeoDataFrame(geoms, crs=crs)
    return gdf

# ── Main loop ─────────────────────────────────────────────────────────────────
while data_date < 2022:
    print(f"\nProcessing year: {data_date}")

    base_url = (
        f"https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GSWE/YearlyClassification/"
        f"LATEST/tiles/yearlyClassification{data_date}/"
        f"yearlyClassification{data_date}"
    )

    suffixes = [
        "-0000160000-0000240000.tif",
        "-0000160000-0000280000.tif",
        "-0000120000-0000240000.tif",
        "-0000120000-0000280000.tif"
    ]

    local_paths = [os.path.join(cache_dir, f"Ras{i+1}.tif") for i in range(4)]

    # ── Download tiles ────────────────────────────────────────────────────────
    for suffix, local_path in zip(suffixes, local_paths):
        download_file(base_url + suffix, local_path)

    # ── Merge rasters ─────────────────────────────────────────────────────────
    print("  Merging rasters...")
    src_files = [rasterio.open(p) for p in local_paths]

    merged_array, merged_transform = merge(src_files, method="first")

    # Get metadata from first file and update for merged output
    merged_meta = src_files[0].meta.copy()
    merged_meta.update({
        "driver":    "GTiff",
        "height":    merged_array.shape[1],
        "width":     merged_array.shape[2],
        "transform": merged_transform
    })

    # Close source files and free memory
    for src in src_files:
        src.close()
    del src_files
    gc.collect()

    # Write merged raster to disk
    merged_path = os.path.join(cache_dir, f"merged_{data_date}.tif")
    with rasterio.open(merged_path, "w", **merged_meta) as dest:
        dest.write(merged_array)

    del merged_array
    gc.collect()

    # ── Clip to reservoir area ────────────────────────────────────────────────
    print("  Clipping to reservoir area...")

    # Reproject clip geometry to match raster CRS if needed
    with rasterio.open(merged_path) as src:
        raster_crs = src.crs
        clip_geom  = reservoir_clip.to_crs(raster_crs)
        geom_list  = [geom.__geo_interface__ for geom in clip_geom.geometry]

        clipped_array, clipped_transform = mask(src, geom_list, crop=True, nodata=0)
        clipped_meta = src.meta.copy()
        clipped_meta.update({
            "height":    clipped_array.shape[1],
            "width":     clipped_array.shape[2],
            "transform": clipped_transform,
            "nodata":    0
        })

    # Write clipped raster to disk
    clipped_path = os.path.join(cache_dir, f"clipped_{data_date}.tif")
    with rasterio.open(clipped_path, "w", **clipped_meta) as dest:
        dest.write(clipped_array)

    del clipped_array
    gc.collect()

    # ── Polygonize ────────────────────────────────────────────────────────────
    print("  Polygonizing...")
    water_gdf = raster_to_polygons(clipped_path)

    if water_gdf is not None:
        export_path = os.path.join(cache_dir, f"JRC_{data_date}.geojson")
        water_gdf.to_file(export_path, driver="GeoJSON")
        print(f"  Written: {export_path} ({len(water_gdf)} features)")
    else:
        print(f"  No water features for year {data_date} - skipping export")

    # ── Cleanup ───────────────────────────────────────────────────────────────
    for p in local_paths + [merged_path, clipped_path]:
        if os.path.exists(p):
            os.remove(p)

    gc.collect()
    data_date += 1

print("\nDone")