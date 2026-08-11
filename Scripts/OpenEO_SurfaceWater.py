import openeo
from openeo.processes import if_
import geopandas as gpd
import json
import rasterio
import rasterio.features
from rasterio.fill import fillnodata
import numpy as np
import time
import os
from datetime import datetime, date, timedelta

res_areas    = "GIS_Data/Surface_Water/Reservoir_Clip.geojson"
gdf          = gpd.read_file(res_areas)
geojson_dict = json.loads(gdf.to_json())
total_bounds = gdf.total_bounds

connection = openeo.connect(
    "https://openeo.dataspace.copernicus.eu"
).authenticate_oidc()

Year = 2025


# ── Query Data ───────────────────────────────────────────────────────
cube = connection.load_collection(
    "SENTINEL2_L2A",
    spatial_extent = {
        "west":  total_bounds[0],
        "south": total_bounds[1],
        "east":  total_bounds[2],
        "north": total_bounds[3],
        "crs":   "EPSG:4326"
    },
    temporal_extent = [f"{Year}-01-01", f"{Year}-12-31"],
    bands           = ["B03", "B08"],
    max_cloud_cover = 1
)

cube  = cube.filter_spatial(geometries=geojson_dict)
green = cube.band("B03")
nir   = cube.band("B08")
ndwi  = (green - nir) / (green + nir)
ndwi  = ndwi.reduce_dimension(dimension="t", reducer="median")
#ndwi = ndwi.mask(ndwi <= 0.1) # Keeps pixels equal to or above 0.1 (rough threshold for water bodies)
ndwi = ndwi.apply(
    process=lambda x: if_(x > 0, 1, None)
)

# ── Submit batch job ──────────────────────────────────────────────────────────
job = ndwi.create_job(
    title       = f"NDWI_{Year}",
    description = f"NDWI water detection for {Year}",
    out_format  = "GTiff"
)

job.start_job()
print(f"Job submitted: {job.job_id}")

# ── Poll until complete ───────────────────────────────────────────────────────
while True:
    status = job.status()
    print(f"  Status: {status} [{datetime.now().strftime('%H:%M:%S')}]")

    if status == "finished":
        break
    elif status in ("error", "canceled"):
        print(f"Job failed: {status}")
        for log in job.logs():
            print(log)
        exit(1)

    time.sleep(300)

# ── Download results ──────────────────────────────────────────────────────────
out_dir    = f"GIS_Data/EO"
os.makedirs(out_dir, exist_ok=True)

print(f"Downloading results to {out_dir}...")
job.get_results().download_files(out_dir)

# Find the downloaded tif file
tif_files = [f for f in os.listdir(out_dir) if f.endswith(".tif")]
if not tif_files:
    print("No tif files found in output directory")
    exit(1)

tif_path = os.path.join(out_dir, tif_files[0])
print(f"Processing: {tif_path}")


# ── Convert raster to vector ──────────────────────────────────────────────────
with rasterio.open(tif_path) as src:
    image     = src.read(1)
    nodata    = src.nodata if src.nodata is not None else -9999
    mask      = (image != nodata).astype(np.uint8)
    transform = src.transform
    crs       = src.crs

results = list(rasterio.features.shapes(image, mask=mask, transform=transform))

if results:
    water_gdf = gpd.GeoDataFrame.from_features(
        [{"geometry": geom, "properties": {"value": float(val)}} for geom, val in results],
        crs = crs
    )
    water_gdf = water_gdf.to_crs("EPSG:4326")
    water_gdf.to_file(f"GIS_Data/Surface_Water/SW_{Year}.geojson", driver="GeoJSON")
    print(f"Saved: GIS_Data/Surface_Water/SW_{Year}.geojson ({len(water_gdf)} features)")
else:
    print("No features found in raster output")
