import openeo
import geopandas as gpd

# Load basin boundary data
basin = gpd.read_file(f"GIS_Data/UpperBasin_HUC6.geojson")

# Connect to OpenEO
connection = openeo.connect(
    "https://openeo.dataspace.copernicus.eu"
).authenticate_oidc()

# Query data
cube = connection.load_collection(
    "CLMS_ETA_GLOBAL_300M_10DAILY_V1",
    spatial_extent = {
        "west":  -115.71,
        "south": 29.8,
        "east":  -105.6,
        "north": 43.5,
        "crs":   "EPSG:4326"
    },
    temporal_extent = ["2025-11-01", "2026-11-30"],
    bands           = ["et_ensemble"]
)


# Mask datacube
mask = basin.geometry.iloc[5]
masked_cube = cube.mask_polygon(mask)

# Calculate Sum
masked_cube.reduce_dimension(dimension="t", reducer=openeo.processes.mean)

# Download
masked_cube.download(f"D:\Water Data\Colorado River\ET\Sentinel\ET_Mean_Daily_mm.tif", format="GTiff")
