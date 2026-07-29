## Downloads data to the root directory
# Move script with API key .txt file to desired download directory or move data once downloaded

from bmi_topography import Topography


topo = Topography(
    dem_type="SRTMGL3",
    south=29.8,
    north=43.461,
    west=-115.8,
    east=-105.5,
    output_format="GTiff",
    cache_dir=".",
    api_key="api key"
)


fname = topo.fetch()
print(fname)