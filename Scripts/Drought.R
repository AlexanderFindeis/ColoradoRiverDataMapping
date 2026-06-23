# !! -- Not currently in use for processing. See Drought.qmd -- !!

library(dplyr)
library(sf)

drought_HUC6_UB <- read.csv("https://usdmdataservices.unl.edu/api/HUCStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=14&startdate=1/1/2000&enddate=01/01/2026&statisticsType=1&hucLevel=8") %>%
  mutate(Region = "Upper Basin")
drought_HUC6_LB <- read.csv("https://usdmdataservices.unl.edu/api/HUCStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=15&startdate=1/1/2000&enddate=01/01/2026&statisticsType=1&hucLevel=8") %>%
  mutate(Region = "Lower Basin")
drought_UB_Overall <- read.csv("https://usdmdataservices.unl.edu/api/HUCStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=14&startdate=1/1/2000&enddate=01/01/2026&statisticsType=1&hucLevel=2") %>%
  mutate(Region = "Upper Basin")
drought_LB_Overall <- read.csv("https://usdmdataservices.unl.edu/api/HUCStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=15&startdate=1/1/2000&enddate=01/01/2026&statisticsType=1&hucLevel=2") %>%
  mutate(Region = "Lower Basin")

drought_percent_area <- bind_rows(drought_HUC6_LB, drought_HUC6_UB, drought_LB_Overall, drought_UB_Overall)

write.csv(drought_percent_area, "Pages/Drought Monitoring/Data/Drought_Conditions_PercentArea.csv")

# Current drought geojson
library(sf)
library(dplyr)
geo <- sf::st_read("/vsizip//vsicurl/https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_current_M.zip")
st_write(geo, "GIS_Data/Current_Drought.geojson", delete_dsn = TRUE)

DM <- st_read("GIS_Data/Current_Drought.geojson") %>%
  st_transform(4326)
UB <- st_read("GIS_Data/UpperBasin_HUC2.geojson") %>%
  st_transform(4326)
LB <- st_read("GIS_Data/LowerBasin_HUC2.geojson") %>%
  st_transform(4326)

D0 <- geo %>% filter(DM == 0)
D1 <- geo %>% filter(DM == 1)
D2 <- geo %>% filter(DM == 2)
D3 <- geo %>% filter(DM == 3)
D4 <- geo %>% filter(DM == 4)

UB_D0 <- st_intersection(UB, D0)
