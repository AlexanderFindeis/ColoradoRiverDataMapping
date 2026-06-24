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
library(tidyverse)
geo <- sf::st_read("/vsizip//vsicurl/https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_current_M.zip")
st_write(geo, "GIS_Data/Current_Drought.geojson", delete_dsn = TRUE)

DM <- st_read("GIS_Data/Current_Drought.geojson") %>%
  st_transform(5070) %>% st_make_valid()
UB <- st_read("GIS_Data/UpperBasin_HUC2.geojson") %>%
  st_transform(5070) %>% st_make_valid()
LB <- st_read("GIS_Data/LowerBasin_HUC2.geojson") %>%
  st_transform(5070) %>% st_make_valid()

D0 <- DM %>% filter(DM == 0)
D1 <- DM %>% filter(DM == 1)
D2 <- DM %>% filter(DM == 2)
D3 <- DM %>% filter(DM == 3)
D4 <- DM %>% filter(DM == 4)

UB_D0 <- st_intersection(D0, UB)
UB_D0 <- UB_D0 %>%
  mutate(D0_SqKm = as.numeric(st_area(UB_D0) / 1000000),
         Percent_D0 = (D0_SqKm / areasqkm)*100)
UB_D1 <- st_intersection(D1, UB)
UB_D1 <- UB_D1 %>%
  mutate(D1_SqKm = as.numeric(st_area(UB_D1) / 1000000),
         Percent_D1 = (D1_SqKm / areasqkm)*100)
UB_D2 <- st_intersection(D2, UB)
UB_D2 <- UB_D2 %>%
  mutate(D2_SqKm = as.numeric(st_area(UB_D2) / 1000000),
         Percent_D2 = (D2_SqKm / areasqkm)*100)
UB_D3 <- st_intersection(D3, UB)
UB_D3 <- UB_D3 %>%
  mutate(D3_SqKm = as.numeric(st_area(UB_D3) / 1000000),
         Percent_D3 = (D3_SqKm / areasqkm)*100)
UB_D4 <- st_intersection(D4, UB)
UB_D4 <- UB_D4 %>%
  mutate(D4_SqKm = as.numeric(st_area(UB_D4) / 1000000),
         Percent_D4 = (D4_SqKm / areasqkm)*100)

LB_D0 <- st_intersection(D0, LB)
LB_D0 <- LB_D0 %>%
  mutate(D0_SqKm = as.numeric(st_area(LB_D0) / 1000000),
         Percent_D0 = (D0_SqKm / areasqkm)*100)
LB_D1 <- st_intersection(D1, LB)
LB_D1 <- LB_D1 %>%
  mutate(D1_SqKm = as.numeric(st_area(LB_D1) / 1000000),
         Percent_D1 = (D1_SqKm / areasqkm)*100)
LB_D2 <- st_intersection(D2, LB)
LB_D2 <- LB_D2 %>%
  mutate(D2_SqKm = as.numeric(st_area(LB_D2) / 1000000),
         Percent_D2 = (D2_SqKm / areasqkm)*100)
LB_D3 <- st_intersection(D3, LB)
LB_D3 <- LB_D3 %>%
  mutate(D3_SqKm = as.numeric(st_area(LB_D3) / 1000000),
         Percent_D3 = (D3_SqKm / areasqkm)*100)
LB_D4 <- st_intersection(D4, LB)
LB_D4 <- LB_D4 %>%
  mutate(D4_SqKm = as.numeric(st_area(LB_D4) / 1000000),
         Percent_D4 = (D4_SqKm / areasqkm)*100)

Drought_Stats <- data.frame(
  Basin = c("Upper Basin", "Upper Basin", "Upper Basin", "Upper Basin", "Upper Basin", "Lower Basin", "Lower Basin", "Lower Basin", "Lower Basin", "Lower Basin"),
  Drought_Level = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
  Drought_Area_SqKm = c(UB_D0$D0_SqKm[1], UB_D1$D1_SqKm[1], UB_D2$D2_SqKm[1], UB_D3$D3_SqKm[1], UB_D4$D4_SqKm[1], LB_D0$D0_SqKm[1], LB_D1$D1_SqKm[1], LB_D2$D2_SqKm[1], LB_D3$D3_SqKm[1], LB_D4$D4_SqKm[1]),
  Drought_Area_Percent= c(UB_D0$Percent_D0[1], UB_D1$Percent_D1[1], UB_D2$Percent_D2[1], UB_D3$Percent_D3[1], UB_D4$Percent_D4[1], LB_D0$Percent_D0[1], LB_D1$Percent_D1[1], LB_D2$Percent_D2[1], LB_D3$Percent_D3[1], LB_D4$Percent_D4[1])
) %>% replace_na(list(Drought_Area_SqKm = 0, Drought_Area_Percent = 0))

write.csv(Drought_Stats, "Pages/Drought Monitoring/Data/Drought_Stats.csv")
