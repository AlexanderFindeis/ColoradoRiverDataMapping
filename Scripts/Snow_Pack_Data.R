library(dplyr) 
library(tidyr)
library(sf)

options(timeout = 300)

print("Pulling Snow Pack Data")

Upper_Green <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140401_Upper_Green.csv") %>%
    mutate(HUC6 = "Upper Green")

White_Yampa <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140500_White-Yampa.csv") %>%
    mutate(HUC6 = "White-Yampa")

Colorado_Headwaters <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140100_Colorado_Headwaters.csv") %>%
    mutate(HUC6 = "Colorado Headwaters")

Lower_Green <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140600_Lower_Green.csv") %>%
    mutate(HUC6 = "Lower Green")

Upper_Colorado_Dolores <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140300_Upper_Colorado-Dolores.csv") %>%
    mutate(HUC6 = "Upper Colorado-Dolores")

Gunnison <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140200_Gunnison.csv") %>%
    mutate(HUC6 = "Gunnison")

Upper_San_Juan <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140801_Upper_San_Juan.csv") %>%
    mutate(HUC6 = "Upper San Juan")

Lower_San_Juan <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140802_Lower_San_Juan.csv") %>%
    mutate(HUC6 = "Lower San Juan")

UpperColorado_DirtyDevil <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140700_Upper_Colorado-Dirty_Devil.csv") %>%
    mutate(HUC6 = "Upper Colorado-Dirty Devil")

LowerColorado_LakeMead <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/150100_Lower_Colorado-Lake_Mead.csv") %>%
    mutate(HUC6 = "Lower Colorado-Lake Mead")

Little_Colorado <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/150200_Little_Colorado.csv") %>%
    mutate(HUC6 = "Little Colorado")

Verde <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/150602_Verde.csv") %>%
    mutate(HUC6 = "Verde")

Salt <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/150601_Salt.csv") %>%
    mutate(HUC6 = "Salt")

Upper_Gila <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/150400_Upper_Gila.csv") %>%
    mutate(HUC6 = "Upper Gila")

Colorado_Basin_SWE <- bind_rows(Upper_Green, White_Yampa, Colorado_Headwaters, Lower_Green, Upper_Colorado_Dolores, Gunnison, Upper_San_Juan, Lower_San_Juan, UpperColorado_DirtyDevil, LowerColorado_LakeMead, Little_Colorado, Verde, Salt, Upper_Gila) %>%
    group_by(date) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(HUC6 = "Full Basin")

Colorado_Basin_SWE_Full <- bind_rows(Upper_Green, White_Yampa, Colorado_Headwaters, Lower_Green, Upper_Colorado_Dolores, Gunnison, Upper_San_Juan, Lower_San_Juan, UpperColorado_DirtyDevil, LowerColorado_LakeMead, Little_Colorado, Verde, Salt, Upper_Gila, Colorado_Basin_SWE) %>%
    group_by(date, HUC6) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

### !!! - Year fields are not calendar years, they are snow years
### treating them like calendar years assigns values to fall and winter of this year that haven't happened year
### Need to adjust dates to account for this
data_long <- Colorado_Basin_SWE_Full %>%
  group_by(HUC6) %>%
  pivot_longer(
    cols = starts_with("X"),       # select only X-prefixed year columns
    names_to  = "Snow_Year",
    names_prefix = "X",            # strips the X prefix from year values
    values_to = "Average_SWE"
  ) %>% ungroup() %>%
  mutate(
    Snow_Year = as.integer(Snow_Year),
    Month = as.integer(substr(date,1,2)),
    #Day = as.integer(substr(date,4,5)),
    Calendar_Year = if_else(Month >= 10, Snow_Year - 1, Snow_Year),
    Snow_Date = as.Date(paste(Snow_Year, date, sep = "-"), format = "%Y-%m-%d"),
    Calendar_Date = as.Date(paste(Calendar_Year, date, sep = "-"), format = "%Y-%m-%d")
  ) %>%
  rename(Median_91_20 = `Median...91..20.`, Month_Day = date) %>%
  filter(Snow_Year > 100) # Removes values for percentile statistics that were pivoted as if they were years

write.csv(data_long, "Pages/Snow Pack/Data/Snow Water Equivalent.csv")



### Days to peak average SWE
Days2Peak <- data_long %>%
    filter(!is.na(Average_SWE)) %>%
    group_by(Snow_Year, HUC6) %>%
    slice_max(Average_SWE, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Reference_Date = as.Date(paste((Snow_Year - 1), "10", "01", sep = "-"), format = "%Y-%m-%d"),
           Days2Peak = as.integer(Snow_Date - Reference_Date),
           Date = as.Date(paste(Snow_Year, "01", "01", sep = "-"), format = "%Y-%m-%d")) %>%
    select(HUC6, Peak_Avg_SWE=Average_SWE, Snow_Year, Date, Days2Peak)

write.csv(Days2Peak, "Pages/Snow Pack/Data/Days_to_Peak.csv")


##### Join data to geojson
UB_HUC6 <- st_read("GIS_Data/UpperBasin_HUC6.geojson")
LB_HUC6 <- st_read("GIS_Data/LowerBasin_HUC6.geojson")

SWE_Current <- read.csv("Pages/Snow Pack/Data/Snow Water Equivalent.csv") %>%
    filter(!is.na(Average_SWE)) %>%
    filter(HUC6 != "Full Basin") %>%
    group_by(HUC6) %>%
    filter(Calendar_Date == max(Calendar_Date, na.rm = TRUE))

HUC6_Full <- union(UB_HUC6, LB_HUC6) %>%
    right_join(SWE_Current, by = c("name" = "HUC6")) %>%
    mutate(across(c("Average_SWE"), round, 2))

st_write(HUC6_Full, "GIS_Data/SWE_HUC6.geojson", append=FALSE, delete_dsn = TRUE)

print("Snow Pack Data Compilation Complete")
