library(dplyr) 
library(tidyr)

Upper_Green <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140401_Upper_Green.csv") %>%
    mutate(HUC6 = "Upper Green")

White_Yampa <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140500_White-Yampa.csv") %>%
    mutate(HUC6 = "White Yampa")

Colorado_Headwaters <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140100_Colorado_Headwaters.csv") %>%
    mutate(HUC6 = "Colorado Headwaters")

Lower_Green <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140600_Lower_Green.csv") %>%
    mutate(HUC6 = "Lower Green")

Upper_Colorado_Dolores <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140300_Upper_Colorado-Dolores.csv") %>%
    mutate(HUC6 = "Upper Colorado - Dolores")

Gunnison <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140200_Gunnison.csv") %>%
    mutate(HUC6 = "Gunnison")

Upper_San_Juan <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140801_Upper_San_Juan.csv") %>%
    mutate(HUC6 = "Upper San Juan")

Lower_San_Juan <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140802_Lower_San_Juan.csv") %>%
    mutate(HUC6 = "Lower San Juan")

UpperColorado_DirtyDevil <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/140700_Upper_Colorado-Dirty_Devil.csv") %>%
    mutate(HUC6 = "Upper Colorado - Dirty Devil")

LowerColorado_LakeMead <- read.csv("https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/150100_Lower_Colorado-Lake_Mead.csv") %>%
    mutate(HUC6 = "Lower Colorado - Lake Mead")

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

data_long <- Colorado_Basin_SWE_Full %>%
  pivot_longer(
    cols = starts_with("X"),       # select only X-prefixed year columns
    names_to  = "Year",
    names_prefix = "X",            # strips the X prefix from year values
    values_to = "Average_SWE"
  ) %>%
  mutate(
    Date = as.Date(paste(Year, date, sep = "-"), format = "%Y-%m-%d")
  ) %>%
  rename(Median_91_20 = `Median...91..20.`)

write.csv(data_long, "Pages/Snow Pack/Data/Snow Water Equivalent.csv")