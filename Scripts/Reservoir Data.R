library(dplyr)
library(zoo)
library(lubridate)
library(sf)
library(httr2)

options(timeout = 300)

# base_url <- 'https://data.usbr.gov/rise/api/catalog-item'
# response <- request(base_url) |> 
#   req_url_path_append(
#     'page=2',
#     'itemsPerPage=25'
#   ) |> 
#   req_perform()
# response

# Lake Mead Elevation
Mead.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/921/csv/49.csv") %>% mutate(Reservoir = "Lake Mead")
Sys.sleep(30)
# Lake Mead Storage
Mead.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/921/csv/17.csv") %>% mutate(Reservoir = "Lake Mead")
Sys.sleep(30)
# Lake Mead Releases
# Mead.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/921/csv/43.csv") %>% mutate(Reservoir = "Lake Mead")
# Sys.sleep(20)

# Lake Powell Elevation
Powell.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/49.csv") %>% mutate(Reservoir = "Lake Powell")
Sys.sleep(30)
# Lake Powell Storage
Powell.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/17.csv") %>% mutate(Reservoir = "Lake Powell")
Sys.sleep(30)
# Lake Powell Releases
# Powell.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/43.csv") %>% mutate(Reservoir = "Lake Powell")
# Sys.sleep(30)
# # Lake Powell Inflows
# Powell.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/30.csv") %>% mutate(Reservoir = "Lake Powell")
# Sys.sleep(20)

Flaming.Gorge.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/917/csv/49.csv") %>% mutate(Reservoir = "Flaming Gorge")
Sys.sleep(30)
Flaming.Gorge.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/917/csv/17.csv") %>% mutate(Reservoir = "Flaming Gorge")
Sys.sleep(30)
# Flaming.Gorge.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/917/csv/43.csv") %>% mutate(Reservoir = "Flaming Gorge")
# Sys.sleep(30)
# Flaming.Gorge.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/917/csv/30.csv") %>% mutate(Reservoir = "Flaming Gorge")
# Sys.sleep(20)

Mohave.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/922/csv/49.csv") %>% mutate(Reservoir = "Lake Mohave") 
Sys.sleep(30)
Mohave.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/922/csv/17.csv") %>% mutate(Reservoir = "Lake Mohave") 
Sys.sleep(30)
# Mohave.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/922/csv/43.csv") %>% mutate(Reservoir = "Lake Mohave") 
# Sys.sleep(20)

Navajo.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/920/csv/49.csv") %>% mutate(Reservoir = "Navajo Reservoir") 
Sys.sleep(30)
Navajo.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/920/csv/17.csv") %>% mutate(Reservoir = "Navajo Reservoir") 
Sys.sleep(30)
# Navajo.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/920/csv/43.csv") %>% mutate(Reservoir = "Navajo Reservoir") 
# Sys.sleep(30)
# Navajo.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/920/csv/30.csv") %>% mutate(Reservoir = "Navajo Reservoir") 
# Sys.sleep(20)

Straw.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/962/csv/49.csv") %>% mutate(Reservoir = "Strawberry Reservoir") 
Sys.sleep(30)
Straw.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/962/csv/17.csv") %>% mutate(Reservoir = "Strawberry Reservoir") 
Sys.sleep(30)
# Straw.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/962/csv/43.csv") %>% mutate(Reservoir = "Strawberry Reservoir") 
# Sys.sleep(30)
# Straw.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/962/csv/30.csv") %>% mutate(Reservoir = "Strawberry Reservoir") 
# Sys.sleep(20)

Blue.Mesa.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/913/csv/49.csv") %>% mutate(Reservoir = "Blue Mesa Reservoir") 
Sys.sleep(30)
Blue.Mesa.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/913/csv/17.csv") %>% mutate(Reservoir = "Blue Mesa Reservoir") 
Sys.sleep(30)
# Blue.Mesa.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/913/csv/43.csv") %>% mutate(Reservoir = "Blue Mesa Reservoir") 
# Sys.sleep(30)
# Blue.Mesa.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/913/csv/30.csv") %>% mutate(Reservoir = "Blue Mesa Reservoir") 
# Sys.sleep(20)

Havasu.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/923/csv/49.csv") %>% mutate(Reservoir = "Lake Havasu") 
Sys.sleep(30)
Havasu.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/923/csv/17.csv") %>% mutate(Reservoir = "Lake Havasu") 
Sys.sleep(30)
# Havasu.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/923/csv/43.csv") %>% mutate(Reservoir = "Lake Havasu") 
# Sys.sleep(20)

Granby.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/100010/csv/49.csv") %>% mutate(Reservoir = "Granby Reservoir") 
Sys.sleep(30)
Granby.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/100010/csv/17.csv") %>% mutate(Reservoir = "Granby Reservoir") 
Sys.sleep(30)

## Elevation Data
# Elevation Plot
Res.Elv <- rbind(Mead.Elv, Powell.Elv, Flaming.Gorge.Elv, Mohave.Elv, Navajo.Elv, Straw.Elv, Blue.Mesa.Elv, Havasu.Elv, Granby.Elv) %>%
  select(Date = datetime, Elevation = pool.elevation, Reservoir) %>%
  mutate(
    Year = as.integer(substr(Date, 0,4)),
    Month = as.integer(substr(Date, 6,7)),
    Day = as.integer(substr(Date, 9,10)),
    Water_Year = as.integer(if_else(Month >= 10, Year + 1, Year)),
    Date = as.Date(Date)) %>%
  arrange(Date) %>%
  #mutate(One_Year_Average = zoo::rollmean(Elevation, k = 365, fill = NA, align = 'right')) %>%
  mutate(Elv_10yr_Average = zoo::rollmean(Elevation, k = 3650, fill = NA, align = 'right')) %>% # 10 years * 365 days
  mutate(Elv_30yr_Average = zoo::rollmean(Elevation, k = 10950, fill = NA, align = 'right')) %>% # 30 years * 365 days
  group_by(Reservoir) %>%
  mutate(Elv_1yr_Ago = lag(Elevation, n = 365)) %>%
  ungroup()

# Average Elevation of each day of the year (e.g. August 1 across all years)
Elv.Day.Average.10yr <- Res.Elv %>%
  filter(Date >= (max(Date) - years(10))) %>%
  group_by(Month, Day, Reservoir) %>%
  summarise(Elv_Day_Avg_10yr = mean(Elevation, na.rm=T))

Elv.Day.Average.30yr <- Res.Elv %>%
  filter(Date >= (max(Date) - years(30))) %>%
  group_by(Month, Day, Reservoir) %>%
  summarise(Elv_Day_Avg_30yr = mean(Elevation, na.rm=T))

Elv.Day.Average.Pre2000 <- Res.Elv %>%
  filter(Year > 2000) %>%
  group_by(Month, Day, Reservoir) %>%
  summarise(Elv_Day_Avg_Pre2000 = mean(Elevation, na.rm=T))

Res.Elv.Output <- Res.Elv %>%
  left_join(Elv.Day.Average.10yr, by = c("Month", "Day", "Reservoir")) %>%
  left_join(Elv.Day.Average.30yr, by = c("Month", "Day", "Reservoir")) %>%
  left_join(Elv.Day.Average.Pre2000, by = c("Month", "Day", "Reservoir")) %>%
  select(-Year, -Month, -Day)

write.csv(Res.Elv.Output, "Pages/Reservoirs/Data/Reservoir_Elevation.csv")


## Storage Data
Res.Stor <- rbind(Mead.Stor, Powell.Stor, Flaming.Gorge.Stor, Mohave.Stor, Navajo.Stor, Straw.Stor, Blue.Mesa.Stor, Havasu.Stor, Granby.Stor) %>%
  mutate(
    Date = as.Date(datetime),
    Storage_MAF = storage / 1000000
  ) %>%
  select(Date, Storage = storage, Storage_MAF, Reservoir)


Res.Total.Stor <- Res.Stor %>%
  group_by(Date) %>%
  filter(!is.na(Storage)) %>%
  summarize(
    Storage = sum(Storage, rm.na = T),
    Storage_MAF = sum(Storage_MAF)
  ) %>%
  mutate(Reservoir = "Total")

Res.Stor.Bind <- rbind(Res.Stor, Res.Total.Stor) %>%
  mutate(
    Year = as.integer(substr(Date, 0,4)),
    Month = as.integer(substr(Date, 6,7)),
    Day = as.integer(substr(Date, 9,10)),
    Water.Year = if_else(Month >= 10, Year + 1, Year))

Reservoir.Capacity <- Res.Stor.Bind %>%
  distinct(Reservoir) %>%
  mutate(Max_Capacity = case_when(
    Reservoir == "Lake Mead" ~ 26120000,
    Reservoir == "Lake Powell" ~ 24322000,
    Reservoir == "Flaming Gorge" ~ 3671100,
    Reservoir == "Lake Mohave" ~ 1809800,
    Reservoir == "Navajo Reservoir" ~ 1696000,
    Reservoir == "Strawberry Reservoir" ~ 1106500,
    Reservoir == "Blue Mesa Reservoir" ~ 827940,
    Reservoir == "Lake Havasu" ~ 619400,
    Reservoir == "Granby Reservoir" ~ 539758,
    Reservoir == "Total" ~ 60712498
)) %>%
  mutate(Max_Capacity_MAF = Max_Capacity / 1000000)

# Comment out, doesn't work for storage. would work for inflows and outflows, maybe evaporation depending on data
# Stor.CumSum <- Res.Stor.Bind %>% group_by(Water.Year, Reservoir) %>%
#   mutate(
#     WY_Storage_CumSum = cumsum(Storage),
#     WY_StorageMAF_CumSum = cumsum(Storage_MAF)) %>%
#   ungroup() %>%
#   select(Date, Reservoir, WY_Storage_CumSum, WY_StorageMAF_CumSum)

# Average Storage of each day of the year (e.g. August 1 across all years)
Stor.Day.Average.10yr <- Res.Stor.Bind %>%
  filter(Date >= (max(Date) - years(10))) %>%
  group_by(Month, Day, Reservoir) %>%
  summarise(Stor_Day_Avg_10yr = mean(Storage_MAF, na.rm=T))

Stor.Day.Average.30yr <- Res.Stor.Bind %>%
  filter(Date >= (max(Date) - years(30))) %>%
  group_by(Month, Day, Reservoir) %>%
  summarise(Stor_Day_Avg_30yr = mean(Storage_MAF, na.rm=T))

Stor.Day.Average.Pre2000 <- Res.Stor.Bind %>%
  filter(Year > 2000) %>%
  group_by(Month, Day, Reservoir) %>%
  summarise(Stor_Day_Avg_Pre2000 = mean(Storage_MAF, na.rm=T))

Res.Stor.Output <- Res.Stor.Bind %>%
  # left_join(Stor.CumSum, by = c("Date", "Reservoir")) %>%
  left_join(Stor.Day.Average.10yr, by = c("Month", "Day", "Reservoir")) %>%
  left_join(Stor.Day.Average.30yr, by = c("Month", "Day", "Reservoir")) %>%
  left_join(Stor.Day.Average.Pre2000, by = c("Month", "Day", "Reservoir")) %>%
  select(-Year, -Month, -Day) %>%
  group_by(Reservoir) %>%
  mutate(StorMAF_1yr_Ago = lag(Storage_MAF, n = 365)) %>%
  ungroup() %>%
  left_join(Reservoir.Capacity, by = c("Reservoir")) %>%
  mutate(Percent_Full_MAF = (Storage_MAF / Max_Capacity_MAF)*100)

write.csv(Res.Stor.Output, "Pages/Reservoirs/Data/Reservoir_Storage.csv")

### Spatial Data
reservoirs <- st_read("GIS_Data/Colorado_River_Basin_Reservoirs.geojson")

latest_storage <- Res.Stor.Output %>%
  group_by(Reservoir) %>%
  slice_tail(n=1)

latest_elevation <- Res.Elv.Output %>%
  group_by(Reservoir) %>%
  slice_tail(n=1)

latest_res_data <- latest_storage %>%
  left_join(latest_elevation, by = c("Reservoir", "Date")) %>%
  mutate(lat = case_when(
    Reservoir == "Lake Mead" ~ 36.02,
    Reservoir == "Lake Powell" ~ 36.94,
    Reservoir == "Flaming Gorge" ~ 40.91,
    Reservoir == "Lake Mohave" ~ 35.20,
    Reservoir == "Navajo Reservoir" ~ 36.80,
    Reservoir == "Strawberry Reservoir" ~ 40.14,
    Reservoir == "Blue Mesa Reservoir" ~ 38.45,
    Reservoir == "Lake Havasu" ~ 34.40,
    Reservoir == "Granby Reservoir" ~ 40.14
  )) %>%
  mutate(long = case_when(
    Reservoir == "Lake Mead" ~ -114.74,
    Reservoir == "Lake Powell" ~ -111.48,
    Reservoir == "Flaming Gorge" ~ -109.42,
    Reservoir == "Lake Mohave" ~ -114.57,
    Reservoir == "Navajo Reservoir" ~ -107.61,
    Reservoir == "Strawberry Reservoir" ~ -111.03,
    Reservoir == "Blue Mesa Reservoir" ~ -107.33,
    Reservoir == "Lake Havasu" ~ -114.14,
    Reservoir == "Granby Reservoir" ~ -105.87
  )) %>%
  filter(Reservoir != "Total") %>%
  mutate(across(where(is.numeric), round, 2))

latest_res_sf <- st_as_sf(latest_res_data, coords = c("long", "lat"), crs=4326)

st_write(latest_res_sf, "GIS_Data/Reservoirs.geojson", append=FALSE, delete_dsn = TRUE)
