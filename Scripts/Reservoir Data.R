library(dplyr)
library(zoo)
library(lubridate)

# Lake Mead Elevation
Mead.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/921/csv/49.csv") %>% mutate(Reservoir = "Lake Mead")
# Lake Mead Storage
Mead.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/921/csv/17.csv") %>% mutate(Reservoir = "Lake Mead")
# Lake Mead Releases
Mead.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/921/csv/43.csv") %>% mutate(Reservoir = "Lake Mead")

# Lake Powell Elevation
Powell.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/49.csv") %>% mutate(Reservoir = "Lake Powell")
# Lake Powell Storage
Powell.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/17.csv") %>% mutate(Reservoir = "Lake Powell")
# Lake Powell Releases
Powell.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/43.csv") %>% mutate(Reservoir = "Lake Powell")
# Lake Powell Inflows
Powell.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/30.csv") %>% mutate(Reservoir = "Lake Powell")

Flaming.Gorge.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/917/csv/49.csv") %>% mutate(Reservoir = "Flaming Gorge")
Flaming.Gorge.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/917/csv/17.csv") %>% mutate(Reservoir = "Flaming Gorge")
Flaming.Gorge.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/917/csv/43.csv") %>% mutate(Reservoir = "Flaming Gorge")
Flaming.Gorge.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/917/csv/30.csv") %>% mutate(Reservoir = "Flaming Gorge")

Mohave.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/922/csv/49.csv") %>% mutate(Reservoir = "Lake Mohave") 
Mohave.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/922/csv/17.csv") %>% mutate(Reservoir = "Lake Mohave") 
Mohave.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/922/csv/43.csv") %>% mutate(Reservoir = "Lake Mohave") 

Navajo.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/920/csv/49.csv") %>% mutate(Reservoir = "Navajo Reservoir") 
Navajo.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/920/csv/17.csv") %>% mutate(Reservoir = "Navajo Reservoir") 
Navajo.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/920/csv/43.csv") %>% mutate(Reservoir = "Navajo Reservoir") 
Navajo.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/920/csv/30.csv") %>% mutate(Reservoir = "Navajo Reservoir") 

Straw.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/962/csv/49.csv") %>% mutate(Reservoir = "Strawberry Reservoir") 
Straw.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/962/csv/17.csv") %>% mutate(Reservoir = "Strawberry Reservoir") 
Straw.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/962/csv/43.csv") %>% mutate(Reservoir = "Strawberry Reservoir") 
Straw.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/962/csv/30.csv") %>% mutate(Reservoir = "Strawberry Reservoir") 

Blue.Mesa.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/913/csv/49.csv") %>% mutate(Reservoir = "Blue Mesa Reservoir") 
Blue.Mesa.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/913/csv/17.csv") %>% mutate(Reservoir = "Blue Mesa Reservoir") 
Blue.Mesa.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/913/csv/43.csv") %>% mutate(Reservoir = "Blue Mesa Reservoir") 
Blue.Mesa.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/913/csv/30.csv") %>% mutate(Reservoir = "Blue Mesa Reservoir") 

Havasu.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/923/csv/49.csv") %>% mutate(Reservoir = "Lake Havasu") 
Havasu.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/923/csv/17.csv") %>% mutate(Reservoir = "Lake Havasu") 
Havasu.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/923/csv/43.csv") %>% mutate(Reservoir = "Lake Havasu") 

Granby.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/100010/csv/49.csv") %>% mutate(Reservoir = "Granby Reservoir") 
Granby.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/100010/csv/17.csv") %>% mutate(Reservoir = "Granby Reservoir") 

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
  mutate(Ten_Year_Average = zoo::rollmean(Elevation, k = 3650, fill = NA, align = 'right')) %>% # 10 years * 365 days
  mutate(Thirty_Year_Average = zoo::rollmean(Elevation, k = 10950, fill = NA, align = 'right')) %>% # 30 years * 365 days
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

