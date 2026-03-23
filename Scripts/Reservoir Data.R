library(dplyr)

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
    Date = as.Date(datetime)
  #  Year = substr(datetime, 1, 4),
  #  Elevation = as.numeric(Elevation)
   )
  #group_by(Year, Reservoir) %>%
  #summarize(Elevation = mean(Elevation, na.rm = TRUE), .groups = "drop") %>%
  #mutate(Year = as.integer(Year))
write.csv(Res.Elv, "Colorado_Data_Website/Pages/Reservoirs/Data/Reservoir_Elevation.csv")


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

Res.Stor.Output <- rbind(Res.Stor, Res.Total.Stor)

write.csv(Res.Stor.Output, "Colorado_Data_Website/Pages/Reservoirs/Data/Reservoir_Storage.csv")
