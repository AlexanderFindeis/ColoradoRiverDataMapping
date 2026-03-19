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


## Elevation Data
# Elevation Plot
Res.Elv <- rbind(Mead.Elv, Powell.Elv, Flaming.Gorge.Elv) %>%
  select(datetime, Elevation = pool.elevation, Reservoir) %>%
  mutate(
    Year = substr(datetime, 1, 4),
    Elevation = as.numeric(Elevation)
  ) %>%
  group_by(Year, Reservoir) %>%
  summarize(Elevation = mean(Elevation, na.rm = TRUE), .groups = "drop") %>%
  mutate(Year = as.integer(Year))
write.csv(Res.Elv, "Colorado_Data_Website/Pages/Reservoirs/Data/Reservoir_Elevation.csv")


## Storage Data
Res.Stor <- rbind(Mead.Stor, Powell.Stor, Flaming.Gorge.Stor) %>%
  mutate(
    Date = as.Date(datetime),
    'Storage MAF' = storage / 1000000
  ) %>%
  select(Date, Storage = storage, `Storage MAF`, Reservoir)

Res.Total.Stor <- Res.Stor %>%
  group_by(Date) %>%
  summarize(
    Storage = sum(Storage, rm.na = T),
    'Storage MAF' = sum(`Storage MAF`)
  ) %>%
  mutate(Reservoir = "Total")

Res.Stor.Output <- rbind(Res.Stor, Res.Total.Stor)

write.csv(Res.Stor.Output, "Colorado_Data_Website/Pages/Reservoirs/Data/Reservoir_Storage.csv")
