library(dplyr)
library(purrr)
library(zoo)
library(lubridate)
library(sf)
library(httr)
library(jsonlite)

options(timeout = 300)

# Query data
# Example: "https://data.usbr.gov/rise/api/result?itemsPerPage=2000&order%5BdateTime%5D=ASC&itemId=6123&dateTime%5Bafter%5D=20260501&dateTime%5Bstrictly_before%5D=20260522"
DataFetch <- function(ItemID, dateFrom, dateTo){
  base_url <- "https://data.usbr.gov/rise/api/result"
  res <- GET(
    base_url,
    query = list(
      itemsPerPage = "10000",
      `order[dateTime]` = "ASC",
      itemId = ItemID,
      `dateTime[after]` = dateFrom,
      `dateTime[before]` = dateTo
    ),
    add_headers(Accept = "application/vnd.api+json")
  )
  results <- fromJSON(content(res, "text"))
  data.frame(Date = unlist(results$data$attributes$dateTime), Result = unlist(results$data$attributes$result))
}

print("Beginning data fetch...")

# Date chunks 
# Necessary to keep data pulls within max of 10,000 records at once
date_chunks <- list(
  c("20200101", ""        ),
  c("20000101", "20191231"),
  c("19800101", "19991231"),
  c("19600101", "19791231"),
  c("19400101", "19591231"),
  c("",         "19391231")
)

# Reservoir definitions
# All USBR managed reservoirs with data above 100k AF
# Arizona based reservoirs not included in RISE API system
# Non USBR managed reservoirs not included yet
reservoirs <- tribble(
  ~name,               ~elv_id, ~stor_id,
  "Lake Mead",         6123,    6124,
  "Lake Powell",       508,     509,
  "Flaming Gorge",     341,     337,
  "Lake Mohave",       6133,    6134,
  "Navajo Reservoir",  612,     613,
  "Strawberry Reservoir", 782,  779,
  "Blue Mesa Reservoir",  78,      76,
  "Lake Havasu",       6128,    6129,
  "Granby Reservoir",  384,     383,
  "McPhee Reservoir",  572,     569,
  "Fontenelle Reservoir", 349,  347,
  "Starvation Reservoir", 767,  764,
  "Green Mountain Reservoir", 22, 21,
  "Vallecito Reservoir", 834,   831,
  "Lake Nighthorse",    505,    504,
  "Morrow Point Reservoir", 594, 592,
  "Taylor Park Reservoir", 796, 793,
  "Ruedi Reservoir",    712,    711
)

# Function to fetch all chunks for one item ID
fetch_all_chunks <- function(item_id, reservoir_name) {
  message(paste("Fetching:", reservoir_name, "| Item ID:", item_id))

  map_dfr(date_chunks, function(chunk) {
    tryCatch(
      DataFetch(item_id, chunk[1], chunk[2]),
      error = function(e) {
        message(paste("  Failed chunk:", chunk[1], "-", chunk[2], ":", e$message))
        NULL
      }
    )
  }) %>%
    mutate(
      Date      = as.Date(Date),
      Result    = as.numeric(Result),
      Reservoir = reservoir_name
    ) %>%
    distinct()
}

# Fetch all data
message("Beginning data fetch...")

Elevation <- pmap_dfr(
  reservoirs %>% select(name, item_id = elv_id),
  ~ fetch_all_chunks(..2, ..1)
)

Storage <- pmap_dfr(
  reservoirs %>% select(name, item_id = stor_id),
  # Purrr positional references to reservoir definitions tribble
  ~ fetch_all_chunks(..2, ..1)
)

message("All data fetched successfully")



## Elevation Data
print("Compiling data...")
# Elevation Plot
Res.Elv <- Elevation %>%
  rename(Elevation = Result) %>%
  mutate(
    Year = as.integer(substr(Date, 0,4)),
    Month = as.integer(substr(Date, 6,7)),
    Day = as.integer(substr(Date, 9,10)),
    Water_Year = as.integer(if_else(Month >= 10, Year + 1, Year))) %>%
  arrange(Date) %>%
  #mutate(One_Year_Average = zoo::rollmean(Elevation, k = 365, fill = NA, align = 'right')) %>%
  mutate(Elv_10yr_Average = zoo::rollmean(Elevation, k = 3650, fill = NA, align = 'right')) %>% # 10 years * 365 days
  mutate(Elv_30yr_Average = zoo::rollmean(Elevation, k = 10950, fill = NA, align = 'right')) %>% # 30 years * 365 days
  group_by(Reservoir) %>%
  mutate(Elv_1yr_Ago = lag(Elevation, n = 365)) %>%
  ungroup()

# Average Elevation of each day of the year (e.g. August 1 across all years)
Elv.Day.Average.10yr <- Res.Elv %>%
  # Filters data to the past ten years starting from the previous year (keeping data to only completed data)
  filter(Year <= (max(Year)-1) & Year >= (max(Year)-11)) %>%
  group_by(Month, Day, Reservoir) %>%
  summarise(Elv_Day_Avg_10yr = mean(Elevation, na.rm = TRUE), .groups = "drop")

Elv.Day.Average.30yr <- Res.Elv %>%
    # Filters data to the past ten years starting from the previous year (keeping data to only completed data)
  filter(Year <= (max(Year)-1) & Year >= (max(Year)-31)) %>%
  group_by(Month, Day, Reservoir) %>%
  summarise(Elv_Day_Avg_30yr = mean(Elevation, na.rm = TRUE), .groups = "drop")

# Removing for now since it is not being deployed on the site currently
# Elv.Day.Average.Pre2000 <- Res.Elv %>%
#   filter(Year > 2000) %>%
#   group_by(Month, Day, Reservoir) %>%
#   summarise(Elv_Day_Avg_Pre2000 = mean(Elevation, na.rm=T))

Res.Elv.Output <- Res.Elv %>%
  left_join(Elv.Day.Average.10yr, by = c("Month", "Day", "Reservoir")) %>%
  left_join(Elv.Day.Average.30yr, by = c("Month", "Day", "Reservoir")) %>%
  # left_join(Elv.Day.Average.Pre2000, by = c("Month", "Day", "Reservoir")) %>%
  # Rolling median window resolves the sudden drop in values on December 31
  arrange(Reservoir, Month, Day) %>%
  group_by(Reservoir) %>%
  mutate(Elv_Day_Avg_10yr = zoo::rollmedian(Elv_Day_Avg_10yr, k = 5, fill = NA)) %>%
  mutate(Elv_Day_Avg_30yr = zoo::rollmedian(Elv_Day_Avg_30yr, k = 5, fill = NA)) %>%
  select(-Year, -Month, -Day)

write.csv(Res.Elv.Output, "Pages/Reservoirs/Data/Reservoir_Elevation.csv")



## Storage Data
Res.Stor <- Storage %>%
  rename(Storage = Result) %>%
  mutate(
    Storage_MAF = Storage / 1000000
  ) %>%
  select(Date, Storage, Storage_MAF, Reservoir)

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
    Reservoir == "McPhee Reservoir" ~ 381100,
    Reservoir == "Fontenelle Reservoir" ~ 345360,
    Reservoir == "Starvation Reservoir" ~ 167300,
    Reservoir == "Green Mountain Reservoir" ~ 153639,
    Reservoir == "Vallecito Reservoir" ~ 129700,
    Reservoir == "Lake Nighthorse" ~ 120000,
    Reservoir == "Morrow Point Reservoir" ~ 117190,
    Reservoir == "Taylor Park Reservoir" ~ 106200,
    Reservoir == "Ruedi Reservoir" ~ 102000,
    Reservoir == "Total" ~ 61953887
)) %>%
  mutate(Max_Capacity_MAF = Max_Capacity / 1000000)


# Average Storage of each day of the year (e.g. August 1 across all years)
Stor.Day.Average.10yr <- Res.Stor.Bind %>%
  # Filters data to the past ten years starting from the previous year (keeping data to only completed data)
  filter(Year <= (max(Year)-1) & Year >= (max(Year)-11)) %>%
  group_by(Month, Day, Reservoir) %>%
  summarise(Stor_Day_Avg_10yr = mean(Storage_MAF, na.rm = TRUE), .groups = "drop")

Stor.Day.Average.30yr <- Res.Stor.Bind %>%
  # Filters data to the past ten years starting from the previous year (keeping data to only completed data)
  filter(Year <= (max(Year)-1) & Year >= (max(Year)-31)) %>%
  group_by(Month, Day, Reservoir) %>%
  summarise(Stor_Day_Avg_30yr = mean(Storage_MAF, na.rm = TRUE), .groups = "drop")

# Removing for now since it is not being deployed on the site currently
# Stor.Day.Average.Pre2000 <- Res.Stor %>%
#   filter(Year > 2000) %>%
#   group_by(Month, Day, Reservoir) %>%
#   summarise(Stor_Day_Avg_Pre2000 = mean(Storage, na.rm=T))

Res.Stor.Output <- Res.Stor.Bind %>%
  left_join(Stor.Day.Average.10yr, by = c("Month", "Day", "Reservoir")) %>%
  left_join(Stor.Day.Average.30yr, by = c("Month", "Day", "Reservoir")) %>%
  #left_join(Stor.Day.Average.Pre2000, by = c("Month", "Day", "Reservoir")) %>%
  #
  # Rolling median window resolves the sudden drop in values on December 31
  #
  # !! This is working for the Elevation data but not for storage data, working on a solution !!
  #
  arrange(Reservoir, Month, Day) %>%
  group_by(Reservoir) %>%
  mutate(Stor_Day_Avg_10yr = zoo::rollmedian(Stor_Day_Avg_10yr, k = 5, fill = NA)) %>%
  mutate(Stor_Day_Avg_30yr = zoo::rollmedian(Stor_Day_Avg_30yr, k = 5, fill = NA)) %>%
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
  arrange(Date) %>%
  slice_tail(n=1)

latest_elevation <- Res.Elv.Output %>%
  group_by(Reservoir) %>%
  arrange(Date) %>%
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
    Reservoir == "Granby Reservoir" ~ 40.14,
    Reservoir == "McPhee Reservoir" ~ 37.577,
    Reservoir == "Fontenelle Reservoir" ~ 42.0989,
    Reservoir == "Starvation Reservoir" ~ 40.1897,
    Reservoir == "Green Mountain Reservoir" ~ 39.8789,
    Reservoir == "Vallecito Reservoir" ~ 37.3922,
    Reservoir == "Lake Nighthorse" ~ 37.2182,
    Reservoir == "Morrow Point Reservoir" ~ 38.4516,
    Reservoir == "Taylor Park Reservoir" ~ 38.8388,
    Reservoir == "Ruedi Reservoir" ~ 39.363
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
    Reservoir == "Granby Reservoir" ~ -105.87,
    Reservoir == "McPhee Reservoir" ~ -108.571,
    Reservoir == "Fontenelle Reservoir" ~ -110.1343,
    Reservoir == "Starvation Reservoir" ~ -110.4528,
    Reservoir == "Green Mountain Reservoir" ~ -106.3133,
    Reservoir == "Vallecito Reservoir" ~ -107.5702,
    Reservoir == "Lake Nighthorse" ~ -107.8976,
    Reservoir == "Morrow Point Reservoir" ~ -107.5372,
    Reservoir == "Taylor Park Reservoir" ~ -106.578,
    Reservoir == "Ruedi Reservoir" ~ -106.7847
  )) %>%
  filter(Reservoir != "Total") %>%
  mutate(across(where(is.numeric), round, 2))

latest_res_sf <- st_as_sf(latest_res_data, coords = c("long", "lat"), crs=4326)

st_write(latest_res_sf, "GIS_Data/Reservoirs.geojson", append=FALSE, delete_dsn = TRUE)

print("Reservoir Data Preparation Complete")
