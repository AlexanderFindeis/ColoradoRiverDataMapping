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

# # Lake Mead Elevation
# Mead.Elv1 <- DataFetch(6123, "20200101", "") # Leaving dateTo blank will pull to latest date
# Mead.Elv2 <- DataFetch(6123, "20000101", "20191231")
# Mead.Elv3 <- DataFetch(6123, "19800101", "19991231")
# Mead.Elv4 <- DataFetch(6123, "19600101", "19791231")
# Mead.Elv5 <- DataFetch(6123, "19400101", "19591231")
# Mead.Elv6 <- DataFetch(6123, "", "19391231")
# Mead.Elv <- bind_rows(Mead.Elv1, Mead.Elv2, Mead.Elv3, Mead.Elv4, Mead.Elv5, Mead.Elv6) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Lake Mead") %>% distinct()
# rm(Mead.Elv1, Mead.Elv2, Mead.Elv3, Mead.Elv4, Mead.Elv5, Mead.Elv6)
# Elv.DataFrames <- list(Mead.Elv)

# # Lake Mead Storage
# Mead.Stor1 <- DataFetch(6124, "20200101", "") # Leaving before blank will pull to latest date
# Mead.Stor2 <- DataFetch(6124, "20000101", "20191231")
# Mead.Stor3 <- DataFetch(6124, "19800101", "19991231")
# Mead.Stor4 <- DataFetch(6124, "19600101", "19791231")
# Mead.Stor5 <- DataFetch(6124, "19400101", "19591231")
# Mead.Stor6 <- DataFetch(6124, "", "19391231")
# Mead.Stor <- bind_rows(Mead.Stor1, Mead.Stor2, Mead.Stor3, Mead.Stor4, Mead.Stor5, Mead.Stor6) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Lake Mead") %>% distinct()
# rm(Mead.Stor1, Mead.Stor2, Mead.Stor3, Mead.Stor4, Mead.Stor5, Mead.Stor6)
# Stor.DataFrames <- list(Mead.Stor)

# print("Lake Mead Data Fetched")

# ## Lake Powell
# # evapotation = 510, inflow cfs = 511, infow af = 4288, outflow cfs = 4315, outflow af = 4354, area acres = 4784
# # Lake Powell Elevation
# Powell.Elv1 <- DataFetch(508, "20200101", "") # Leaving before blank will pull to latest date
# Powell.Elv2 <- DataFetch(508, "20000101", "20191231")
# Powell.Elv3 <- DataFetch(508, "19800101", "19991231")
# Powell.Elv4 <- DataFetch(508, "19600101", "19791231")
# Powell.Elv <- bind_rows(Powell.Elv1, Powell.Elv2, Powell.Elv3, Powell.Elv4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Lake Powell") %>% distinct()
# rm(Powell.Elv1, Powell.Elv2, Powell.Elv3, Powell.Elv4)
# Elv.DataFrames <- append(Elv.DataFrames, list(Powell.Elv))
# # Lake Powell Storage
# Powell.Stor1 <- DataFetch(509, "20200101", "") # Leaving before blank will pull to latest date
# Powell.Stor2 <- DataFetch(509, "20000101", "20191231")
# Powell.Stor3 <- DataFetch(509, "19800101", "19991231")
# Powell.Stor4 <- DataFetch(509, "19600101", "19791231")
# Powell.Stor <- bind_rows(Powell.Stor1, Powell.Stor2, Powell.Stor3, Powell.Stor4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Lake Powell") %>% distinct()
# rm(Powell.Stor1, Powell.Stor2, Powell.Stor3, Powell.Stor4)
# Stor.DataFrames <- append(Stor.DataFrames, list(Powell.Stor))

# print("Lake Powell Data Fetched")

# ## Flaming Gorge - https://data.usbr.gov/catalog/2300
# # Elevation
# # Lake Flaming.Gorge Elevation
# Flaming.Gorge.Elv1 <- DataFetch(341, "20200101", "")
# Flaming.Gorge.Elv2 <- DataFetch(341, "20000101", "20191231")
# Flaming.Gorge.Elv3 <- DataFetch(341, "19800101", "19991231")
# Flaming.Gorge.Elv4 <- DataFetch(341, "19600101", "19791231")
# Flaming.Gorge.Elv <- bind_rows(Flaming.Gorge.Elv1, Flaming.Gorge.Elv2, Flaming.Gorge.Elv3, Flaming.Gorge.Elv4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Flaming Gorge")
# rm(Flaming.Gorge.Elv1, Flaming.Gorge.Elv2, Flaming.Gorge.Elv3, Flaming.Gorge.Elv4)
# Elv.DataFrames <- append(Elv.DataFrames, list(Flaming.Gorge.Elv))
# # Storage
# # Lake Flaming.Gorge Elevation
# Flaming.Gorge.Stor1 <- DataFetch(337, "20200101", "")
# Flaming.Gorge.Stor2 <- DataFetch(337, "20000101", "20191231")
# Flaming.Gorge.Stor3 <- DataFetch(337, "19800101", "19991231")
# Flaming.Gorge.Stor4 <- DataFetch(337, "19600101", "19791231")
# Flaming.Gorge.Stor <- bind_rows(Flaming.Gorge.Stor1, Flaming.Gorge.Stor2, Flaming.Gorge.Stor3, Flaming.Gorge.Stor4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Flaming Gorge")
# rm(Flaming.Gorge.Stor1, Flaming.Gorge.Stor2, Flaming.Gorge.Stor3, Flaming.Gorge.Stor4)
# Stor.DataFrames <- append(Stor.DataFrames, list(Flaming.Gorge.Stor))

# print("Flaming Gorge Data Fetched")


# ## Lake Mohave https://data.usbr.gov/catalog/4369
# # Elevation
# # Lake Mohave Elevation
# Mohave.Elv1 <- DataFetch(6133, "20200101", "")
# Mohave.Elv2 <- DataFetch(6133, "20000101", "20191231")
# Mohave.Elv3 <- DataFetch(6133, "19800101", "19991231")
# Mohave.Elv4 <- DataFetch(6133, "19600101", "19791231")
# Mohave.Elv <- bind_rows(Mohave.Elv1, Mohave.Elv2, Mohave.Elv3, Mohave.Elv4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Lake Mohave")
# rm(Mohave.Elv1, Mohave.Elv2, Mohave.Elv3, Mohave.Elv4)
# Elv.DataFrames <- append(Elv.DataFrames, list(Mohave.Elv))
# # Storage
# # Lake Mohave Elevation
# Mohave.Stor1 <- DataFetch(6134, "20200101", "")
# Mohave.Stor2 <- DataFetch(6134, "20000101", "20191231")
# Mohave.Stor3 <- DataFetch(6134, "19800101", "19991231")
# Mohave.Stor4 <- DataFetch(6134, "19600101", "19791231")
# Mohave.Stor <- bind_rows(Mohave.Stor1, Mohave.Stor2, Mohave.Stor3, Mohave.Stor4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Lake Mohave")
# rm(Mohave.Stor1, Mohave.Stor2, Mohave.Stor3, Mohave.Stor4)
# Stor.DataFrames <- append(Stor.DataFrames, list(Mohave.Stor))

# print("Lake Mohave Data Fetched")

# ## Navajo Reservoir https://data.usbr.gov/catalog/2392
# # Elevation
# Navajo.Elv1 <- DataFetch(612, "20200101", "")
# Navajo.Elv2 <- DataFetch(612, "20000101", "20191231")
# Navajo.Elv3 <- DataFetch(612, "19800101", "19991231")
# Navajo.Elv4 <- DataFetch(612, "19600101", "19791231")
# Navajo.Elv <- bind_rows(Navajo.Elv1, Navajo.Elv2, Navajo.Elv3, Navajo.Elv4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Navajo Reservoir")
# rm(Navajo.Elv1, Navajo.Elv2, Navajo.Elv3, Navajo.Elv4)
# Elv.DataFrames <- append(Elv.DataFrames, list(Navajo.Elv))
# # Storage
# Navajo.Stor1 <- DataFetch(613, "20200101", "")
# Navajo.Stor2 <- DataFetch(613, "20000101", "20191231")
# Navajo.Stor3 <- DataFetch(613, "19800101", "19991231")
# Navajo.Stor4 <- DataFetch(613, "19600101", "19791231")
# Navajo.Stor <- bind_rows(Navajo.Stor1, Navajo.Stor2, Navajo.Stor3, Navajo.Stor4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Navajo Reservoir")
# rm(Navajo.Stor1, Navajo.Stor2, Navajo.Stor3, Navajo.Stor4)
# Stor.DataFrames <- append(Stor.DataFrames, list(Navajo.Stor))

# print("Navajo Reservoir Data Fetched")


# ## Strawberry Reservoir https://data.usbr.gov/catalog/2456
# # Elevation
# Strawberry.Elv1 <- DataFetch(782, "20200101", "")
# Strawberry.Elv2 <- DataFetch(782, "20000101", "20191231")
# Strawberry.Elv3 <- DataFetch(782, "19800101", "19991231")
# Strawberry.Elv4 <- DataFetch(782, "19600101", "19791231")
# Strawberry.Elv <- bind_rows(Strawberry.Elv1, Strawberry.Elv2, Strawberry.Elv3, Strawberry.Elv4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Strawberry Reservoir")
# rm(Strawberry.Elv1, Strawberry.Elv2, Strawberry.Elv3, Strawberry.Elv4)
# Elv.DataFrames <- append(Elv.DataFrames, list(Strawberry.Elv))
# # Storage
# Strawberry.Stor1 <- DataFetch(779, "20200101", "")
# Strawberry.Stor2 <- DataFetch(779, "20000101", "20191231")
# Strawberry.Stor3 <- DataFetch(779, "19800101", "19991231")
# Strawberry.Stor4 <- DataFetch(779, "19600101", "19791231")
# Strawberry.Stor <- bind_rows(Strawberry.Stor1, Strawberry.Stor2, Strawberry.Stor3, Strawberry.Stor4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Strawberry Reservoir")
# rm(Strawberry.Stor1, Strawberry.Stor2, Strawberry.Stor3, Strawberry.Stor4)
# Stor.DataFrames <- append(Stor.DataFrames, list(Strawberry.Stor))

# print("Strawberry Reservoir Data Fetched")


# ## Blue Mesa https://data.usbr.gov/catalog/2249
# # Elevation
# Blue.Mesa.Elv1 <- DataFetch(78, "20200101", "")
# Blue.Mesa.Elv2 <- DataFetch(78, "20000101", "20191231")
# Blue.Mesa.Elv3 <- DataFetch(78, "19800101", "19991231")
# Blue.Mesa.Elv4 <- DataFetch(78, "19600101", "19791231")
# Blue.Mesa.Elv <- bind_rows(Blue.Mesa.Elv1, Blue.Mesa.Elv2, Blue.Mesa.Elv3, Blue.Mesa.Elv4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Blue Mesa Reservoir")
# rm(Blue.Mesa.Elv1, Blue.Mesa.Elv2, Blue.Mesa.Elv3, Blue.Mesa.Elv4)
# Elv.DataFrames <- append(Elv.DataFrames, list(Blue.Mesa.Elv))
# # Storage
# Blue.Mesa.Stor1 <- DataFetch(76, "20200101", "")
# Blue.Mesa.Stor2 <- DataFetch(76, "20000101", "20191231")
# Blue.Mesa.Stor3 <- DataFetch(76, "19800101", "19991231")
# Blue.Mesa.Stor4 <- DataFetch(76, "19600101", "19791231")
# Blue.Mesa.Stor <- bind_rows(Blue.Mesa.Stor1, Blue.Mesa.Stor2, Blue.Mesa.Stor3, Blue.Mesa.Stor4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Blue Mesa Reservoir")
# rm(Blue.Mesa.Stor1, Blue.Mesa.Stor2, Blue.Mesa.Stor3, Blue.Mesa.Stor4)
# Stor.DataFrames <- append(Stor.DataFrames, list(Blue.Mesa.Stor))

# print("Blue Mesa Data Fetched")


# ## Havasu https://data.usbr.gov/catalog/4371
# # Elevation
# Havasu.Elv1 <- DataFetch(6128, "20200101", "")
# Havasu.Elv2 <- DataFetch(6128, "20000101", "20191231")
# Havasu.Elv3 <- DataFetch(6128, "19800101", "19991231")
# Havasu.Elv4 <- DataFetch(6128, "19600101", "19791231")
# Havasu.Elv <- bind_rows(Havasu.Elv1, Havasu.Elv2, Havasu.Elv3, Havasu.Elv4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Lake Havasu")
# rm(Havasu.Elv1, Havasu.Elv2, Havasu.Elv3, Havasu.Elv4)
# Elv.DataFrames <- append(Elv.DataFrames, list(Havasu.Elv))
# # Storage
# Havasu.Stor1 <- DataFetch(6129, "20200101", "")
# Havasu.Stor2 <- DataFetch(6129, "20000101", "20191231")
# Havasu.Stor3 <- DataFetch(6129, "19800101", "19991231")
# Havasu.Stor4 <- DataFetch(6129, "19600101", "19791231")
# Havasu.Stor <- bind_rows(Havasu.Stor1, Havasu.Stor2, Havasu.Stor3, Havasu.Stor4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Lake Havasu")
# rm(Havasu.Stor1, Havasu.Stor2, Havasu.Stor3, Havasu.Stor4)
# Stor.DataFrames <- append(Stor.DataFrames, list(Havasu.Stor))

# print("Lake Havasu Data Fetched")


# ## Granby Reservoir https://data.usbr.gov/catalog/2321
# # Elevation
# Granby.Elv1 <- DataFetch(384, "20200101", "")
# Granby.Elv2 <- DataFetch(384, "20000101", "20191231")
# Granby.Elv3 <- DataFetch(384, "19800101", "19991231")
# Granby.Elv4 <- DataFetch(384, "19600101", "19791231")
# Granby.Elv <- bind_rows(Granby.Elv1, Granby.Elv2, Granby.Elv3, Granby.Elv4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Granby Reservoir")
# rm(Granby.Elv1, Granby.Elv2, Granby.Elv3, Granby.Elv4)
# Elv.DataFrames <- append(Elv.DataFrames, list(Granby.Elv))
# # Storage
# Granby.Stor1 <- DataFetch(383, "20200101", "")
# Granby.Stor2 <- DataFetch(383, "20000101", "20191231")
# Granby.Stor3 <- DataFetch(383, "19800101", "19991231")
# Granby.Stor4 <- DataFetch(383, "19600101", "19791231")
# Granby.Stor <- bind_rows(Granby.Stor1, Granby.Stor2, Granby.Stor3, Granby.Stor4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "Granby Reservoir")
# rm(Granby.Stor1, Granby.Stor2, Granby.Stor3, Granby.Stor4)
# Stor.DataFrames <- append(Stor.DataFrames, list(Granby.Stor))

# print("Granby Reservoir Data Fetched")

# ## McPhee Reservoir https://data.usbr.gov/catalog/2379
# # Elevation
# McPhee.Elv1 <- DataFetch(572, "20200101", "")
# McPhee.Elv2 <- DataFetch(572, "20000101", "20191231")
# McPhee.Elv3 <- DataFetch(572, "19800101", "19991231")
# McPhee.Elv4 <- DataFetch(572, "19600101", "19791231")
# McPhee.Elv <- bind_rows(McPhee.Elv1, McPhee.Elv2, McPhee.Elv3, McPhee.Elv4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "McPhee Reservoir")
# rm(McPhee.Elv1, McPhee.Elv2, McPhee.Elv3, McPhee.Elv4)
# Elv.DataFrames <- append(Elv.DataFrames, list(McPhee.Elv))
# # Storage
# McPhee.Stor1 <- DataFetch(569, "20200101", "")
# McPhee.Stor2 <- DataFetch(569, "20000101", "20191231")
# McPhee.Stor3 <- DataFetch(569, "19800101", "19991231")
# McPhee.Stor4 <- DataFetch(569, "19600101", "19791231")
# McPhee.Stor <- bind_rows(McPhee.Stor1, McPhee.Stor2, McPhee.Stor3, McPhee.Stor4) %>%
#   mutate(Date = as.Date(Date), Reservoir = "McPhee Reservoir")
# rm(McPhee.Stor1, McPhee.Stor2, McPhee.Stor3, McPhee.Stor4)
# Stor.DataFrames <- append(Stor.DataFrames, list(McPhee.Stor))



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

print("Reservoir Data Preparation Compelte")
