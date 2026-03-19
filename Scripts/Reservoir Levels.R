# Lake Mead Elevation
Mead.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/921/csv/49.csv")
# Lake Mead Storage
Mead.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/921/csv/17.csv")
# Lake Mead Releases
Mead.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/921/csv/43.csv")

# Lake Powell Elevation
Powell.Elv <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/49.csv")
# Lake Powell Storage
Powell.Stor <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/17.csv")
# Lake Powell Releases
Powell.Rel <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/43.csv")
# Lake Powell Inflows
Powell.In <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/30.csv")


### Chart Lake Powell Releases vs Lake Mead Releases
Mead.Flow <- Mead.Rel %>%
  select(datetime, Flow = release.volume) %>%
  mutate(Year = substr(datetime,0,4)) %>% # Extract Year from Date
  group_by(Year) %>%
  summarize(Flow = sum(Flow, na.rm=T)) %>%
  mutate(Reservoir = "Lake Mead")

Powell.Flow <- Powell.Rel %>%
  select(datetime, Flow = release.volume) %>%
  mutate(Year = substr(datetime,0,4)) %>% # Extract Year from Date
  group_by(Year) %>%
  summarize(Flow = sum(Flow, na.rm=T)) %>%
  mutate(Reservoir = "Lake Powell")

Flow.Data <- rbind(Mead.Flow, Powell.Flow) %>%
  mutate(Flow.MAF = Flow / 1000000) %>%
  filter(Year < 2026) 

# Flow.Plot <- ggplot(Flow.Data, aes(x = Year, y = Flow.MAF, group = Reservoir, color = Reservoir)) +
#   geom_line() +
#   labs(title = "Lake Powell vs Lake Mead Releases",
#        x = "Date",
#        y = "Flow (MAF)") +
#   theme_minimal()

# Flow.Plot


# Elevation Plot
Mead.Pow.Elv <- dplyr::bind_rows(
  Mead.Elv %>% mutate(Reservoir = "Lake Mead"),
  Powell.Elv %>% mutate(Reservoir = "Lake Powell")
) %>%
  select(datetime, Elevation = pool.elevation, Reservoir) %>%
  mutate(
    Year = substr(datetime, 1, 4),
    Elevation = as.numeric(Elevation)
  ) %>%
  group_by(Year, Reservoir) %>%
  summarize(Elevation = mean(Elevation, na.rm = TRUE), .groups = "drop") %>%
  mutate(Year = as.integer(Year))
write.csv(Mead.Pow.Elv, "Colorado_Data_Website/Pages/Reservoirs/Data/Reservoir_Elevation.csv")

# Elv.Plot <- ggplot(Mead.Pow.Elv, aes(x = Year, y = Elevation, group = Reservoir, color = Reservoir)) +
#   geom_line() +
#   facet_wrap(~Reservoir) +
#   labs(title = "Lake Powell vs Lake Mead Elevation",
#        x = "Date",
#        y = "Elevation (ft)") +
#   theme_minimal()
# Elv.Plot

Powell.Elv.Plot <- ggplot(data = subset(Mead.Pow.Elv, Reservoir == "Lake Powell"), aes(x = Year, y = Elevation, group = Reservoir)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 3490, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 3370, linetype = "solid", color = "black") +
  labs(title = "Lake Powell Elevation",
       x = "Date",
       y = "Elevation (ft)") +
  theme_minimal(plot.title = element_text(hjust = 0.5))
Powell.Elv.Plot

# Mead.Elv.Plot <- ggplot(data = subset(Mead.Pow.Elv, Reservoir == "Lake Mead"), aes(x = Year, y = Elevation, group = Reservoir)) +
#   geom_line(color = "blue") +
#   geom_hline(yintercept = 1075, linetype = "dashed", color = "black") +
#   geom_hline(yintercept = 1050, linetype = "dashed", color = "black") +
#   geom_hline(yintercept = 1025, linetype = "dashed", color = "black") +
#   geom_hline(yintercept = 950, linetype = "solid", color = "black") +
#   geom_hline(yintercept = 895, linetype = "solid", color = "black") +
#   labs(title = "Lake Mead Elevation",
#        x = "Date",
#        y = "Elevation (ft)") +
#   theme_minimal(plot.title = element_text(hjust = 0.5))
# Mead.Elv.Plot


# Inflow vs Outflow - Need to collect additional data for Lake Mead inflows but can largely assume releases from Powell equals inflow to Mead
Mead.Powell.Balance <- dplyr::bind_rows(
  Powell.In %>% select(datetime, Flow = inflow.volume) %>% mutate(Reservoir = "Lake Powell", Type = "Inflow"),
  Powell.Rel %>% select(datetime, Flow = release.volume) %>% mutate(Reservoir = "Lake Powell", Type = "Outflow"),
  Mead.Rel %>% select(datetime, Flow = release.volume) %>% mutate(Reservoir = "Lake Mead", Type = "Outflow")
) %>%
  mutate(Year = as.integer(substr(datetime, 1, 4))) %>%
  group_by(Year, Reservoir, Type) %>%
  summarize(Flow = sum(Flow, na.rm = TRUE), .groups = "drop") %>%
  mutate(Flow.MAF = Flow / 1000000)

# Powell.Balance.Plot <- Mead.Powell.Balance %>%
#   filter(Reservoir == "Lake Powell", Year < 2026) %>%
#   ggplot(aes(x = Year, y = Flow.MAF, group = Type, color = Type)) +
#   geom_line() +
#   labs(title = "Lake Powell Inflow vs Outflow",
#        x = "Date",
#        y = "Flow (MAF)") +
#   theme_minimal()
# ggplotly(Powell.Balance.Plot)


## Lake Mead Outflow vs Lower Basin Consumption from Mainstem (not counting tributary consumption) 
## *Need to account for tributary inflows after Lake Mead or do just mainstream consumption*
# Mexico deliveries
mex.del <- read.xlsx("https://www.usbr.gov/lc/region/g4000/4200Rpts/CULReport/1971-2024%20Lower%20Colorado%20River%20System%20CUL%20Data.xlsx", sheet = "Mexico") %>%
  group_by(YEAR) %>%
  summarize(Consumption = sum(Total, na.rm = TRUE)) %>%
  mutate(Region = "Mexico")
# Lower Basin Use
lower.basin <- read.xlsx("https://www.usbr.gov/lc/region/g4000/4200Rpts/CULReport/1971-2024%20Lower%20Colorado%20River%20System%20CUL%20Data.xlsx", sheet = "Total_Lower_System") %>%
  filter(SOURCE == "MAINSTREAM_SOURCE") %>%
  group_by(YEAR) %>%
  summarize(Consumption = sum(TOTAL, na.rm = TRUE)) %>%
  mutate(Region = "Lower Basin")

Mex.LB.Cons <- rbind(mex.del, lower.basin) %>%
  group_by(YEAR) %>%
  summarize(Volume = sum(Consumption, na.rm = TRUE)) %>%
  mutate(Type = "Lower Basin Consumptive Use")

Mead.Out <- Mead.Rel %>%
  mutate(YEAR = as.integer(substr(datetime,1,4))) %>%
  group_by(YEAR) %>%
  summarize(Volume = sum(release.volume, na.rm = TRUE)) %>%
  filter(YEAR > 1970, YEAR < 2026) %>%
  mutate(Type = "Lake Mead Outflow")

Mead.OutCons <- rbind(Mex.LB.Cons, Mead.Out) %>%
  mutate(Volume.MAF = Volume / 1000000)

# Mead.OutCons.Plot <- ggplot(Mead.OutCons, aes(x = YEAR, y = Volume.MAF, group = Type, color = Type)) +
#   geom_line() 
# Mead.OutCons.Plot
