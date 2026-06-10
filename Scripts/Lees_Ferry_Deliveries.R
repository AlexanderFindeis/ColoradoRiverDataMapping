library(dataRetrieval)
library(dplyr)
library(lubridate)

# Use Mexico deliveries data from Consumptive Use script. Joining water year to calander year so may introduce error
# Commented out joning Mexico deliveries and instead smoothed out data to be obligations to LB and half of Mexico treaty (82.5maf over 10 years)
# Did this to conform with figures coming from other sources and match methodology

Lees_Ferry <- readNWISdv(siteNumbers = "09380000", # New River
                      parameterCd = "00060", # Average discharge cubic feet per second
                      startDate = "1900-10-01",
                      endDate = "2025-12-31") %>%
  rename("Discharge" = 4) %>%
  mutate(TotalDailyDischarge = Discharge * 60 * 60 * 24) %>% # Cubic feet per second -> Cubic Feet Per Day
  mutate(Year = substr(Date,0,4)) %>% # Extract Year from Date
  mutate(Month = substr(Date,6,7)) %>% # Extract Month from Date
  mutate(Year = as.integer(Year)) %>% # Convert text year into integer year
  mutate(Month = as.integer(Month)) %>% # Convert text month into integer month
  #mutate(Water.Year = ifelse(Month >= 10, Year+1, Year)) %>% # Create Water Year column
  group_by(Year) %>%
  summarize(AnnualFlow = sum(TotalDailyDischarge, na.rm=T), # In cubic feet
            AverageCFS = mean(Discharge, na.rm=T)) %>% # In cubic feet per second 
  #left_join(Mex.Del.Annual, by = c("Year" = "YEAR")) %>% # Join Mexico deliveries data
  #mutate(Deliveries = if_else(Year == 2025, 1240000, Deliveries)) %>% # Using operating report fro BoR to fill in hole in data
  #mutate(AnnualFlowAF = AnnualFlow / 43560 - (Deliveries / 2)) %>% # Convert cubic feet to acre feet and subtract deliveries to Mexico. Dividing by two because deliveries to Mexico are national, so upper basin responsible for half
  mutate(AnnualFlowAF = (AnnualFlow / 43560)) %>% 
  mutate(AF_Millions = AnnualFlowAF / 1000000) %>%
  mutate(Ten.Year.Average = zoo::rollmean(AnnualFlowAF, k = 10, fill = NA, align = 'right')) %>%
  mutate(Ten.Year.Sum = zoo::rollsum(AnnualFlowAF, k = 10, fill = NA, align = 'right')) %>%
  mutate(across(where(is.numeric), round, 2))


## Data on releases from Glen Canyon dam as alternative to Lee Ferry
Glen_Canyon_Data <- read.csv("https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/43.csv")

Glen_Canyon_Out <- Glen_Canyon_Data %>%
  mutate(Year = substr(datetime,0,4)) %>% # Extract Year from Date
  mutate(Month = substr(datetime,6,7)) %>% # Extract Month from Date
  mutate(Year = as.integer(Year)) %>% # Convert text year into integer year
  mutate(Month = as.integer(Month)) %>% # Convert text month into integer month
  #mutate(Water.Year = ifelse(Month >= 10, Year+1, Year)) %>% # Create Water Year column
  filter(Year < 2026) %>% # Filter out partial water year 2026
  group_by(Year) %>%
  summarize(Releases = sum(release.volume, na.rm=T)) %>%
  mutate(LB.Releases = case_when(Year == 2025 ~ Releases - 1318393/2, # Subtracting deliveries to Mexico. No data for 2025 yet, assuming the same as 2024 - https://www.usbr.gov/lc/region/g4000/wtracct.html
                              Year == 2024 ~ Releases - 1318393/2, # Dividing by two because deliveries to Mexico are national, so upper basin responsible for half
                              Year == 2023 ~ Releases - 1405522/2,
                              Year == 2022 ~ Releases - 1449820/2,
                              Year == 2021 ~ Releases - 1455061/2,
                              TRUE ~ Releases - 1500000/2)) %>%
  mutate(LB.Releases.MillionAF = LB.Releases / 1000000) %>%
  mutate(LB.Ten.Year.Average = zoo::rollmean(LB.Releases, k = 10, fill = NA, align = 'right')) %>%
  mutate(LB.Ten.Year.Sum = zoo::rollsum(LB.Releases, k = 10, fill = NA, align = 'right')) %>%
  mutate(Ten.Year.Average = zoo::rollmean(Releases, k = 10, fill = NA, align = 'right')) %>%
  mutate(Ten.Year.Sum = zoo::rollsum(Releases, k = 10, fill = NA, align = 'right'))


# Graphing

library(ggplot2)

Glen_Canyon_Plot <- ggplot(Glen_Canyon_Out, aes(x = Year, y = LB.Releases.MillionAF)) +
  geom_line(color = "blue") +
  geom_line(aes(y = Ten.Year.Average / 1000000), color = "red", linetype = "dashed") +
  labs(title = "Glen Canyon Dam Annual Releases",
       x = "Water Year",
       y = "Releases (Million AF)") +
  geom_hline(yintercept=7.5, color = "black") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

Glen_Canyon_Plot


# Plot rolling 10yr sum
Moving_Sum_Plot <- Lees_Ferry %>% filter(Year >= 1980) %>%
  ggplot(aes(x = Year, y = Ten.Year.Sum / 1000000)) +
  geom_line(color = "blue") +
  labs(title = "Lees Ferry 10-Year Moving Sum of Annual Flow",
       x = "Year",
       y = "10 Year Moving Sum of Annual Flow (Million AF)") +
  geom_hline(yintercept=82.5, color = "black", linetype = "dashed") +
  #geom_rect(aes(xmin=1963,xmax=1980,ymin=60,ymax=Inf),color="grey",alpha=0.0025) +
  annotate("text", x = 2002.5, y = 80, label = "Obligations to Lower Basin and Mexico - 82.5maf/10yrs", color = "black", size = 4) +
  ylim(70, 135) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
Moving_Sum_Plot

