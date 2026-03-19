library(pacman)
pacman::p_load(tidyverse, openxlsx, plotly)

### Lower Basin Consumptive Use Report
# Area reference sheet with key for location/tributary names and codes
area.reference <- read.xlsx("https://www.usbr.gov/lc/region/g4000/4200Rpts/CULReport/1971-2024%20Lower%20Colorado%20River%20System%20CUL%20Data.xlsx", sheet = "Area_Reference")
# Reservoir evaporation rates
res.evap <- read.xlsx("https://www.usbr.gov/lc/region/g4000/4200Rpts/CULReport/1971-2024%20Lower%20Colorado%20River%20System%20CUL%20Data.xlsx", sheet = "Mainstream_Reservoirs")
# Mainstream consumptive use data
main.cu <- read.xlsx("https://www.usbr.gov/lc/region/g4000/4200Rpts/CULReport/1971-2024%20Lower%20Colorado%20River%20System%20CUL%20Data.xlsx", sheet = "Mainstream")
# Tributary consumptive use data
trib.cu <- read.xlsx("https://www.usbr.gov/lc/region/g4000/4200Rpts/CULReport/1971-2024%20Lower%20Colorado%20River%20System%20CUL%20Data.xlsx", sheet = "Tributary")
# Mexico deliveries
mex.del <- read.xlsx("https://www.usbr.gov/lc/region/g4000/4200Rpts/CULReport/1971-2024%20Lower%20Colorado%20River%20System%20CUL%20Data.xlsx", sheet = "Mexico")
# Tributary Irrigated Acreage
trib.irr.acre <- read.xlsx("https://www.usbr.gov/lc/region/g4000/4200Rpts/CULReport/1971-2024%20Lower%20Colorado%20River%20System%20CUL%20Data.xlsx", sheet = "Tributary_Irrigated_Acreage")
# Population data 
lb.pop <- read.xlsx("https://www.usbr.gov/lc/region/g4000/4200Rpts/CULReport/1971-2024%20Lower%20Colorado%20River%20System%20CUL%20Data.xlsx", sheet = "Population")


## Mainstream Consumptive Use Pivot Table
Main.Jan <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = January) %>% mutate(Month = "01")
Main.Feb <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = February) %>% mutate(Month = "02")
Main.Mar <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = March) %>% mutate(Month = "03")
Main.Apr <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = April) %>% mutate(Month = "04")
Main.May <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = May) %>% mutate(Month = "05")
Main.Jun <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = June) %>% mutate(Month = "06")
Main.Jul <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = July) %>% mutate(Month = "07")
Main.Aug <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = August) %>% mutate(Month = "08")
Main.Sep <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = September) %>% mutate(Month = "09")
Main.Oct <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = October) %>% mutate(Month = "10")
Main.Nov <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = November) %>% mutate(Month = "11")
Main.Dec <- main.cu %>% select(STATE_NAME, SOURCE, CALC_TYPE, YEAR, CONSUMPTIVE_USE = December) %>% mutate(Month = "12")

Main.CU.Pivot <- rbind(Main.Jan, Main.Feb, Main.Mar, Main.Apr, Main.May, Main.Jun, Main.Jul, Main.Aug, Main.Sep, Main.Oct, Main.Nov, Main.Dec) %>%
  mutate(Date = as.Date(paste(YEAR, Month, "01", sep = "-"))) %>% # Make date field for each month, using the first day of the month as a placeholder
  select(-Month) %>%
  group_by(STATE_NAME, SOURCE, CALC_TYPE, Date, YEAR) %>%
  summarise(CONSUMPTIVE_USE = sum(CONSUMPTIVE_USE, na.rm = TRUE)) %>%
  ungroup()
rm(Main.Jan, Main.Feb, Main.Mar, Main.Apr, Main.May, Main.Jun, Main.Jul, Main.Aug, Main.Sep, Main.Oct, Main.Nov, Main.Dec)

Main.CU.Annual.Total <- Main.CU.Pivot %>%
  group_by(STATE_NAME, YEAR) %>%
  summarise(CONSUMPTIVE_USE = sum(CONSUMPTIVE_USE, na.rm = TRUE)) %>%
  mutate(CU.Millions = CONSUMPTIVE_USE / 1e6) %>%
  ungroup()


## Tributary Consumptive Use Pivot Table
Trib.Jan <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "01")
Trib.Feb <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "02")
Trib.Mar <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "03")
Trib.Apr <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "04")
Trib.May <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "05")
Trib.Jun <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "06")
Trib.Jul <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "07")
Trib.Aug <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "08")
Trib.Sep <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "09")
Trib.Oct <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "10")
Trib.Nov <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "11")
Trib.Dec <- trib.cu %>% select(NODE_CODE, CALC_TYPE, Year, CONSUMPTIVE_USE = January) %>% mutate(Month = "12")

Trib.CU.Pivot <- rbind(Trib.Jan, Trib.Feb, Trib.Mar, Trib.Apr, Trib.May, Trib.Jun, Trib.Jul, Trib.Aug, Trib.Sep, Trib.Oct, Trib.Nov, Trib.Dec) %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>% # Make date field for each month, using the first day of the month as a placeholder
  select(-Month) %>%
  group_by(NODE_CODE, CALC_TYPE, Date, Year) %>%
  summarise(CONSUMPTIVE_USE = sum(CONSUMPTIVE_USE, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(area.reference, by = c("NODE_CODE" = "NODE_CODE")) %>%
  filter(DOWNSTREAM_NODE != 31) %>% # Filter out consumptive use for Gila River since it is not considered part of Arizona's apportionment from the Colorado River
  select(-STATE_CODE, -DOWNSTREAM_NODE, -HU8_CODE)
rm(Trib.Jan, Trib.Feb, Trib.Mar, Trib.Apr, Trib.May, Trib.Jun, Trib.Jul, Trib.Aug, Trib.Sep, Trib.Oct, Trib.Nov, Trib.Dec)

Trib.CU.Annual <- Trib.CU.Pivot %>%
  group_by(STATE_NAME, HU8_NAME, REPORTING_AREA, Year) %>%
  summarise(CONSUMPTIVE_USE = sum(CONSUMPTIVE_USE, na.rm = TRUE)) %>%
  mutate(CU.Millions = CONSUMPTIVE_USE / 1e6) %>%
  ungroup()

Trib.CU.Annual.Total <- Trib.CU.Pivot %>%
  group_by(STATE_NAME, Year) %>%
  summarise(CONSUMPTIVE_USE = sum(CONSUMPTIVE_USE, na.rm = TRUE)) %>%
  mutate(CU.Millions = CONSUMPTIVE_USE / 1e6) %>%
  ungroup()


## Mexico Deliveries
Mex.Total.Del.Jan <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = January) %>% mutate(Month = "01")
Mex.Total.Del.Feb <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = February) %>% mutate(Month = "02")
Mex.Total.Del.Mar <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = March) %>% mutate(Month = "03")
Mex.Total.Del.Apr <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = April) %>% mutate(Month = "04")
Mex.Total.Del.May <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = May) %>% mutate(Month = "05")
Mex.Total.Del.Jun <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = June) %>% mutate(Month = "06")
Mex.Total.Del.Jul <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = July) %>% mutate(Month = "07")
Mex.Total.Del.Aug <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = August) %>% mutate(Month = "08")
Mex.Total.Del.Sep <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = September) %>% mutate(Month = "09")
Mex.Total.Del.Oct <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = October) %>% mutate(Month = "10")
Mex.Total.Del.Nov <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = November) %>% mutate(Month = "11")
Mex.Total.Del.Dec <- mex.del %>% filter(DELIVERY_TYPE == "TOTAL") %>% select(YEAR, Deliveries = December) %>% mutate(Month = "12")

Mex.Total.Deliveries <- rbind(Mex.Total.Del.Jan, Mex.Total.Del.Feb, Mex.Total.Del.Mar, Mex.Total.Del.Apr, Mex.Total.Del.May, Mex.Total.Del.Jun, Mex.Total.Del.Jul, Mex.Total.Del.Aug, Mex.Total.Del.Sep, Mex.Total.Del.Oct, Mex.Total.Del.Nov, Mex.Total.Del.Dec) %>%
  mutate(Date = as.Date(paste(YEAR, Month, "01", sep = "-"))) %>% # Make date field for each month, using the first day of the month as a placeholder
  select(-Month) 
rm(Mex.Total.Del.Jan, Mex.Total.Del.Feb, Mex.Total.Del.Mar, Mex.Total.Del.Apr, Mex.Total.Del.May, Mex.Total.Del.Jun, Mex.Total.Del.Jul, Mex.Total.Del.Aug, Mex.Total.Del.Sep, Mex.Total.Del.Oct, Mex.Total.Del.Nov, Mex.Total.Del.Dec)

Mex.Del.Annual <- Mex.Total.Deliveries%>%
  group_by(YEAR) %>%
  summarise(Deliveries = sum(Deliveries, na.rm = TRUE)) %>%
  mutate(Del.Millions = Deliveries / 1e6) %>%
  ungroup()



# Graph mainstream consumptive use by source and state
Plot.Source.Use <- ggplot(Main.CU.Annual.Total, aes(x = YEAR, y = CU.Millions, color = STATE_NAME)) +
  geom_line() +
  #facet_wrap(~STATE_NAME) +
  geom_hline(yintercept = 4.4, linetype = "dashed", color = "dark green") +
  geom_hline(yintercept = 2.8, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.3, linetype = "dashed", color = "blue") +
  labs(title = "Mainstream Consumptive Use by State",
       x = "Date",
       y = "Volume (Million AF)",
       color = "State") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
Plot.Source.Use
ggplotly(Plot.Source.Use)

# Graph tributary consumptive use by HU8 and state
Plot.HU8.Use <- ggplot(Trib.CU.Annual, aes(x = Year, y = CU.Millions, color = STATE_NAME)) +
  geom_line() +
  facet_wrap(~REPORTING_AREA) +
  labs(title = "Tributary Consumptive Use by HU8 and State",
       x = "Date",
       y = "Consumptive Use (Million AF)",
       color = "State") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggplotly(Plot.HU8.Use)


# Total Lower Basin Consumptive Use (Mainstem + Tributary)
Total.LB.CU <- rbind(
  Main.CU.Annual.Total %>% group_by(YEAR) %>% summarise(CONSUMPTIVE_USE = sum(CONSUMPTIVE_USE, na.rm = TRUE)) %>% mutate(Type = "Mainstem"),
  Trib.CU.Annual.Total %>% group_by(Year) %>% summarise(CONSUMPTIVE_USE = sum(CONSUMPTIVE_USE, na.rm = TRUE)) %>% rename(YEAR = Year) %>% mutate(Type = "Tributary")
) %>%
  group_by(YEAR) %>%
  summarise(CONSUMPTIVE_USE = sum(CONSUMPTIVE_USE, na.rm = TRUE)) %>%
  mutate(CU.Millions = CONSUMPTIVE_USE / 1e6) %>%
  ungroup()


### Upper Basin Consumptive Use
UB.CU <- read.xlsx("D:/Water Data/v24.5_CUL_ResultsCU.xlsx", sheet = "monthlyData")

UB.StateCons <- UB.CU %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30")) %>%
  mutate(YEAR = as.integer(substr(Date, 1, 4))) %>%
  group_by(State, YEAR) %>%
  summarise(CONSUMPTIVE_USE = sum(Value, na.rm = TRUE)) %>%
  mutate(CU.Millions = CONSUMPTIVE_USE / 1e6) %>%
  ungroup()

Total.UB.CU <- UB.CU %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30")) %>%
  mutate(YEAR = as.integer(substr(Date, 1, 4))) %>%
  group_by(YEAR) %>%
  summarise(CONSUMPTIVE_USE = sum(Value, na.rm = TRUE)) %>%
  mutate(CU.Millions = CONSUMPTIVE_USE / 1e6) %>%
  ungroup()


## Total Basin Consumptive Use by region
Total.Basin.CU <- rbind(
  Total.UB.CU %>% select(YEAR, CU.Millions) %>% mutate(Region = "Upper Basin"),
  Total.LB.CU %>% select(YEAR, CU.Millions) %>% mutate(Region = "Lower Basin")
)

Total.CU.Plot <- ggplot(Total.Basin.CU, aes(x = YEAR, y = CU.Millions, color = Region)) +
  geom_line() +
  labs(title = "Total Basin Consumptive Use by Region",
       x = "Date",
       y = "Consumptive Use (Million AF)",
       color = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggplotly(Total.CU.Plot)


### Consumptive Use by State Including Reservoir Evaporation

# Lower Basin Reservoir Evaporation - Upper Basin included in by-state consumptive use records
LB.Res.Evap <- res.evap %>%
  group_by(YEAR) %>%
  summarise(Reservoir.Evaporation = sum(Total, na.rm = TRUE)) %>%
  mutate(LB.ResEvap.Mil = Reservoir.Evaporation / 1e6)

# Combined state data including evaporation
CU.PerState <- rbind(
  Main.CU.Annual.Total %>% select(STATE_NAME, YEAR, CU.Millions) %>% mutate(Region = "Lower Basin"),
  Trib.CU.Annual.Total %>% select(STATE_NAME, Year, CU.Millions) %>% rename(YEAR = Year) %>% mutate(Region = "Lower Basin"),
  UB.StateCons %>% select(State, YEAR, CU.Millions) %>% rename(STATE_NAME = State) %>% mutate(Region = "Upper Basin")
) %>%
  #mutate(Source = STATE_NAME) %>%
  mutate(State = case_when(
    STATE_NAME == "Blue Mesa" ~ "Colorado",
    STATE_NAME == "Flaming Gorge" ~ "Utah",
    STATE_NAME == "Lake Powell" ~ "Arizona",
    STATE_NAME == "Morrow Point" ~ "Colorado",
    TRUE ~ STATE_NAME
  )) %>%
  group_by(State,YEAR) %>%
  summarise(CU.Millions = sum(CU.Millions, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(LB.Res.Evap, by = c("YEAR")) %>%
  mutate(CU.Millions = case_when(
    State == "California" ~ CU.Millions + LB.ResEvap.Mil * 0.586, # Multiplying reservoir evaporation by the share of each state's allocation of the whole lower basin
    State == "Arizona" ~ CU.Millions + LB.ResEvap.Mil * 0.373, 
    state.abb[match(State, state.name)] %>% as.character() == "Nevada" ~ CU.Millions + LB.ResEvap.Mil * 0.04,
    TRUE ~ CU.Millions
  )) %>%
  mutate(Region = case_when(
    State %in% c("California", "Arizona", "Nevada") ~ "Lower Basin",
    TRUE ~ "Upper Basin"
  ))

# Plot consumptive use w/ evaporatoin by state
CU.PerState.Plot <- ggplot(CU.PerState, aes(x = YEAR, y = CU.Millions, group = State, color = State)) +
  geom_line() +
  #facet_wrap(~State) +
  labs(title = "Consumptive Use by State Including Reservoir Evaporation",
       x = "Date",
       y = "Consumptive Use (Million AF)",
       color = "State") +
  theme_minimal() +
  theme(legend.position = "bottom")
CU.PerState.Plot

# Plot consumptive use w/ evaporation by region
CU.Region.wEvap <- CU.PerState %>%
  group_by(Region, YEAR) %>%
  summarise(CU.Millions = sum(CU.Millions, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = YEAR, y = CU.Millions, color = Region)) +
  geom_line() +
  labs(title = "Consumptive Use by Region Including Reservoir Evaporation",
       x = "Date",
       y = "Consumptive Use (Million AF)",
       color = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom")
CU.Region.wEvap



### Reconstructing total basin supply by adding Upper Basin use to Lake Powell inflow
# Upper Basin CU with evap
UB.CUevap <- CU.PerState %>%
  filter(Region == "Upper Basin") %>%
  group_by(YEAR) %>%
  summarise(CU.Millions = sum(CU.Millions, na.rm = TRUE)) %>%
  ungroup()

Basin.Supply <- Mead.Powell.Balance %>% # DF from Reservoir Levels script
  filter(Reservoir == "Lake Powell", Type == "Inflow", Year < 2026) %>%
  left_join(Total.UB.CU, by = c("Year" = "YEAR")) %>%
  mutate(Supply = Flow.MAF + CU.Millions) %>%
  select(Year, Volume.Millions = Supply) %>%
  mutate('Ten.Year.Average' = zoo::rollmean(Volume.Millions, k = 10, fill = NA, align = 'right')) %>%
  mutate(Type = "Supply")


Annual.CU <- CU.PerState %>%
  group_by(YEAR) %>%
  summarise(CU.Millions = sum(CU.Millions, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(Mex.Del.Annual, by = "YEAR") %>%
  mutate(CU.Millions = CU.Millions + Del.Millions) %>%
  select(Year = YEAR, Volume.Millions = CU.Millions) %>%
  mutate('Ten.Year.Average' = zoo::rollmean(Volume.Millions, k = 10, fill = NA, align = 'right')) %>%
  mutate(Type = "Consumptive Use")

Supply.Use <- rbind(Basin.Supply, Annual.CU)

Supply.Use.Plot <- ggplot(Supply.Use, aes(x = Year, y = Volume.Millions, color = Type)) +
  geom_line() +
  #geom_smooth(method = "lm", se = FALSE) +
  #geom_line(stat="smooth", method = "lm", alpha = .75) +
  stat_smooth(geom = "line", method = "loess", span = 1, se = FALSE, alpha = 0.75, linetype = "dashed") +
  labs(title = "Total Basin Supply vs Total Consumptive Use", # Upper, Lower and Mexico use
       x = "Date",
       y = "Volume (Million AF)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(1970, 2025)
#ggplotly(Supply.Use.Plot)
Supply.Use.Plot

Supply.Use.Plot.10yr <- ggplot(Supply.Use, aes(x = Year, y = Ten.Year.Average, color = Type)) +
  geom_line() +
  labs(title = "Colorado River Supply vs Consumptive Use: 10-Year Moving Average", # Upper, Lower and Mexico use
       x = "Date",
       y = "Volume (Million AF)") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  xlim(1980, 2025)
Supply.Use.Plot.10yr
ggplotly(Supply.Use.Plot.10yr)
