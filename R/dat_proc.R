library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(here)

# import and clean Alafia epc data ----------------------------------------

# original data from D. Karlen via email 12/16/21

marine <- c(74, 178, 153, 179)

aladat <- read_excel(here('data/raw/EPC WQ Data Alafia River 1972 - October 2021.xlsx')) %>% 
  select(
    station = StationNumber, 
    date = SampleTime, 
    entero = Enterococci,
    tn = Total_Nitrogen,
    nh4 = Ammonia, 
    no23 = Nitrates_Nitrites,
    chla = Chlorophyll_a,
    lat = Latitude, 
    lng = Longitude, 
    class = Class
  ) %>% 
  filter(station %in% marine) %>% 
  mutate(station = factor(station, levels = marine))

alaloc <- aladat %>% 
  select(station, lat, lng) %>% 
  unique %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326)

aladat <- aladat %>% 
  select(-lat, -lng, -class) %>% 
  mutate(
    date = as.Date(date)
  ) %>% 
  pivot_longer(col = -matches('station|date')) %>% 
  mutate(
    yr = year(date), 
    mo = month(date),
    value = gsub('>|<', '', value), 
    value = as.numeric(value), 
    lab = case_when(
      name == 'entero' ~ 'Enterococc (#/100mL)',
      name == 'tn' ~ 'Total Nitrogen (mg/L)', 
      name == 'nh4' ~ 'Amonia (mg/L)', 
      name == 'chla' ~ 'Chl-a (ug/L)',
      name == 'no23' ~ 'Nitrate-Nitrite (mg/L)'
    )
  ) %>% 
  group_by(station, yr, mo, name, lab) %>% 
  summarise(value = mean(value, na.rm = T), .groups = 'drop') %>% 
  na.omit

save(aladat, file = here('data/aladat.RData'))
save(alaloc, file = here('data/alaloc.RData'))
  