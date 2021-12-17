library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(here)

# import and clean Alafia epc data ----------------------------------------

# original data from D. Karlen via email 12/16/21

alaraw <- read_excel(here('data/raw/EPC WQ Data Alafia River 1972 - October 2021.xlsx')) %>% 
  select(
    station = StationNumber, 
    date = SampleTime, 
    entero = Enterococci,
    ecoli = E_Coliform, 
    tn = Total_Nitrogen,
    chla = Chlorophyll_a,
    lat = Latitude, 
    lng = Longitude, 
    class = Class
  ) %>% 
  filter(class == '3M')

alaloc <- alaraw %>% 
  select(station, lat, lng) %>% 
  unique %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326)

aladat <- alaraw %>% 
  select(-lat, -lng, -class) %>% 
  mutate(
    date = as.Date(date)
  ) %>% 
  pivot_longer(col = -matches('station|date')) %>% 
  mutate(
    yr = year(date), 
    mo = month(date),
    value = gsub('>|<', '', value), 
    value = as.numeric(value)
  ) %>% 
  group_by(station, yr, mo, name) %>% 
  summarise(value = mean(value, na.rm = T), .groups = 'drop') %>% 
  na.omit %>% 
  pivot_wider()

save(aladat, file = here('data/aladat.RData'))
save(alaloc, file = here('data/alaloc.RData'))
  