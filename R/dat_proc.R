library(tidyverse)
library(lubridate)
library(readxl)
library(here)

# import and clean Alafia epc data ----------------------------------------

# original data from D. Karlen via email 12/16/21

alaraw <- read_excel(here('data/raw/EPC WQ Data Alafia River 1972 - October 2021.xlsx'))
