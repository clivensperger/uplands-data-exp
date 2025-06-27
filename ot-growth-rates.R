###-------------------------------------------------####
#- Title: Overstory Tree Growth Rates
#- Author: Carolyn Livensperger
#- Date: June 26, 2025
#- 
#- Data Source: Plot overstory tree data extracted from Uplands database. Some has been edited in excel to exclude invalid entries into the census.
#-
#- This script is to get an estimate of growth rates for different tree species. See ot-growth-rates-blcu.R for pinyon and juniper tree growth rates at BLCA and CURE.
#-
###--------------------------------------------------####

#------- Packages -----------
library(tidyverse)
library(lubridate)
library(janitor)

#------- Load Data --------------------
trees <- read.csv('../uplands-dm/output/ot-raw-250506.csv')

#------- Clean up -------------
trees <- trees %>%
  mutate(Start_Date = ymd(Start_Date)) %>%
  mutate(Visit_Year = year(Start_Date)) %>%
  clean_names() 

trees_brca <- trees %>%
  filter(unit_code == 'BRCA') %>%
  filter(master_stratification != 'nontarget') %>%
  select(-c(primary_eco_site, vegetation_type, quad)) %>%
  mutate(master_stratification = ifelse(master_stratification == 'High-MixedConifer', 'MixedCon', 'PJ'))
