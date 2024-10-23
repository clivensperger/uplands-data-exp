###----------------------------###
#- Title: Dominant Grasses for Grassland/Shrublands
#- Author: Carolyn Livensperger
#- Date: October 17, 2024
#- 
#- Data Source: NCPN Uplands Vegetation and Soil Monitoring Program
#-
#- Purpose: This script is to determine dominant species for grasslands and shrublands in ARCH, CANY, CARE, BLCU, DINO. This information will be shared with NAU for multi-network grassland trends project.
#-
#- Notes: Exclude pilot years. 
#- 
#- Dominant species will be determined by average cover across all plots. Seems like most straightforward method?!
#-

###----------------------------###

#----------Packages----------
library(tidyverse)
library(lubridate)
library(janitor)

#--------- Load Data and Functions -------------
## Use long version of pi from the uplands-data-publication folder
pi_raw_long <- read.csv('../uplands-data-publication/long/uplands_pi_long_2024-07-24.csv')

# plants table
ncpn_plants <- read.csv('../uplands-dm/output/plants-tbl-231205.csv')

#--------- Initial Clean Up -----------
## filter to parks with grasslands/shrublands
park_codes <- c('ARCH', 'CANY', 'CARE', 'BLCA', 'CURE', 'DINO')

pi_raw_long <- pi_raw_long %>%
  clean_names() %>% #convert column names to snakecase
  filter(unit_code %in% park_codes) 

## filter to select only grassland/shrublands within the parks
# can use vegetation_type == grassland/shrubland but also need to filter 'nontarget' plots
pi_raw_long <- pi_raw_long %>%
  filter(vegetation_type == 'grassland/shrubland' & master_stratification != 'nontarget') 

#---------- Reduce Data Set --------------
## Only want one visit to each plot to determine dominant species across all plots
# find the first visit, excluding pilot years
first_visit <- pi_raw_long %>%
  select(unit_code, master_stratification, visit_year, plot_id) %>%
  filter(!visit_year %in% c(2009, 2010, 2011)) %>% #drop pilot years
  group_by(unit_code, master_stratification, visit_year, plot_id) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(unit_code, master_stratification, plot_id) %>% #regroup 
  summarize(min_year = min(visit_year)) #first visit year

## and then do a filtering join to only get one visit per plot
pi_single_visit <- pi_raw_long %>%
  right_join(first_visit, by = c('unit_code', 'master_stratification', 'plot_id', 'visit_year' = 'min_year')) #tested and this worked

#-------- Calculate Cover and Frequency ----------------------
## Since goal is to get relativized cover, I just did all hits instead of only one hit per species per point

# define surface codes
surf_codes <- c(NA, "U", "CY", "LC", "M", "S", "L", "WD", "SR", "LR", "BR", "SL", "SW", "WA", "NR")

# drop surface codes and disturbance codes
pi_single_visit <- pi_single_visit %>%
  filter(!species %in% surf_codes) %>%
  filter(!layer %in% c('D1', 'D2'))

# calculate species cover
pi_sp_cover <- pi_single_visit %>%
  group_by(unit_code, master_stratification, visit_year, plot_id, species) %>%
  summarize(sp_hits = n(), #count species occurrences
            live_sp_hits = sum(status),
            pct_cover = (sp_hits/300)*100) %>%
  ungroup()

# calculate total cover
pi_total_cover <- pi_single_visit %>%
  group_by(unit_code, master_stratification, visit_year, plot_id) %>%
  summarize(tot_hits = n(), #count all occurrences all species
            tot_cover = (tot_hits/300)*100) %>%
  ungroup()

# join, calculate relative cover, average over plots and calculate species frequency
pi_sp_cover <- pi_sp_cover %>%
  left_join(pi_total_cover, by = c('unit_code', 'master_stratification', 'visit_year', 'plot_id')) %>%
  mutate(rel_cover = pct_cover/tot_cover)  %>% #calculate relative cover!
  group_by(unit_code, master_stratification, species) %>%
  summarize(avg_rel_cover = mean(rel_cover), 
            sp_frequency = n()) #this is number of plots the species occurred in, i.e. frequency

# calculate total number of plots by master stratification
total_plots <- pi_single_visit %>%
  group_by(unit_code, master_stratification, plot_id) %>%
  summarize(n_species = n()) %>%
  ungroup() %>%
  group_by(unit_code, master_stratification) %>%
  summarize(total_plots = n()) %>% #cross check with survey design and correct
  ungroup()

# join
pi_sp_cover <- pi_sp_cover %>%
  left_join(total_plots, by = c('unit_code', 'master_stratification')) %>%
  mutate(rel_frequency = sp_frequency/total_plots)

#---------- Calculate DCindex -------------------
## From Alvolio et al 2019
# Dominance candidate index
# DCi = (avg rel abundance + relative frequency)/2; range should be between 0 and 1

dci <- pi_sp_cover %>%
  mutate(dci = (avg_rel_cover + rel_frequency)/2) %>%
  left_join(select(ncpn_plants, Master_PLANT_Code, Master_Species, Master_Common_Name, Photosynthetic_Pathway, Lifeform, Duration, Nativity), by = c('species' = 'Master_PLANT_Code'))

#write.csv(dci, './output/dci.csv', row.names = F)

