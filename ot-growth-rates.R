###-------------------------------------------------####
#- Title: Overstory Tree Growth Rates
#- Author: Carolyn Livensperger
#- Date: June 26, 2025
#- 
#- Data Source: Plot overstory tree data extracted from Uplands database. Some has been edited in excel to exclude invalid entries into the census.
#-
#- This script is to get an estimate of growth rates for different tree species. See ot-growth-rates-blcu.R for pinyon and juniper tree growth rates at BLCA and CURE.
#-
#- Note that this is the uncorrected data for BRCA. That might not matter too much?

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

# currently looking at BRCA only
trees_brca <- trees %>%
  filter(unit_code == 'BRCA') %>%
  filter(master_stratification != 'nontarget') %>%
  select(-c(primary_eco_site, vegetation_type, quad, crown_class, notes)) %>%
  mutate(master_stratification = ifelse(master_stratification == 'High-MixedConifer', 'MixedCon', 'PJ'))

trees_brca <- trees_brca %>%
  filter(crown_health < 5) %>% #live trees only
  mutate(unique_id = paste(plot_id, tag_no, species, sep = '_')) %>% #create unique id
  filter(!(visit_year == 2010 & species == 'PIED')) %>% #filter out PINEDU and JUNOST that were measured in 2010, these tags were all moved in the subsequent visit
  filter(!(visit_year == 2010 & species == 'JUOS'))

#--------- Summarize -----------
# group by unique id and identify first and last visits
start_end <- trees_brca %>%
  group_by(unique_id) %>%
  mutate(first_visit = min(visit_year),
         last_visit = max(visit_year),
         year_diff = last_visit - first_visit) %>%
  filter(year_diff != 0) #drop plots or tags that were only visited once

# this drops the intermediate visits, so growth rate just based on first and last visits
test <- start_end %>%
  mutate(visit_type = ifelse(visit_year == first_visit, 'yr_initial', 'drop'),
         visit_type = ifelse(visit_year == last_visit, 'yr_final', visit_type)) %>%
  filter(visit_type != 'drop')

growth_rate <- test %>%
  select(unique_id, visit_type, year_diff, dbh) %>%
  group_by(unique_id) %>%
  pivot_wider(names_from = visit_type, values_from = dbh)

growth_rate <- growth_rate %>%
  mutate(cm_per_yr = (yr_final-yr_initial)/year_diff)

hist(growth_rate$cm_per_yr, breaks = 20)

mean(growth_rate$cm_per_yr)

median(growth_rate$cm_per_yr)

# may be outliers in this dataset since I didn't exclude to valid entries only
# but since rates are based on a unique id it might not matter that much, invalid entries would grow at same rate
# and similarly, rates of retags would just be based on comparison to themselves

## Explore individual species
growth_rate <- growth_rate %>%
  separate(unique_id, into = c('plot_id', 'tag_no', 'species'), remove = FALSE)

# summarize growth rate stats by species
growth_rate_sp <- growth_rate %>%
  group_by(species) %>%
  summarize(min_cm_per_yr = min(cm_per_yr),
            max_cm_per_yr = max(cm_per_yr),
            mean_cm_per_yr = mean(cm_per_yr),
            median_cm_per_yr = median(cm_per_yr),
            n_cm_per_yr = n())

## Visualize
p_growth_rate_hist <- ggplot(growth_rate, aes(cm_per_yr)) +
  geom_histogram() +
  facet_wrap(.~species)
p_growth_rate_hist

## Export
write.csv(growth_rate_sp, './output/brca_species_growth_rates.csv', row.names = F)

