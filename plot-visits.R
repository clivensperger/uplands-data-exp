###-------------------------------------------------####
#- Title: Plot Visits
#- Author: Carolyn Livensperger
#- Date: April 6, 2023
#- Update: November 6, 2024
#- 
#- Data Source: Plot visit data extracted from Uplands database
#-
#- This script is to visualize number and dates of plots visits for Uplands.  
#-
###--------------------------------------------------####

#------- Packages -----------
library(tidyverse)
library(lubridate)
library(janitor)

my_theme <- function() {
  theme_bw() %+replace%
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 13, face = 'bold'), 
      legend.position = "bottom", 
      legend.text = element_text(size = 12),
      legend.title = element_blank())
}

#------- Load Data --------------------
visits <- read.csv('../uplands-dm/output/plot-visits-240722.csv')

#------- Clean up -------------
visits <- visits %>%
  mutate(Start_Date = ymd(Start_Date)) %>%
  mutate(Year = year(Start_Date)) %>%
  clean_names() %>%
  filter(master_stratification != 'nontarget') #remove non-target plots

#------- Summaries -------------
visit_totals_by_park <- visits %>%
  group_by(unit_code) %>%
  summarize(n = n())

#------- Visualize -----------
p_all_parks_effort <- ggplot(visit_summary, aes(as.factor(year), n), color = unit_code) +
  geom_col() +
  facet_grid(unit_code~.) +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme()
p_all_parks_effort
#ggsave(filename = 'Uplands Plot Effort.jpg', path = './Uplands/', width = 8, height = 8)

p_all_parks_stacked <- ggplot(visit_summary, aes(as.factor(year), n, fill = unit_code)) +
  geom_col(position = 'stack') +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme()
p_all_parks_stacked
#ggsave(filename = 'Uplands Plot Effort Color.jpg', path = './Uplands/output', width = 8, height = 4)

#------ Park Specific Summaries ----------
## BLCU for trend report
#filter to BLCA and CURE and calculate percent completed
blcu_visits <- visits %>%
  group_by(unit_code, year) %>%
  summarize(n = n()) %>%
  filter(unit_code == 'BLCA' | unit_code == 'CURE') %>%
  filter(year >= 2011) %>%
  mutate(total = ifelse(unit_code == 'BLCA', 24, 18)) %>% #number of scheduled plots per year
  mutate(total = ifelse(year %in% c('2012', '2013', '2017', '2018') & unit_code == 'BLCA', 26, total)) %>%
  mutate(pct_complete = (n/total)*100)

ggplot(blcu_visits, aes(as.factor(year), pct_complete)) +
  geom_col() +
  facet_grid(unit_code ~ .) +
  labs(y = 'Percent of Plots Completed', x = 'Year') +
  ylim(0, 100) +
  my_theme()
#ggsave(filename = 'BLCA-CURE Uplands Plot Effort.jpg', path = './Uplands/output/', width = 8, height = 6)

#get range of visit dates
blcu_dates <- visits %>%
  filter(unit_code == 'BLCA' | unit_code == 'CURE') %>%
  filter(year >= 2011) %>%
  group_by(year) %>%
  summarize(year_min = min(start_date),
            year_max = max(start_date))

## --------- Sampling Effort by Stratum ---------------
#-------- could rewrite this with a loop? --------------
## ARCH ------------
arch_visits <- visit_summary %>%
  filter(unit_code == 'ARCH')

p_arch_visits <- ggplot(arch_visits, aes(as.factor(year), n, fill = master_stratification)) +
  geom_col(position = 'stack') +
  scale_fill_brewer(palette = "Spectral") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme()
p_arch_visits
#ggsave(filename = 'ARCH plot effort.jpg', path = './Uplands/output/', width = 8, height = 4, dpi = 300)

#BLCA
blca_visits <- visit_summary %>%
  filter(unit_code == 'BLCA')

p_blca_visits <- ggplot(blca_visits, aes(as.factor(year), n, fill = master_stratification)) +
  geom_col(position = 'stack') +
  scale_fill_brewer(palette = "Spectral") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme()
p_blca_visits
#ggsave(filename = 'BLCA plot effort.jpg', path = './Uplands/output/', width = 8, height = 4, dpi = 300)

spectral_colors <- RColorBrewer::brewer.pal(4, "Spectral")

#CURE
cure_visits <- visit_summary %>%
  filter(unit_code == 'CURE')

p_cure_visits <- ggplot(cure_visits, aes(as.factor(year), n, fill = master_stratification)) +
  geom_col(position = 'stack') +
  scale_fill_manual(values = c("#2B83BA", "#99D594")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme()
p_cure_visits
#ggsave(filename = 'CURE plot effort.jpg', path = './Uplands/output/', width = 8, height = 4, dpi = 300)

## BRCA ------------
brca_visits <- visit_summary %>%
  filter(unit_code == 'BRCA')

p_brca_visits <- ggplot(brca_visits, aes(as.factor(year), n, fill = master_stratification)) +
  geom_col(position = 'stack') +
  scale_fill_brewer(palette = "Spectral") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme()
p_brca_visits
#ggsave(filename = 'BRCA plot effort.jpg', path = './Uplands/output/', width = 8, height = 4, dpi = 300)

## CANY ------------
cany_visits <- visit_summary %>%
  filter(unit_code == 'CANY')

p_cany_visits <- ggplot(cany_visits, aes(as.factor(year), n, fill = master_stratification)) +
  geom_col(position = 'stack') +
  scale_fill_brewer(palette = "Spectral") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme() +
  theme(axis.text.x = element_text(angle = 90))
p_cany_visits
#ggsave(filename = 'CANY plot effort.jpg', path = './Uplands/output/', width = 8, height = 4, dpi = 300)

## CARE ------------
care_visits <- visit_summary %>%
  filter(unit_code == 'CARE')

p_care_visits <- ggplot(care_visits, aes(as.factor(year), n, fill = master_stratification)) +
  geom_col(position = 'stack') +
  scale_fill_brewer(palette = "Spectral") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme() 
p_care_visits
#ggsave(filename = 'CARE plot effort.jpg', path = './Uplands/output/', width = 8, height = 4, dpi = 300)

## CEBR ------------
cebr_visits <- visit_summary %>%
  filter(unit_code == 'CEBR')

p_cebr_visits <- ggplot(cebr_visits, aes(as.factor(year), n, fill = master_stratification)) +
  geom_col(position = 'stack') +
  scale_fill_brewer(palette = "Spectral") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme() 
p_cebr_visits
#ggsave(filename = 'CEBR plot effort.jpg', path = './Uplands/output/', width = 8, height = 4, dpi = 300)

## DINO ------------
dino_visits <- visit_summary %>%
  filter(unit_code == 'DINO')

p_dino_visits <- ggplot(dino_visits, aes(as.factor(year), n, fill = master_stratification)) +
  geom_col(position = 'stack') +
  scale_fill_brewer(palette = "Spectral") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme() 
p_dino_visits
#ggsave(filename = 'DINO plot effort.jpg', path = './Uplands/output/', width = 8, height = 4, dpi = 300)

## ZION ------------
zion_visits <- visit_summary %>%
  filter(unit_code == 'ZION')

p_zion_visits <- ggplot(zion_visits, aes(as.factor(year), n, fill = master_stratification)) +
  geom_col(position = 'stack') +
  scale_fill_brewer(palette = "Spectral") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(y = 'Number of Plots', x = 'Year') +
  my_theme() 
p_zion_visits
#ggsave(filename = 'ZION plot effort.jpg', path = './Uplands/output/', width = 8, height = 4, dpi = 300)
