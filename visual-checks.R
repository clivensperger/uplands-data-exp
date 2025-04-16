###-------------------------------------------------####
#- Title: Plot Visits
#- Author: Carolyn Livensperger
#- Date: April 14, 2025
#- Update: 
#- 
#- Data Source: Uplands vegetation and soils data
#-
#- This script is to visualize raw data before sending for data requests.  
#-
###--------------------------------------------------####

#------- Packages -----------
library(tidyverse)
library(lubridate)
library(janitor)

#------ Import Data ------------
raw_pi <- read.csv('../uplands-dm/output/pi-raw-250417.csv')
