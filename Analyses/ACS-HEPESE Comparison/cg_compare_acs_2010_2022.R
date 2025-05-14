#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

# "Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
# PAA 2025

  # HEPESE & ACS sample comparison: change over time 2010/11 vs 2022/23

# Script created in RStudio ("Kousa Dogwood")
# updated 3/10/2025
#=======================================================================================================#


#==============#
#=== SET-UP ===#
#==============#

#### Load packages
library(tidyverse)
library(gtsummary)
library(flextable)
library(smd)  
library(ipumsr)
library(survey)
library(srvyr)


#### Load ACS data ---------------------------
# Extract already restricted to 2010-11 & AZ, CA, CO, NM, TX
# DATA LOCATION: Google Drive -> Postdoc -> Projects -> Caregivers-Housing -> Replication Files -> Data -> Raw Data -> IPUMS ACS
setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/IPUMS ACS")
ddi <- read_ipums_ddi("usa_00015.xml")


#### Use the IPUMS object to load its associated data ---------------------------
acs_2223 <- read_ipums_micro(ddi)




#### SAVE 
write.csv(acs_2223, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/acs_2223.csv")
