#=======================================================================================================#
# Anna Bokun: bokun001@umn.edu
# Sociology PhD Candidate, University of Minnesota
# Postdoc, Population Research Center, University of Texas-Austin

# ICAA 2024
# "Living with An Aging Parent in the U.S. and Mexico (or California and Texas): Who benefits, Mom, Dad, (Elderly Parent) or her Children?”  
# Authors: Anna Bokun, Jacqui Angel, Sunshine Rote, and Phil Cantu

  # Demographic Tables
  # Table 2 - movement variables 

# Script created in RStudio ("Chocolate Cosmos")
# updated 7/22/2024
#=======================================================================================================#



  #==============#
  #=== SET-UP ===#
  #==============#
  
  #### Load packages 
    library(tidyverse)
    library(vtable)
    library(tabulator)
  
  
  
  #### Load data 
  # HEPESE, wave 7 (2010-2011): https://www.icpsr.umich.edu/web/NACDA/studies/36537  
  # Google Drive --> UT-Austin --> ICAA 2024 --> Caregivers - Housing Health Paradox --> Data --> Raw Data
  # Originally, Suyoung Kim shared the files with me
  # 01 = recipients
  # 02 = caregivers 
  
  recipients <- read.delim("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/36537_0001_Data.tsv")
  caregivers <- read.delim("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/36537_0002_Data.tsv")

  
  
  
  
  
  
  
  #### 1.) Merge caregivers + recipients by Q_NO ---------------------------
    wave7_all <- merge(recipients, caregivers, by = "Q_NO", all = TRUE)
  
  
  
  
  
  
#### Save wave7_all
write.csv(wave7_all, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/wave7_all.csv")
  






  
  
  
  
  
  #### 2.) Explore "movement" vars by demographics ---------------------------
  
    ## Table 2, column d: Adult child caregiver who moved in ---------------------------
      
      # Who is an adult child caregiver that moved in?
        acc_moved_in <- wave7_all %>%
          filter(RELSUBJ7I == 3 & WHOMOV71 == 3)
  
        length(unique(acc_moved_in$Q_NO)) # n = 63
  
  
      # Inspect WHOMOV71
        WHOMOV71 <- wave7_all %>%
          tab(WHOMOV71)
  
  
      # Age
        mean_age <- wave7_all %>%
          filter(RELSUBJ7I == 3 & WHOMOV71 == 3) %>%
          summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 55.66667

  
      # % Female
        # 1	= Male
        # 2	= Female
        
        female_percent <- mean(acc_moved_in$SEX7I == 2, na.rm = TRUE) * 100
        print(female_percent) # 71.4285
  
    
        
      # HH income 
        inc <- wave7_all %>%
          tab(LL3A7I)
        
        # 1	$0-$4,999
        # 2	$5,000-$9,999
        # 3	$10,000-$14,999
        # 4	$15,000-$19,999
        # 5	$20,000-$29,999
        # 6	$30,000-$39,999
        # 7	$40,000-$49,999
        # 8	$50,000 & Over
        # 98 Do not know
        # 99 Refused
        
        
        inc_acc_moved_in <- acc_moved_in %>%
          tab(LL3A7I, round = 3) 
  
  
        
        
      # HH size
        # NHOUSE7I: Number of people living informant`s house

        mean_hh_size <- acc_moved_in %>%
          summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 3.18124
  
        
        
        
        
      # % Married 
        # CMARSTAT7I_1: CMARSTAT7I_16 --> Marital status of respondent`s child #1 (per informant)
        # Only available in the informant file if the caregiver is a child of the informant      
        
        # 1	Married
        # 2	Separated
        # 3	Divorced
        # 4	Widowed
        # 5	Never married
        # 9	Do not know / refused
        
        
        # Tally 
        married <- acc_moved_in %>%
          tab(CMARSTAT7I_1)
        
        
        
        
        
        
    ## Table 2, column e: Adult child caregiver who moved in because recipient needs help ---------------------------
        
        # Who is an adult child caregiver that moved in bc recipient needs help?
        acc_moved_in_help <- wave7_all %>%
          filter(RELSUBJ7I == 3 & TAKECARE7 == 1)
        
        length(unique(acc_moved_in_help$Q_NO)) # n = 42
        
        
        # Inspect TAKECARE7
        TAKECARE7 <- wave7_all %>%
          tab(TAKECARE7)
        
        
        # Age
          mean_age <- acc_moved_in_help %>%
            summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 54.80952
          
          
        
        # % Female
          # 1	= Male
          # 2	= Female
        
        female_percent <- mean(acc_moved_in_help$SEX7I == 2, na.rm = TRUE) * 100
        print(female_percent) # 69.04762
        
        
        
        
        # HH income 
        inc <- wave7_all %>%
          tab(LL3A7I)
        
        # 1	$0-$4,999
        # 2	$5,000-$9,999
        # 3	$10,000-$14,999
        # 4	$15,000-$19,999
        # 5	$20,000-$29,999
        # 6	$30,000-$39,999
        # 7	$40,000-$49,999
        # 8	$50,000 & Over
        # 98 Do not know
        # 99 Refused
        
        inc_acc_moved_in_help <- acc_moved_in_help %>%
          tab(LL3A7I, round = 3) 
        
        
        
        
        # HH size
          # NHOUSE7I: Number of people living informant`s house
        
        mean_hh_size <- acc_moved_in_help %>%
          summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 3.952381
        
        
        
        
        
        # % Married 
          # CMARSTAT7I_1: CMARSTAT7I_16 --> Marital status of respondent`s child #1 (per informant)
          # Only available in the informant file if the caregiver is a child of the informant      
          
        # 1	Married
        # 2	Separated
        # 3	Divorced
        # 4	Widowed
        # 5	Never married
        # 9	Do not know / refused
        
        # Tally 
          married <- acc_moved_in_help %>%
            tab(CMARSTAT7I_1)
        
  
          
          
          
          
          
          
          
          
          
  ## Table 2, column f: Adult child caregiver who moved in because they needed a play to stay ---------------------------
          
      # Who is an adult child caregiver that moved in bc they need a place to stay?
        acc_moved_in_stay <- wave7_all %>%
          filter(RELSUBJ7I == 3 & PLASTAY7 == 2)
        
        length(unique(acc_moved_in_stay$Q_NO)) # n = 36
        
        
        # Inspect PLAYSTAY7
          PLASTAY7 <- wave7_all %>%
            tab(PLASTAY7)

        
        # Age
          mean_age <- acc_moved_in_stay %>%
            summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 54.80952
          
          
        
        # % Female
          # 1	= Male
          # 2	= Female
        
        female_percent <- mean(acc_moved_in_stay$SEX7I == 2, na.rm = TRUE) * 100
        print(female_percent) # 66.66667
        
        
        
        
        # HH income 
        inc <- wave7_all %>%
          tab(LL3A7I)
        
        # 1	$0-$4,999
        # 2	$5,000-$9,999
        # 3	$10,000-$14,999
        # 4	$15,000-$19,999
        # 5	$20,000-$29,999
        # 6	$30,000-$39,999
        # 7	$40,000-$49,999
        # 8	$50,000 & Over
        # 98 Do not know
        # 99 Refused
        
        
        inc_acc_moved_in_stay <- acc_moved_in_stay %>%
          tab(LL3A7I, round = 3) 
        
        
        
        
        # HH size
          # NHOUSE7I: Number of people living informant`s house
        
        mean_hh_size <- acc_moved_in_stay %>%
          summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 3.952381
        
        
        
        
        
        # % Married 
          # CMARSTAT7I_1: CMARSTAT7I_16 --> Marital status of respondent`s child #1 (per informant)
          # Only available in the informant file if the caregiver is a child of the informant      
          
        # 1	Married
        # 2	Separated
        # 3	Divorced
        # 4	Widowed
        # 5	Never married
        # 9	Do not know / refused
        
        # Tally 
          married <- acc_moved_in_stay %>%
            tab(CMARSTAT7I_1)
          
          
          
          
          
          
          
          
          
          
          
          
  ## Table 2, column g: Adult child caregiver who moved in for other reasons ---------------------------
          
      # Who is an adult child caregiver that moved in for other reasons
        acc_moved_in_other <- wave7_all %>%
          filter(RELSUBJ7I == 3 & YOUOTH7 == 3)
        
        length(unique(acc_moved_in_other$Q_NO)) # n = 26
        
        
        # Inspect YOUOTH7
          YOUOTH7 <- wave7_all %>%
            tab(YOUOTH7) # n = 45

        
        # Age
          mean_age <- acc_moved_in_other %>%
            summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 54.80952
          
          
        
        # % Female
          # 1	= Male
          # 2	= Female
        
        female_percent <- mean(acc_moved_in_other$SEX7I == 2, na.rm = TRUE) * 100
        print(female_percent) # 66.66667
        
        
        
        
        # HH income 
        inc <- wave7_all %>%
          tab(LL3A7I)
        
        # 1	$0-$4,999
        # 2	$5,000-$9,999
        # 3	$10,000-$14,999
        # 4	$15,000-$19,999
        # 5	$20,000-$29,999
        # 6	$30,000-$39,999
        # 7	$40,000-$49,999
        # 8	$50,000 & Over
        # 98 Do not know
        # 99 Refused
        
        
        inc_acc_moved_in_other <- acc_moved_in_other %>%
          tab(LL3A7I, round = 3) 
        
        
        
        
        # HH size
          # NHOUSE7I: Number of people living informant`s house
        
        mean_hh_size <- acc_moved_in_other %>%
          summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 3.952381
        
        
        
        
        
        # % Married 
          # CMARSTAT7I_1: CMARSTAT7I_16 --> Marital status of respondent`s child #1 (per informant)
          # Only available in the informant file if the caregiver is a child of the informant      
          
        # 1	Married
        # 2	Separated
        # 3	Divorced
        # 4	Widowed
        # 5	Never married
        # 9	Do not know / refused
        
        # Tally 
          married <- acc_moved_in_other %>%
            tab(CMARSTAT7I_1)