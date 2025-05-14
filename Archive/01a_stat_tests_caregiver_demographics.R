#=======================================================================================================#
# Anna Bokun: bokun001@umn.edu
# Sociology PhD Candidate, University of Minnesota
# Postdoc, Population Research Center, University of Texas-Austin

  # ICAA 2024
  # "Living with An Aging Parent in the U.S. and Mexico (or California and Texas): Who benefits, Mom, Dad, (Elderly Parent) or her Children?”  
  # Authors: Anna Bokun, Jacqui Angel, Sunshine Rote, and Phil Cantu

    # Demographic Tables
    # Table 2 - tests of statistical significance

# Script created in RStudio ("Chocolate Cosmos")
# updated 7/8/2024
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
    # Google Drive --> UT-Austin --> ICAA 2024 --> Caregivers - Housing Health Paradox --> Data
    # Originally, Suyoung Kim shared the files with me
    # 01 = recipients
    # 02 = caregivers 

    recipients <- read.delim("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/36537_0001_Data.tsv")
    caregivers <- read.delim("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/36537_0002_Data.tsv")


  #### See variables 
    vtable(caregivers)
    vtable(recipients)


    
    
  #### 1.) All caregivers vs adult children caregivers ---------------------------  
    

  #### Filter data:
    # Sons/daughters of care receiviers  
    adult_child_caregiver <- caregivers %>%
      filter(RELSUBJ7I == 3)
    
    length(unique(adult_child_caregiver$Q_NO)) # n = 629

    
    
  #### T-test for Age
    t_test_age <- t.test(caregivers$AGE7I, adult_child_caregiver$AGE7I)
    print(t_test_age)

      ## SD
        sd_all <- sd(caregivers$AGE7I, na.rm = TRUE)
        print(sd_all)
        
        sd_adult_child <- sd(adult_child_caregiver$AGE7I, na.rm = TRUE)
        print(sd_adult_child)
      
    
    
  #### T-test for Household Size
    t_test_size <- t.test(caregivers$NHOUSE7I, adult_child_caregiver$NHOUSE7I)
    print(t_test_size)
    
      ## SD
        sd_all <- sd(caregivers$NHOUSE7I, na.rm = TRUE)
        print(sd_all)
      
        sd_adult_child <- sd(adult_child_caregiver$NHOUSE7I, na.rm = TRUE)
        print(sd_adult_child)
      

    
    
  #### Chi-square test for Gender
    gender_all <- table(caregivers$SEX7I)
    gender_adult_child <- table(adult_child_caregiver$SEX7I)
    chi2_test_gender <- chisq.test(rbind(gender_all, gender_adult_child))
    print(chi2_test_gender)

    
    
  #### Chi-square test for Household Income
    income_all <- table(caregivers$LL3A7I)
    income_adult_child <- table(adult_child_caregiver$LL3A7I)
    chi2_test_income <- chisq.test(rbind(income_all, income_adult_child))
    print(chi2_test_income)

    
    
  #### Chi-square test for Marital Status
    marital_all <- table(caregivers$CMARSTAT7I_1)
    marital_adult_child <- table(adult_child_caregiver$CMARSTAT7I_1)
    chi2_test_marital <- chisq.test(rbind(marital_all, marital_adult_child))
    print(chi2_test_marital)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #### 2.) Adult children caregivers vs adult children + children-in-law caregivers ---------------------------  
    
    
    ## Who is an adult child caregiver?
    
      # Sons/daughters 
        adult_child_caregiver <- caregivers %>%
          filter(RELSUBJ7I == 3)
        
        length(unique(adult_child_caregiver$Q_NO)) # n = 629
        
      # Sons/daughters PLUS sons-in-law/daughters-in-law
        adult_child_caregiver_inlaw <- caregivers %>%
          filter(RELSUBJ7I == 3 | RELSUBJ7I == 4)
        
        length(unique(adult_child_caregiver_inlaw$Q_NO)) # n = 659
        
    
    
    
    #### T-test for Age
    t_test_age <- t.test(adult_child_caregiver_inlaw$AGE7I, adult_child_caregiver$AGE7I)
    print(t_test_age)
    
    ## SD
    sd_all <- sd(adult_child_caregiver_inlaw$AGE7I, na.rm = TRUE)
    print(sd_all)
    
    sd_adult_child <- sd(adult_child_caregiver$AGE7I, na.rm = TRUE)
    print(sd_adult_child)
    
    
    
    #### T-test for Household Size
    t_test_size <- t.test(adult_child_caregiver_inlaw$NHOUSE7I, adult_child_caregiver$NHOUSE7I)
    print(t_test_size)
    
    ## SD
    sd_all <- sd(adult_child_caregiver_inlaw$NHOUSE7I, na.rm = TRUE)
    print(sd_all)
    
    sd_adult_child <- sd(adult_child_caregiver$NHOUSE7I, na.rm = TRUE)
    print(sd_adult_child)
    
    
    
    
    #### Chi-square test for Gender
    gender_all <- table(adult_child_caregiver_inlaw$SEX7I)
    gender_adult_child <- table(adult_child_caregiver$SEX7I)
    chi2_test_gender <- chisq.test(rbind(gender_all, gender_adult_child))
    print(chi2_test_gender)
    
    
    
    #### Chi-square test for Household Income
    income_all <- table(adult_child_caregiver_inlaw$LL3A7I)
    income_adult_child <- table(adult_child_caregiver$LL3A7I)
    chi2_test_income <- chisq.test(rbind(income_all, income_adult_child))
    print(chi2_test_income)
    
    
    
    #### Chi-square test for Marital Status
    marital_all <- table(adult_child_caregiver_inlaw$CMARSTAT7I_1)
    marital_adult_child <- table(adult_child_caregiver$CMARSTAT7I_1)
    chi2_test_marital <- chisq.test(rbind(marital_all, marital_adult_child))
    print(chi2_test_marital)