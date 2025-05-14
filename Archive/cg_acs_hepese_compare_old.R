#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

  # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025
  
  # TABLE 4: HEPESE/ACS sample comparison

# Script created in RStudio ("Kousa Dogwood")
# updated 1/27/2025
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
  
    
  #### Load IPUMS data ---------------------------
    # DATA LOCATION: Google Drive -> Postdoc -> Projects -> Caregivers-Housing -> Replication Files -> Data -> Raw Data -> IPUMS ACS
    setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/IPUMS ACS")
    ddi <- read_ipums_ddi("usa_00011.xml")
      
      
  #### Use the object to load its associated data ---------------------------
    acs <- read_ipums_micro(ddi)
    
    
  #### Resources for working with IPUMS:
    # Working with Census microdata: https://walker-data.com/tidycensus/articles/pums-data.html 
    
    
    
#### SAVE 
write.csv(acs, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/acs.csv")
saveRDS(acs, file = "acs.rds")   
    
    



  #===============#
  #=== CLEANING ==#
  #===============#
  
    
  #### 1. Gen aging parents/care recipients sample ---------------------------
    parents_acs <- acs %>% 
      filter(AGE %in% 79:102, # to match w7 --> https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0001/variables/AGE7?archive=nacda
             HISPAN == 1, # Mexican
             GQ %in% 1:2, # exclude institutionalized (to match HEPESE community-dwelling sample). 1= Households under 1970 definition | 2 = Additional households under 1990 definition
             DIFFMOB == 2 | # has independent living difficulty
             DIFFPHYS == 2 | # or limited basic physical activities, such as walking, climbing stairs, reaching, lifting, or carrying.
             DIFFCARE == 2 | # or physical or mental health condition that has lasted at least 6 months and makes it difficult for them to take care of their own personal needs   
             DIFFREM == 2) # or cognitive difficulties
    # n = 4077 (unweighted)
    

    
  #### 2. Gen adult children caregiver sample ---------------------------
   
    ## Resource for categorizing family caregivers: https://forum.ipums.org/t/categorization-of-paid-family-caregivers/5974 
    
    ## NOTES ---------------------------
      # ID-ing co-residential AND non-co-residential parents is tricky with ACS since it's household-based
      # Do not use RELATE or SFRELATE, as they focus on *co-residential* household relationships and subfamily structures 
      # WHAT ARE SUBFAMILIES, AND HOW DOES THE IPUMS MEASURE THEM? --> https://usa.ipums.org/usa/volii/subfamilies.shtml      
          # SFRELATE: relationship within subfamily --> https://usa.ipums.org/usa-action/variables/SFRELATE#description_section
            # The Census Bureau assigns a "reference person" to each subfamily (==1). In parent-child subfamilies, this is the parent.
          # SFTYPE: Parent-child subfamilies comprise an *unmarried parent* living with one or more of their own, never-married children under 18 years of age --> https://usa.ipums.org/usa/volii/subfamilies.shtml
            # However, can't use to gen ACC or parent samples bc parent-child subfamilies comprise an unmarried parent *living with* one or more of their own, never-married children under 18 years of age.   
      # To ID adult children, I initially, wanted to use SFRELATE instead of RELATE, since RELATE is in relation to HOH (what if the ACC is the HOH? If I filter by "child" they would be missed! Also it implies co-residence - for the analytic sample we want all ACCs and co-residence is the main independent variable) 
      # SICKNESS == 2104: dementia
      # RELATE: relationship to household head --> https://usa.ipums.org/usa-action/variables/RELATE#description_section    
    
    acc_acs <- acs %>%
      filter(AGE %in% 18:70,     # to match w7 --> https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/AGE7I?archive=nacda
             HISPAN == 1,        # Mexican
             CLASSWKRD == 29,    # CLASSWKRD==29: Unpaid family worker (proxy for informal caregiver)
             GQ %in% 1:2)        # exclude institutionalized (to match HEPESE community-dwelling sample). 1= Households under 1970 definition | 2 = Additional households under 1990 definition
    # n = 427 (unweighted)
    
        # Small-ish sample bc only ~2,600 (unweighted) are unpaid family workers (further reduced by demo restrictions)
          table(acs$CLASSWKRD)
          table(acs$CLASSWKRD, acs$HISPAN)
    
    

          
          
            
  #=======================#
  #=== ACS DESCRIPTIVES ==#
  #=======================#   

          
    #### 1. Weighted n of ACCs ---------------------------   
      acc_n <- sum(acc_acs$PERWT) # n = 42,174
      
       
      
    #### 2. Weighted n of aging parents ---------------------------   
      parents_n <- sum(parents_acs$PERWT) # n = 367,444
         
          
    
  #### ADULT CHILDREN CAREGIVERS  

    #### 3. Age (mean) ---------------------------
      
      # First create survey design object
        acc_acs_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acc_acs) 
          
        acc_age_mean <- svymean(~AGE, acc_acs_svy) # 37.5


      
    #### 4. % Female ---------------------------   
      weighted_female <- svymean(~I(SEX == 2), acc_acs_svy)
      print(weighted_female) # 51.4
      

      
    #### 5.) Educ ---------------------------    
      educ_pct <- svymean(~I(EDUC <= 05) +             # Less than high school (N/A to Grade 11)
                            I(EDUC == 06) +              # High school (Grade 12)
                            I(EDUC %in% c(07,08,09)) +   # Some college (1-3 years)
                            I(EDUC >= 10),               # Bachelor's+ (4+ years)
                          acs_svy) * 100

      acc_acs <- acc_acs %>%
        mutate(
          educ_cat = case_when(
            EDUC %in% 0:5 ~ "Less than High School",
            EDUC == 6 ~ "High School Graduate",
            EDUC %in% 7:9 ~ "Some College",
            EDUC %in% 10:11 ~ "College Graduate or Higher",
            TRUE ~ NA_character_ # Handle missing values
          )
        )
      
      # Create survey design
        acc_acs_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acc_acs)
      
      # Weighted proportions for education levels
        educ_dist <- svymean(~factor(educ_cat), acc_acs_svy, na.rm = TRUE)
      
      # Print results
        print(educ_dist)

      
      
    ## 5a.) Recode EDUC into HS/No HS categories
      acc_acs <- acc_acs %>%
        mutate(
          hs_binary = case_when(
            EDUC %in% 0:5 ~ "Less than High School",
            EDUC %in% 6:11 ~ "High School or More",
            TRUE ~ NA_character_ # Handle missing values
          )
        )
      
      # Create survey design
        acc_acs_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acc_acs)
      
      # Weighted proportions for HS/No HS
        hs_distribution <- svymean(~factor(hs_binary), acc_acs_svy, na.rm = TRUE)
      
      # Print results
        print(hs_distribution)
      
      
      
  #### 6.) HH income ($20k binary) ---------------------------   
    summary(acc_acs$HHINCOME)
    
    # Create 3-category income variable
    acc_acs <- acc_acs %>%
      mutate(
        inc_3cat = case_when(
          HHINCOME < 20000 ~ "< $20,000",
          HHINCOME >= 20000 ~ "$20,000+",
          is.na(HHINCOME) ~ "Missing",
          TRUE ~ NA_character_
        )
      )
    
    # Create survey design
      acc_acs_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acc_acs)
    
    # Weighted proportions for HS/No HS
      income_cat_pct <- svymean(~factor(inc_3cat), acc_acs_svy, na.rm = TRUE)
    
    # Print results
      print(income_cat_pct)
    
      

  #### 7.) Married % 
      acc_acs <- acc_acs %>%
        mutate(
          married_dummy = case_when(
            MARST %in% c(1, 2) ~ 1,  # Married (spouse present or absent)
            MARST %in% c(3, 4, 5, 6) ~ 0,  # Not married (separated, divorced, widowed, never married)
            TRUE ~ NA_real_  # Handle missing values
          )
        )
      
      # Create survey design
      acc_acs_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acc_acs)
      
      # Weighted proportions for HS/No HS
      married_pct <- svymean(~factor(married_dummy), acc_acs_svy, na.rm = TRUE)
      
      # Print results
      print(married_pct)
      

      # Or, Calculate weighted percentage married using survey design
      married_pct <- svymean(~married_dummy, acc_acs_svy, na.rm = TRUE) * 100