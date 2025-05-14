#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

# "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
# PAA 2025

  # PARENTS: HEPESE & ACS sample comparison

# Script created in RStudio ("Kousa Dogwood")
# updated 1/28/2025
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


  #### PARENTS ACS SAMPLE ---------------------------
  
  ## Get the older parents (not their children or other HH members) ---------------------------
    parents_acs <- acs_mex %>%
      filter(PERNUM %in% MOMLOC | PERNUM %in% POPLOC) %>%  # Keep only those who are someone's parent in the household (using MOMLOC/POPLOC)
      filter(AGE >= 80)       # And ensure they are 80+
    
    # Unweighted n 
      # n = 2,466
    
    # Weighted n 
      parents_acs_n <- sum(parents_acs$PERWT) # 244,098
  

    
    # Check sample ---------------------------  
      samp_parents <- parents_acs %>%
        select(YEAR, SERIAL, PERNUM, STATEFIP, RELATE, RELATED, MOMLOC, POPLOC, NCHILD, 
               SEX, AGE, mother_age, father_age, MARST, married_dummy, RACE, HISPAN, EDUC, hs_binary, HHINCOME, inc_3cat, NUMPREC, FAMSIZE, PERWT)
    
      
      
      
      #### Collapse EDUC into HS/No HS categories ---------------------------
        parents_acs <- parents_acs %>%
          mutate(
            hs_binary = case_when(
              EDUC %in% 0:5 ~ "Less than High School",
              EDUC %in% 6:11 ~ "High School or More",
              TRUE ~ NA_character_ # Handle missing values
            )
          )  
        
        # Weighted proportions for HS/No HS
        parents_acs_svy <- svydesign(ids = ~1, weights = ~PERWT, data = parents_acs)
        hs_distribution <- svymean(~factor(hs_binary), parents_acs_svy, na.rm = TRUE)
        print(hs_distribution)
      
      
      
      
      
      #### Collapse HH income ($20k binary) ---------------------------   
        summary(parents_acs$HHINCOME)
        
        # Create 3-category income variable
        parents_acs <- parents_acs %>%
          mutate(
            inc_3cat = case_when(
              HHINCOME < 20000 ~ "< $20,000",
              HHINCOME >= 20000 ~ "$20,000+",
              is.na(HHINCOME) ~ "Missing",
              TRUE ~ NA_character_
            )
          )
        
        # Weighted proportions for HS/No HS
        parents_acs_svy <- svydesign(ids = ~1, weights = ~PERWT, data = parents_acs)
        income_cat_pct <- svymean(~factor(inc_3cat), parents_acs_svy, na.rm = TRUE)
        print(income_cat_pct)
      
      
      
      
      
      #### Collapse married ---------------------------   
        parents_acs <- parents_acs %>%
          mutate(
            married_dummy = case_when(
              MARST %in% c(1, 2) ~ 1,  # Married (spouse present or absent)
              MARST %in% c(3, 4, 5, 6) ~ 0,  # Not married (separated, divorced, widowed, never married)
              TRUE ~ NA_real_  # Handle missing values
            )
          )
        
        # Weighted proportions for HS/No HS
        parents_acs_svy <- svydesign(ids = ~1, weights = ~PERWT, data = parents_acs)
        married_pct <- svymean(~factor(married_dummy), parents_acs_svy, na.rm = TRUE)
        print(married_pct)
        
        # Or, can calculate weighted percentages this way too
        married_pct <- svymean(~married_dummy, parents_acs_svy, na.rm = TRUE) * 100    
      
      

      
    
    #### ACS TABLE ---------------------------
      parents_acs %>% # ALREADY CO-RESIDENTIAL, SINCE IT'S A HOUSEHOLD-BASED SURVEY
        mutate(
          SEX = haven::as_factor(SEX),   # convert haven_labelled columns to factors (otherwise got warning)
          LANGUAGE = haven::as_factor(LANGUAGE)
        ) %>%
        as_survey_design(weight = PERWT) %>% # weights
        select(
          AGE, SEX, hs_binary, married_dummy, inc_3cat, # demographics
          NUMPREC, # hh size
          LANGUAGE # language used during interview
        ) %>%
        mutate(
          # Ensure continuous variables are numeric
          AGE = as.numeric(AGE),
          NUMPREC = as.numeric(NUMPREC)
        ) %>%
        tbl_svysummary(
          type = list(
            "AGE" ~ "continuous",
            "NUMPREC" ~ "continuous"
          ),
          statistic = list(
            all_continuous() ~ "{mean}",
            all_categorical() ~ "{n} ({p}%)"
          ),
          label = list(
            AGE ~ "Age",
            SEX ~ "Female",
            hs_binary ~ "Education",
            married_dummy ~ "Married",
            inc_3cat ~ "Household income (annual)",
            NUMPREC ~ "Household size", 
            LANGUAGE ~ "Language"
          ),
          missing = "always",  # Changed to show missing
          missing_text = "Missing",  # Text to display for missing values
          digits = list(
            all_categorical() ~ c(0, 1),
            all_continuous() ~ 2
          )
        ) %>%
        modify_header(
          label = "**Characteristic**"
        ) %>%
        modify_table_styling(
          columns = starts_with("stat_")
        ) %>%
        modify_caption("**HEPESE vs ACS Demographics: Aging Parents (80+) in AZ, CA, CO, NM, TX (2010–2011)**")
        
      
      
  #### SAVE 
    write.csv(parents_acs, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/parents_acs.csv")
      
