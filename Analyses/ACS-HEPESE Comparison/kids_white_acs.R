#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

  # "Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025
  
  # HEPESE & ACS sample comparison: non-Hispanic Whites

# Script created in RStudio ("Kousa Dogwood")
# updated 2/11/2025
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
    
    
    
  #### Load data 
    # start with acs file (loaded in the "kids_acs_hepese_compare" script)
    
    

    
      ## 1. Filter for non-Hispanic Whites ---------------------------
        
          # 1a.) Check RACE variable 
            # Use RACHSING / RACESING: single race identification
            table(acs$RACHSING)
      
            acs_whites <- acs %>%
              filter(RACHSING == 1)               
            
            length(unique(acs_whites$person_id)) # n = 752,519

      
    
            
         
      ## 2. Create subset of HHs with adult children and their older parents ---------------------------
            acs_whites <- acs_whites %>%
              group_by(hh_id) %>%              # Ensure in same HH 
              filter(GQ %in% 1:2 | GQ==5) %>%  # 1:2 (exclude group quarters); 5 (HHs w/10+ or individuals unrelated to the HOH)
              mutate(                                                                    
                mother_age = if_else(MOMLOC > 0, AGE[match(MOMLOC, PERNUM)], NA_real_),        # If MOMLOC > 0 (mother lives in same HH), then find her age; match(MOMLOC, PERNUM) finds the row where PERNUM equals MOMLOC; AGE[match(...)] gets the age of that person (the mother)
                father_age = if_else(POPLOC > 0, AGE[match(POPLOC, PERNUM)], NA_real_)) %>%    # Same for dads
              filter(
                any(AGE >= 18 & (mother_age >= 80 | father_age >= 80)) &                       # Ensure an adult child (18+) lives with a parent (80+)
                  any(RELATE == 1 & (AGE >= 18 | mother_age >= 80 | father_age >= 80))) %>%    # Ensure either adult child OR parent is the HOH
              ungroup()                                                                        
            

              length(unique(acs_whites$person_id)) # n = 
          
          
              
            
#### SAVE 
write.csv(acs_whites, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/acs_whites.csv")
  setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp")
  saveRDS(acs_whites, file = "acs_whites.rds")  
  
  
  
  
  
  
  #================#
  #=== CLEANING ===#
  #================#
  
  
  #### 3. ACS Cleaning ---------------------------   
  
    #### 3a. Collapse EDUC into HS/No HS categories ---------------------------
      acs_whites <- acs_whites %>%
        mutate(
          hs_binary = case_when(
            EDUC %in% 0:5 ~ "Less than High School",
            EDUC %in% 6:11 ~ "High School or More",
            TRUE ~ NA_character_)) # Handle missing values
    
    # Weighted proportions for HS/No HS
      acs_whites_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_whites)
      hs_distribution <- svymean(~factor(hs_binary), acs_whites_svy, na.rm = TRUE)
      print(hs_distribution)
  
  
  
  
  
  #### 3b. Collapse HH income ($20k binary) ---------------------------   
    summary(acs_whites$HHINCOME)
  
  # Create 3-category income variable
    acs_whites <- acs_whites %>%
      mutate(
        inc_3cat = case_when(
          HHINCOME < 20000 ~ "< $20,000",
          HHINCOME >= 20000 ~ "$20,000+",
          HHINCOME == 9999999 ~ "NA",
          is.na(HHINCOME) ~ "Missing",
          TRUE ~ NA_character_))
  
  # Weighted proportions 
    acs_whites_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_whites)
    income_cat_pct <- svymean(~factor(inc_3cat), acs_whites_svy, na.rm = TRUE)
    print(income_cat_pct)
    
    table(acs_whites$HHINCOME)
    table(acs_whites$inc_3cat)
  
  
        # Missing income
          missing_inc <- sum(is.na(acs_whites$HHINCOME)) / nrow(acs_whites) * 100

  
  
  
  
  #### 3c. Collapse married ---------------------------   
    acs_whites <- acs_whites %>%
      mutate(
        married_dummy = case_when(
          MARST %in% c(1, 2) ~ 1,  # Married (spouse present or absent)
          MARST %in% c(3, 4, 5, 6) ~ 0,  # Not married (separated, divorced, widowed, never married)
          TRUE ~ NA_real_))  # Handle missing values
  
  # Weighted proportions 
    acs_whites_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_whites)
    married_pct <- svymean(~factor(married_dummy), acs_whites_svy, na.rm = TRUE)
    print(married_pct)
  
  # Or, can calculate weighted percentages this way too
    married_pct <- svymean(~married_dummy, acs_whites_svy, na.rm = TRUE) * 100    
  
  
  
  
  #### 3d. Visually inspect sample ---------------------------  
    acs_whites_samp <- acs_whites %>%
      select(YEAR, SERIAL, PERNUM, STATEFIP, SEX, AGE, RELATE, MOMLOC, POPLOC, NCHILD, 
             MARST, married_dummy, RACE, HISPAN, EDUC, hs_binary, HHINCOME, inc_3cat, NUMPREC, FAMSIZE, GQ, PERWT)
  
  
  
  
#### SAVE 
write.csv(acs_whites, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/acs_whites.csv")
write.csv(acs_whites_samp, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/acs_whites_samp.csv")
    
  
    


    
  #=======================================#
  #=== ACS TABLE: WHITE ADULT CHILDREN ===#
  #=======================================#

    
  #### 4. Set-up ACS sample ---------------------------
    
    ## 4a. Get just the adult children (not their parents or other HH members) ---------------------------   
      # Otherwise the descriptives will reflect pooled data for children + parents (e.g., age will be skewed)  
        adult_children_white_acs <- acs_whites %>%
          filter(AGE >= 18 & (mother_age >= 80 | father_age >= 80)) # Keep only adults who have at least one elderly parent in the household

          unique(length(adult_children_white_acs$person_id)) # n = 6,038       


          # Visually inspect sample ---------------------------  
            samp_white_kids <- adult_children_white_acs %>%
              select(YEAR, SERIAL, PERNUM, STATEFIP, RELATE, RELATED, MOMLOC, POPLOC, NCHILD, 
                     SEX, AGE, mother_age, father_age, MARST, married_dummy, RACE, HISPAN, EDUC, hs_binary, HHINCOME, inc_3cat, NUMPREC, FAMSIZE, PERWT)

            
                       
#### SAVE 
write.csv(adult_children_white_acs, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/adult_children_white_acs.csv")
write.csv(samp_white_kids, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/samp_white_kids.csv")
            
    

    
    ## 4b. Gen ACS desc table ---------------------------
            
      # Set compact theme for gtsummary
        theme_gtsummary_compact()
            
         
      # TABLE (weighted)
        adult_children_white_acs %>% 
          mutate(
            SEX = haven::as_factor(SEX),   # convert haven_labelled columns to factors (otherwise get warning)
            LANGUAGE = haven::as_factor(LANGUAGE)
          ) %>%
          as_survey_design(weight = PERWT) %>% # weights (N will be weighted, but ignore - we only want weighted prop/means, not sample sizes)
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
          modify_caption("**HEPESE vs ACS Caregiver Demographics: White Adult Children (18+) Living with Aging Parents (80+) in AZ, CA, CO, NM, TX (2010–2011)**")
      
      
      
      
    