#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

  # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025
  
  # TABLE X: HEPESE & ACS sample comparison

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
    library(srvyr)
    
    
  #### Load ACS 2010-2011 data ---------------------------
    # Extract already restricted to 5 HEPESE states 
    # DATA LOCATION: Google Drive -> Postdoc -> Projects -> Caregivers-Housing -> Replication Files -> Data -> Raw Data -> IPUMS ACS
    setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/IPUMS ACS")
    ddi <- read_ipums_ddi("usa_00013.xml")
      
      
  #### Use the object to load its associated data ---------------------------
    acs <- read_ipums_micro(ddi)
    
    
  
#### SAVE 
write.csv(acs, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/acs.csv")
saveRDS(acs, file = "acs.rds")  





  #===============#
  #=== CLEANING ==#
  #===============#

    #### GOAL: Broadly compare Mexican-American households with adult children (18+) AND aging parents (80+) in the ACS 
      # vs HEPESE (without the specific focus on caregivers) 
  

          #### Resources for working with IPUMS:
              # Working with Census microdata: https://walker-data.com/tidycensus/articles/pums-data.html 
              # Tables with weighted descriptives: https://tech.popdata.org/pma-data-hub/posts/2021-07-01-covid-tables/
  


  #### 1.) Gen sample of Mexican-American households with adult-children 18+ & parents 80+ ---------------------------

    ## Set working directory 
      setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data")


    ## 1a) Double-check year and state match with HEPESE 
      table(acs$YEAR, acs$STATEFIP) # good 


    ## 1b.) Filter for Mexican-American households
        mexican_hhs <- acs %>%
          filter(HISPAN == 1)   # Mexican ethnicity 
                                # n = 405,979 (unweighted) 
     
         
    ## 1c.) Identify households with both adult children (18+) AND aging parents (80+)
        acs_ma <- mexican_hhs %>%
          filter(GQ %in% c(1, 2)) %>%             # Exclude group quarters 
          group_by(SERIAL) %>%                    # Make sure in same HH 
          filter(
            any(RELATE == 3 & AGE %in% 18:70),    # 18-70 to match HEPESE
            any(RELATE == 5 & AGE %in% 79:102),   # 80-102 to match HEPESE
          ) %>%
          ungroup()
        
        # Unweighted n 
          # n = 2,408  (unweighted) 
        
        # Weighted n 
          acs_ma_n <- sum(acs_ma$PERWT) # 266,826 
        
          
        
          # Check results
          acs_ma %>%
            group_by(SERIAL) %>%
            summarise(
              n_adult_children = sum(RELATE == 3 & AGE %in% 18:70),
              n_elderly_parents = sum(RELATE == 5 & AGE %in% 79:102)
            ) %>%
            summarise(
              total_households = n(),
              avg_adult_children = mean(n_adult_children),
              avg_elderly_parents = mean(n_elderly_parents),
              min_adult_children = min(n_adult_children),
              min_elderly_parents = min(n_elderly_parents)
            )
          
          
          
          
    ## 1d.) Add additional filter for unpaid family worker
        # CLASSWKR == 29 = unpaid family worker --> https://usa.ipums.org/usa-action/variables/CLASSWKR#codes_section
        acs_ma2 <- mexican_hhs %>%
          filter(GQ %in% c(1, 2)) %>%  
          group_by(SERIAL) %>%
          filter(
            any(RELATE == 3 & AGE >= 18 & CLASSWKRD == 29),  
            any(RELATE == 5 & AGE >= 79)
          ) %>%
          ungroup()
        
        # Unweighted n 
          # n = 9 (unweighted) 
        
        # Weighted n 
          acs_ma_n <- sum(acs_ma2$PERWT) # 642 (tiny for the ACS!)

        
        
        
        
        
  #====================#
  #=== ACS CLEANING ===#
  #====================#
  
    #### Collapse EDUC into HS/No HS categories
        acs_ma <- acs_ma %>%
          mutate(
            hs_binary = case_when(
              EDUC %in% 0:5 ~ "Less than High School",
              EDUC %in% 6:11 ~ "High School or More",
              TRUE ~ NA_character_ # Handle missing values
            )
          )  
        
        # Create survey design
          acs_ma_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_ma)
        
        # Weighted proportions for HS/No HS
          hs_distribution <- svymean(~factor(hs_binary), acs_ma_svy, na.rm = TRUE)
        
        # Print results
          print(hs_distribution)
        
        
        
        
        
  #### Collapse HH income ($20k binary) ---------------------------   
        summary(acs_ma$HHINCOME)
        
        # Create 3-category income variable
        acs_ma <- acs_ma %>%
          mutate(
            inc_3cat = case_when(
              HHINCOME < 20000 ~ "< $20,000",
              HHINCOME >= 20000 ~ "$20,000+",
              is.na(HHINCOME) ~ "Missing",
              TRUE ~ NA_character_
            )
          )
        
        # Create survey design
         acs_ma_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_ma)
        
        # Weighted proportions for HS/No HS
         income_cat_pct <- svymean(~factor(inc_3cat), acs_ma_svy, na.rm = TRUE)
        
        # Print results
         print(income_cat_pct)
        
        
        

        
  #### Collapse married  ---------------------------   
        acs_ma <- acs_ma %>%
          mutate(
            married_dummy = case_when(
              MARST %in% c(1, 2) ~ 1,  # Married (spouse present or absent)
              MARST %in% c(3, 4, 5, 6, 9) ~ 0,  # Not married (separated, divorced, widowed, never married, missing)
              TRUE ~ NA_real_  # Handle missing values
            )
          )
        
        # Create survey design
          acs_ma_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_ma)
        
        # Weighted proportions for HS/No HS
          married_pct <- svymean(~factor(married_dummy), acs_ma_svy, na.rm = TRUE)
        
        # Print results
          print(married_pct)

            # Or, calculate weighted percentage married using survey design
              married_pct <- svymean(~married_dummy, acs_ma_svy, na.rm = TRUE) * 100    
            
    
        

  #### Double-check sample ---------------------------  
        samp1 <- acs_ma %>%
          select(YEAR, SERIAL, PERNUM, STATEFIP, RELATE, RELATED, MOMLOC, POPLOC, NCHILD, 
                 SEX, AGE, MARST, married_dummy, RACE, HISPAN, EDUC, hs_binary, HHINCOME, inc_3cat, NUMPREC, FAMSIZE, PERWT)
        
              
        

#### SAVE 
write.csv(acs_ma, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/acs_ma.csv")
  
        


        
  #===============#
  #=== COMPARE ===#
  #===============#
        
    #### HEPESE sample ---------------------------

      ## Gen co-residential sample 
        ACC_cg_coreside <- ACC %>%
          filter(HOUSEK_1==1) # n = 191
      

      ## Set compact theme for gtsummary
        theme_gtsummary_compact()

      ACC_cg_coreside %>%  # MUST BE CORESIDENTIAL SAMPLE TO MATCH THE ACS
            select(AGE7I, SEX7I, cg_educ_binary, cg_married, cg_inc_3cat, # demographics gen in acc_table1_new script
                   NHOUSE7I, # hh size
                   OO79LANGI) %>% # language used during interview
            mutate(
              # Ensure continuous variables are numeric
              AGE7I = as.numeric(AGE7I),
              NHOUSE7I = as.numeric(NHOUSE7I)
            ) %>%
            tbl_summary(
              type = list(
                "AGE7I" ~ "continuous",
                "NHOUSE7I" ~ "continuous"
              ),
              statistic = list(
                all_continuous() ~ "{mean}",
                all_categorical() ~ "{n} ({p}%)"
              ),
              label = list(
                AGE7I ~ "Age",
                SEX7I ~ "Female",
                cg_educ_binary ~ "Education",
                cg_married ~ "Married",
                cg_inc_3cat ~ "Household income (annual)",
                NHOUSE7I ~ "Household size", 
                OO79LANGI ~ "Language"
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
            modify_caption("**Table X. HEPESE vs ACS Caregiver Demographics: Adult Children (18+) Living with Aging Parents (80+) in AZ, CA, CO, NM, TX (2010–2011)**") 
        
        
#### SAVE 
write.csv(ACC_cg_coreside, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/ACC_cg_coreside.csv")
      
        
        
        
        
        
        
        
        
  #### ACS sample ---------------------------

    ## Restrict adult children only ---------------------------   
      # Otherwise the descriptives will reflect pooled data for children + parents (e.g., age will be skewed)  
      adult_children_acs <- acs_ma %>%  
          filter(RELATE == 3 & AGE >= 18) # n = 596

          # Weighted n 
            adult_children_acs_n <- sum(adult_children_acs$PERWT) # 70,976
            
          # Double-check sample ---------------------------  
            samp <- adult_children_acs %>%
              select(YEAR, SERIAL, PERNUM, STATEFIP, RELATE, RELATED, MOMLOC, POPLOC, NCHILD, 
                     SEX, AGE, MARST, married_dummy, RACE, HISPAN, EDUC, hs_binary, HHINCOME, inc_3cat, NUMPREC, FAMSIZE, PERWT)
     

  #### ACS TABLE ---------------------------
    adult_children_acs %>% # ALREADY CORESIDENTIAL, SINCE IT'S A HOUSEHOLD-BASED SURVEY
      mutate(
        SEX = haven::as_factor(SEX),   # Convert haven_labelled columns to factors (otherwise got warning)
        LANGUAGE = haven::as_factor(LANGUAGE)
      ) %>%
      as_survey_design(weight = PERWT) %>%
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
      modify_caption("**Table X. HEPESE vs ACS Caregiver Demographics: Adult Children (18+) Living with Aging Parents (80+) in AZ, CA, CO, NM, TX (2010–2011)**")


                        
#### SAVE 
write.csv(adult_children_acs, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/adult_children_acs.csv")
            
        