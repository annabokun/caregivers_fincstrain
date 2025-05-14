#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

  # "Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025
  
    # HEPESE & ACS sample comparison

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
    ddi <- read_ipums_ddi("usa_00014.xml")
      
      
  #### Use the IPUMS object to load its associated data ---------------------------
    acs <- read_ipums_micro(ddi)



  
#### SAVE 
write.csv(acs, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/acs.csv")









  #===============#
  #=== CLEANING ==#
  #===============#

    #### GOAL: Broadly compare Mexican-origin households with adult children (18+) AND their older parents (80+) in the ACS 
      # vs HEPESE (without the specific focus on caregivers) 
  

          #### Resources for working with IPUMS:
              # Working with Census microdata: https://walker-data.com/tidycensus/articles/pums-data.html 
              # Tables with weighted descriptives: https://tech.popdata.org/pma-data-hub/posts/2021-07-01-covid-tables/


      
  #### 1. Gen sample of Mexican-origin households with adult-children & parents ---------------------------

    ## 1a. Double-check ACS year and state match with HEPESE --------------------------- 
      table(acs$YEAR, acs$STATEFIP) # good 




    ## 1b. Generate unique HH identifier ---------------------------
        # SERIAL is only unique within each year
        # A combination of SAMPLE and SERIAL provides a unique identifier for every HH
      acs$hh_id <- paste(acs$SAMPLE, acs$SERIAL, sep = "_")


      

    ## 1c. Generate unique person identifier ---------------------------
        # A combination of SAMPLE, SERIAL, and PERNUM uniquely identifies every person in the database --> source: https://usa.ipums.org/usa-action/variables/SERIAL#description_section
      acs$person_id <- paste(acs$SAMPLE, acs$SERIAL, acs$PERNUM, sep = "_")
      
        # Relocate new variables to front for easier visibility
          acs <- acs %>%   
            relocate(hh_id, .after = SERIAL)
          acs <- acs %>%   
            relocate(person_id, .after = hh_id)
            

        # Verify new variables 
            # Count number of duplicate HH IDs (should match number of people per household)
              table(table(acs$hh_id))
        
            # Visual inspection
              # Look at all members of one HH
              test <- acs[acs$hh_id == "201001_207436", c("hh_id", "person_id", "PERNUM")]
              
            # Count unique person IDs
              length(unique(acs$person_id)) # 1,487,263 
                                            # Should match the number of rows (since each row is one person) -- good 
            
            # Check for duplicate person IDs
              sum(duplicated(acs$person_id)) # 0 -- good 
              
            # Cross-check SERIAL and PERNUM to verify that hh_id correctly groups individuals within the same HH
              test <- acs %>%
                group_by(hh_id) %>%
                summarise(unique_serial = n_distinct(SERIAL)) %>%
                filter(unique_serial > 1)  # If this returns 0 rows, each hh_id corresponds to only one SERIAL -- good
              
              
            # Check sample size 
              unique(length(acs$person_id))  # n = 1,487,263
                                             
              
              
              
    ## 1d. Filter for Mexican-origin ---------------------------
      # Origin is defined by the Census Bureau as ancestry, lineage, heritage, nationality group, or country of birth
      # People of Hispanic origin may be of any race; see RACE for a discussion of coding issues involved
        mex_hh <- acs %>%
          filter(HISPAN == 1) # Mexican (HISPAN==1 [Mexican] & RACHSING==5 [Latino] yields the same n)
                              
        unique(length(mex_hh$person_id)) # n = 405,979


        

        
        
    ## 1e. Create subset of HHs with adult children living with their older parents ---------------------------
        acs_mex <- mex_hh %>%
          group_by(hh_id) %>%              # Ensure in same HH 
          filter(GQ %in% 1:2 | GQ==5) %>%  # 1:2 (exclude group quarters); 5 (HHs w/10+ or individuals unrelated to the HOH)
          mutate(                                                                    
            mother_age = if_else(MOMLOC > 0, AGE[match(MOMLOC, PERNUM)], NA_real_),        # If MOMLOC > 0 (mother lives in same HH), then find her age; match(MOMLOC, PERNUM) finds the row where PERNUM equals MOMLOC; AGE[match(...)] gets the age of that person (the mother)
            father_age = if_else(POPLOC > 0, AGE[match(POPLOC, PERNUM)], NA_real_)) %>%    # Same for dads
          filter(
                any(AGE >= 18 & (mother_age >= 80 | father_age >= 80)) &                   # Ensure an adult child (18+) lives with a parent (80+)
                any(RELATE == 1 & (AGE >= 18 | mother_age >= 80 | father_age >= 80))) %>%  # Ensure either adult child OR parent is the HOH
          ungroup()                                                                        

        
        unique(length(acs_mex$person_id)) # n = 8,440


          
          

          
    ## 1f. Check sample size if I add a filter for unpaid family worker (i.e., proxy for family caregiver) ---------------------------
        # CLASSWKRD == 29 = unpaid family worker --> https://usa.ipums.org/usa-action/variables/CLASSWKR#codes_section
        unpaid_family_caregivers <- mex_hh %>%
            group_by(hh_id) %>%              # Ensure in same HH 
            filter(GQ %in% 1:2 | GQ==5) %>%  # 1:2 (exclude group quarters); 5 (HHs w/10+ or individuals unrelated to the HOH)s 
            mutate(                                                                              # Get parent ages for each person
              mother_age = if_else(MOMLOC > 0, AGE[match(MOMLOC, PERNUM)], NA_real_),            # If MOMLOC > 0 (mother lives in same HH), then find her age; If MOMLOC = 0 (no mother in household), assigns NA
              father_age = if_else(POPLOC > 0, AGE[match(POPLOC, PERNUM)], NA_real_)) %>%       # Same for dads
            filter(any(AGE >= 18 & CLASSWKRD == 29 & TRANWORK == 80 & (mother_age >= 80 | father_age >= 80)) %>%  # Ensure an adult child (18+) unpaid family caregiver lives with a parent (80+) 
                  any(RELATE == 1 & (AGE >= 18 | mother_age >= 80 | father_age >= 80))) %>%      # Ensure either adult child OR parent is the HOH
          ungroup()                                                                              
                     
            
            # n = 0 LOL 

                                                                                    
      #### ACS UNPAID FAMILY CAREGIVER NOTES ---------------------------  
          # CLASSWKRD code 29 for "unpaid family worker" in IPUMS typically refers to people working in a family business without pay, 
          # rather than providing family care. This might be capturing some caregivers, but could be missing many others who provide care 
          # but don't classify their caregiving as "work" in the traditional sense when responding to the ACS
            # Could further restrict by TRANSPORT to work (==home) to get closer to identifying caregiver for parent?


        
      ### IPUMS note from Ivan ---------------------------
        # I highly recommend reviewing the detailed documentation pages for MOMLOC/POPLOC to determine if you would like to leverage this additional information. 
        # Note that these parental links are used for our attach characteristics tool (available in the last screen before submit your extracts). 
        # Using this tool, you can use the variables in your data cart to add data about a person’s mother, father, household head, or spouse as an additional column in your data extract. 
        # For example, you can append mother’s and father’s age to the person’s record for analysis
        
          
        
#### SAVE 
write.csv(mex_hh, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/mex_hh.csv")
write.csv(acs_mex, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/acs_mex.csv")
write.csv(unpaid_family_caregivers, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/unpaid_family_caregivers.csv")

        





        
  #====================#
  #=== ACS CLEANING ===#
  #====================#
  
    #### 2. ACS Cleaning ---------------------------   

    #### 2a. Collapse EDUC into HS/No HS categories ---------------------------
        acs_mex <- acs_mex %>%
          mutate(
            hs_binary = case_when(
              EDUC %in% 0:5 ~ "Less than High School",
              EDUC %in% 6:11 ~ "High School or More",
              TRUE ~ NA_character_)) # Handle missing values

        # Weighted proportions for HS/No HS
          acs_mex_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_mex)
          hs_distribution <- svymean(~factor(hs_binary), acs_mex_svy, na.rm = TRUE)
          print(hs_distribution)
        
        
        
        
        
  #### 2b. Collapse HH income ($20k binary) ---------------------------   
        summary(acs_mex$HHINCOME)
        
        # Create 3-category income variable
        acs_mex <- acs_mex %>%
          mutate(
            inc_3cat = case_when(
              HHINCOME < 20000 ~ "< $20,000",
              HHINCOME >= 20000 ~ "$20,000+",
              HHINCOME == 9999999 ~ "NA",
              is.na(HHINCOME) ~ "Missing",
              TRUE ~ NA_character_))

        # Weighted proportions 
          acs_mex_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_mex)
          income_cat_pct <- svymean(~factor(inc_3cat), acs_mex_svy, na.rm = TRUE)
          print(income_cat_pct)
        
        table(acs_mex$HHINCOME)
        table(acs_mex$inc_3cat)
        
        
        # Missing income
        missing_inc <- sum(is.na(acs_mex$HHINCOME)) / nrow(acs_mex) * 100
        
        print(paste("Percentage of missing values:", round(missing_percentage, 2), "%"))
        
        

        
  #### 2c. Collapse married ---------------------------   
        acs_mex <- acs_mex %>%
          mutate(
            married_dummy = case_when(
              MARST %in% c(1, 2) ~ 1,  # Married (spouse present or absent)
              MARST %in% c(3, 4, 5, 6) ~ 0,  # Not married (separated, divorced, widowed, never married)
              TRUE ~ NA_real_))  # Handle missing values

        # Weighted proportions 
          acs_mex_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_mex)
          married_pct <- svymean(~factor(married_dummy), acs_mex_svy, na.rm = TRUE)
          print(married_pct)

            # Or, can calculate weighted percentages this way too
              married_pct <- svymean(~married_dummy, acs_mex_svy, na.rm = TRUE) * 100    
            
    
      
              
  #### 2d. Visually inspect sample ---------------------------  
        acs_mex_samp <- acs_mex %>%
          select(YEAR, SERIAL, PERNUM, STATEFIP, SEX, AGE, RELATE, MOMLOC, POPLOC, NCHILD, 
                 MARST, married_dummy, RACE, HISPAN, EDUC, hs_binary, HHINCOME, inc_3cat, NUMPREC, FAMSIZE, GQ, PERWT)
        
            
                

#### SAVE 
write.csv(acs_mex, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/acs_mex.csv")
  saveRDS(acs_mex, file = "acs_mex.rds") 
write.csv(acs_mex_samp, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/acs_mex_samp.csv")

        


        
  #============================#
  #=== COMPARE HEPESE & ACS ===#
  #============================#
        
    #### 3. Set-up HEPESE sample ---------------------------
    ## acc_hepese df generated in the 01_acc_clean script (RELSUBJ7I==3 | RELSUBJ7I==4)


      ## 3a. Gen co-residential sample ---------------------------
        cg_coreside_HEPESE <- acc_hepese %>%
          filter(HOUSEK_1==1) # n = 191


      ## 3b.) Gen HEPESE desc table ---------------------------

      # Set compact theme for gtsummary
      theme_gtsummary_compact()
      
      
      # HEPESE table
      ACC_full_imputed %>%  # MUST BE CO-RESIDENTIAL SAMPLE TO MATCH THE ACS
        select(AGE7I, SEX7I, cg_educ_binary, cg_married, cg_inc_binary, # demographics 
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
            cg_inc_binary ~ "Household income (annual)",
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
        modify_caption("**HEPESE vs ACS Caregiver Demographics: Adult Children (18+) Living with Aging Parents (80+) in AZ, CA, CO, NM, TX (2010–2011)**") 
      
        
    
#### SAVE 
write.csv(cg_coreside_HEPESE, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/cg_coreside_HEPESE.csv")


        
  #================================================#
  #=== ACS TABLE: MEXICAN-ORIGIN ADULT CHILDREN ===#
  #================================================#


  
  #### 4. Set-up ACS sample ---------------------------

    ## 4a. Get just the adult children (not their parents or other HH members) ---------------------------   
      # Otherwise the descriptives will reflect pooled data for children + parents (e.g., age will be skewed)  
      adult_children_acs_mex <- acs_mex %>%
        filter(AGE >= 18 & (mother_age >= 80 | father_age >= 80)) # Keep only adults who have at least one elderly parent in the household

      unique(length(adult_children_acs$person_id)) # n = 2,696

            
          # Visually inspect sample ---------------------------  
            samp_kids <- adult_children_acs_mex %>%
              select(YEAR, SERIAL, PERNUM, STATEFIP, RELATE, RELATED, MOMLOC, POPLOC, NCHILD, 
                     SEX, AGE, mother_age, father_age, MARST, married_dummy, RACE, HISPAN, EDUC, hs_binary, HHINCOME, inc_3cat, NUMPREC, FAMSIZE, PERWT)

         table(adult_children_acs_mex$LANGUAGE)
      
            
    ## 4b. TABLE (weighted) ---------------------------
      adult_children_acs_mex %>% 
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
          modify_caption("**HEPESE vs ACS Caregiver Demographics: Adult Children (18+) Living with Aging Parents (80+) in AZ, CA, CO, NM, TX (2010–2011)**")


            
#### SAVE 
write.csv(adult_children_acs_mex, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/adult_children_acs_mex.csv")
            
    




  #### Extra ACS desc checks --------------------------- 

    ## 1. Basic age statistics
          # ACS
            hist(adult_children_acs_mex$AGE,
                 main = "ACS: adult children age distribution") 
            
          # HEPESE
            hist(ACC$AGE7I,
                 main = "HEPESE: adult children age distribution")  

        adult_children_acs_mex %>%
          as_survey_design(weight = PERWT) %>%
          summarise(
            mean_age = survey_mean(AGE),
            min_age = min(AGE),
            max_age = max(AGE)
          )
        
    ## 2. Age groups distribution
          adult_children_acs_mex %>%
            mutate(
              age_group = case_when(
                AGE >= 18 & AGE < 25 ~ "18-24",
                AGE >= 25 & AGE < 35 ~ "25-34",
                AGE >= 35 & AGE < 45 ~ "35-44",
                AGE >= 45 & AGE < 55 ~ "45-54",
                AGE >= 55 & AGE <= 70 ~ "55-70"
              )
            ) %>%
            as_survey_design(weight = PERWT) %>%
            group_by(age_group) %>%
            summarise(
              n_unweighted = n(),
              n_weighted = sum(PERWT),
              pct = n_weighted/sum(n_weighted)*100
            ) %>%
            arrange(age_group)
        
        # A tibble: 5 × 4
          # age_group n_unweighted n_weighted   pct
          # <chr>            <int>      <dbl> <dbl>
          #  1 18-24              350      40875   100
          #  2 25-34              188      22353   100
          #  3 35-44               43       5035   100
          #  4 45-54               13       2525   100
          #  5 55-70                2        188   100
                        
            
    ## 3. Check the Distribution of MARST
          adult_children_acs_mex %>%
            as_survey_design(weights = PERWT) %>%
            group_by(MARST) %>%
            summarise(
              count = survey_total(),
              percent = survey_mean()
            )
          
    ## 4. Check the married_dummy calculation
          table(acs_mex$MARST)
          table(acs_mex$married_dummy)
          
          adult_children_acs_mex %>%
            as_survey_design(weights = PERWT) %>%
            summarise(
              married_pct = survey_mean(married_dummy, na.rm = TRUE) * 100
            )
          
          
          
  ## 5. Descriptive: Spanish speakers x place of birth 
          
          # Filter for Spanish speakers (LANGUAGE == 12)
          spanish_speakers <- subset(adult_children_acs_mex, LANGUAGE == 12)
          write.csv(spanish_speakers, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/spanish_speakers.csv")
          
          
          # Recode BPL values
          spanish_speakers$BPL <- factor(spanish_speakers$BPL,
                                         levels = c(200, 48, 6, 4, 35, 8),
                                         labels = c("Mexico", "Texas", "California", "Arizona", "New Mexico", "Colorado"))
          
          # Calculate %  by place of birth
          bpl_counts <- prop.table(table(spanish_speakers$BPL)) * 100
          
          # Convert to data frame
          bpl_df <- data.frame(Place_of_Birth = names(bpl_counts), Percentage = as.vector(bpl_counts))
          
          # Table 
          library(knitr)
          kable(bpl_df, col.names = c("Place of Birth", "Percentage of Spanish Speakers"), digits = 2)          
