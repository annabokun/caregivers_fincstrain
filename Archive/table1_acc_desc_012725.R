#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

    # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
    # PAA 2025
  
    # TABLE 1: ACCs: Demographic, Health, and Caregiving Characteristics 

# Script created in RStudio ("Cranberry Hibiscus")
# updated 1/13/2025
#=======================================================================================================#


  #==============#
  #=== SET-UP ===#
  #==============#
  
  
  #### Load packages
    library(tidyverse)
    library(gtsummary)
    library(flextable)
    library(smd)
  
  
  #### Set working directory 
    setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files")
  
  
  
  #### Load df (see acc_table1 for data source & cleaning)
    # ACC
  
  
  #================#
  #=== CLEANING ===#
  #================# 
    
    ## Married ---------------------------
      ACC$cg_married <- ifelse(ACC$CMARSTAT7I_1 == 1, 1,
                               ifelse(ACC$CMARSTAT7I_1 == 9 | is.na(ACC$CMARSTAT7I_1), NA, 0))
      table(ACC$cg_married) #352
    
      
            ## Collapse to married, never married, previously married
              ACC <- ACC %>%
                mutate(cg_marital_cat = case_when(
                  CMARSTAT7I_1 == 1 ~ "Married",
                  CMARSTAT7I_1 == 5 ~ "Never married",
                  CMARSTAT7I_1 %in% c(2, 3, 4) ~ "Previously married",
                  CMARSTAT7I_1 == 9 ~ NA_character_,  # Assign NA for "Do not know/refused"
                  TRUE ~ NA_character_  # Assign NA for missing values
                ))
              
              # Check the distribution
              table(ACC$cg_marital_cat, useNA = "ifany")
      
      
    ## Education ---------------------------
      
        # Create categorical education variable
          ACC$cg_educ_cat <- cut(ACC$CEDUC7I_1, 
                                 breaks = c(-Inf, 11, 12, 15, 16, Inf),  # Keep all breaks
                                 labels = c("Less than HS", 
                                            "HS Graduate",
                                            "Some College",
                                            "Bachelor's+",  # Combined last two categories into one label
                                            "Bachelor's+"), # Same label repeated
                                 right = TRUE)
          
          table(ACC$cg_educ_cat)  # Less than HS: 230 
          # HS: 183 
          # Some college: 129
          # BA+: 105
          
          
          
        # Create binary education variable
          ACC$cg_educ_binary <- cut(ACC$CEDUC7I_1,
                                    breaks = c(-Inf, 11, Inf), # Just one break at 11 years
                                    labels = c("Less than HS", "HS or more"),
                                    right = TRUE)
          
          table(ACC$cg_educ_binary) # Should show Less than HS: 230, HS or more: 417 (183+129+105)
      

      
    ## HH income --------------------------- 
      
      # $20k binary
      ACC <- ACC %>%
        mutate(
          cg_inc_binary = case_when(
            LL3A7I %in% c(1,2,3,4) ~ "< $20,000",
            LL3A7I %in% c(5,6,7,8) ~ "$20,000+",
            is.na(LL3A7I) | LL3A7I == 9 ~ "Missing", # Assuming 9 is missing code
            TRUE ~ NA_character_
          )
        )
      
      # Verify the categories
      table(ACC$cg_inc_binary, useNA = "always")
      
      

         
    
    ## HOH ---------------------------
      # HHREL7I: Informant's relationship to head of household
      ACC$cg_HOH <- ifelse(ACC$HHREL7I == 1, 1,
                           ifelse(ACC$HHREL7I == 98 | ACC$HHREL7I == 99 | is.na(ACC$HHREL7I), NA, 0))
      table(ACC$cg_HOH) #287
    
    
    ## Living with parent/co-residing ---------------------------
      ACC$cg_coreside <- ifelse(ACC$HOUSEK_1 == 1, 1,
                                ifelse(is.na(ACC$HOUSEK_1), NA, 0))
      table(ACC$cg_coreside) #191

    
    
    ## Self-rated health: excellent/good ---------------------------
      ACC$cg_health_xg <- ifelse(ACC$I_HEALTH7I %in% 1:2, 1,
                                 ifelse(ACC$I_HEALTH7I == 8 | ACC$I_HEALTH7I == 9 | is.na(ACC$I_HEALTH7I), NA, 0))
      table(ACC$cg_health_xg) #388
    
    
    ## Self-rated health: fair/poor ---------------------------
      ACC$cg_health_fp <- ifelse(ACC$I_HEALTH7I %in% 3:4, 1,
                                 ifelse(ACC$I_HEALTH7I == 8 | ACC$I_HEALTH7I == 9 | is.na(ACC$I_HEALTH7I), NA, 0))
      table(ACC$cg_health_fp) #267
    
    
    ## Self-rated health: categorical combining excellent/good + fair/poor --------------------------- 
      ACC$cg_health <- factor(
        ifelse(ACC$I_HEALTH7I %in% 1:2, "excellent/good",
               ifelse(ACC$I_HEALTH7I %in% 3:4, "fair/poor",
                      ifelse(ACC$I_HEALTH7I %in% 8:9, NA, NA))),
        levels = c("excellent/good", "fair/poor"))
      
      table(ACC$cg_health) #excellent/good: 388; fair/poor: 267
      
            # Convert to binary variable
            ACC <- ACC %>%
              mutate(cg_health_binary = case_when(
                cg_health == "fair/poor" ~ 1,
                cg_health == "excellent/good" ~ 0,
                TRUE ~ NA_real_  # Assigns NA for missing or unexpected values
              ))
            
            table(ACC$cg_health_binary) # 1 = 267; 0 = 388
    
    
    ## Arthritis ---------------------------
      ACC$cg_arth <- ifelse(ACC$I_ARTHRHEU7I == 1, 1,
                            ifelse(ACC$I_ARTHRHEU7I == 8 | ACC$I_ARTHRHEU7I == 9 | is.na(ACC$I_ARTHRHEU7I), NA, 0))
      table(ACC$cg_arth) #196
    
    
    ## Cardio disease ---------------------------
      ACC$cg_heart_probs <- ifelse(ACC$cg_heart_problems == 1, 1,
                                   ifelse(is.na(ACC$cg_heart_problems), NA, 0))
      table(ACC$cg_heart_probs, useNA = "ifany") #265 
      
            # where is cg_heart_problems??? 
      
      
      
      ACC <- ACC %>%
        mutate(
          cg_heart_problems = case_when(
            I_ICARDI71I == 1 | I_ICARDI75I == 1 |
              I_JSTROK71I == 1 | I_JSTROK75I == 1 |
              I_KHYPER71I == 1  | I_KHYPER74I == 1 ~ 1,
            
            I_ICARDI71I %in% c(2) & I_ICARDI75I %in% c(2) &
              I_JSTROK71I %in% c(2) & I_JSTROK75I %in% c(2) &
              I_KHYPER71I %in% c(2) & I_KHYPER74I %in% c(2) ~ 0,
            
            TRUE ~ NA_real_  # assign NA if mixed or missing info
          )
        )
      
      table(ACC$cg_heart_problems)

      
      
    
    
    ## Diabetes ---------------------------
      ACC$cg_diab <- ifelse(ACC$I_MDIAB71I == 1, 1,
                            ifelse(ACC$I_MDIAB71I == 8 | ACC$I_MDIAB71I == 9 | is.na(ACC$I_MDIAB71I), NA, 0))
      table(ACC$cg_diab) #124
    
    
    ## Cancer ---------------------------
      ACC$cg_cancer <- ifelse(ACC$I_LCANCR71I == 1, 1,
                              ifelse(ACC$I_LCANCR71I == 8 | ACC$I_LCANCR71I == 9 | is.na(ACC$I_LCANCR71I), NA, 0))
      table(ACC$cg_cancer) #29
  
      
    ## Any chronic condition --------------------------- 
      ACC$cg_chronic_any <- ifelse(ACC$cg_arth==1 | ACC$cg_heart_probs==1 | ACC$cg_diab==1 | ACC$cg_cancer==1, 1, 0)
      table(ACC$cg_chronic_any) # 1 = 383; 0 = 1 
      
      table(ACC$cg_chronic_any, ACC$cg_finc_strain)
      
    
    ## Felt sad: Rarely/some ---------------------------
      ACC$cg_sad_rare <- ifelse(ACC$X7CESD6I %in% 0:1, 1,
                                ifelse(ACC$X7CESD6I == 8 | is.na(ACC$I_LCANCR71I), NA, 0))
      table(ACC$cg_sad_rare) #632
    
      
    
    ## Felt sad: Moderate/most ---------------------------
      ACC$cg_sad_most <- ifelse(ACC$X7CESD6I %in% 2:3, 1,
                                ifelse(ACC$X7CESD6I == 8 | is.na(ACC$I_LCANCR71I), NA, 0))
      table(ACC$cg_sad) #25
    
      
    
    ## Feels sad: categorical combining rarely/some + moderate/most ---------------------------
      ACC$cg_sad <- factor(
        ifelse(ACC$X7CESD6I %in% 0:1, "rarely/some",
               ifelse(ACC$X7CESD6I %in% 2:3, "moderate/most",
                      ifelse(ACC$X7CESD6I == 8, NA, NA))),
        levels = c("rarely/some", "moderate/most"))
      
      table(ACC$cg_sad) #rarely/some: 632; moderate/most: 25
      
          # Convert to binary variable
            ACC <- ACC %>%
              mutate(cg_sad_binary = case_when(
                cg_sad == "moderate/most" ~ 1,
                cg_sad == "rarely/some" ~ 0,
                TRUE ~ NA_real_  # Assigns NA for missing or unexpected values
              ))
      
            table(ACC$cg_sad_binary) # 1 = 25; 0 = 632
          
    
      
    
    ## ADL: Hours/day caregiving ---------------------------
      # Create new variable for average care hours
      ACC$cg_hours_adl <- ACC$RR3HRS7I
      
      # Handle special codes
      ACC$cg_hours_adl[ACC$cg_hours_adl == 97] <- 0.5  # "Less than 1 hr" = 0.5
      ACC$cg_hours_adl[ACC$cg_hours_adl >= 98] <- NA   # Set unknown/refused to NA
      
      # Check first few rows to verify
      head(ACC[, c("RR3HRS7I", "cg_hours_adl")])
      
      # Get summary of the new variable
      summary(ACC$cg_hours_adl)

      
    ## IADL: Hours/day caregiving --------------------------- 
      # Create new variable for average care hours
      ACC$cg_hours_iadl <- ACC$RR5HRS7I
      
      # Handle special codes
      ACC$cg_hours_iadl[ACC$cg_hours_iadl == 97] <- 0.5  # "Less than 1 hr" = 0.5
      ACC$cg_hours_iadl[ACC$cg_hours_iadl >= 98] <- NA   # Set unknown/refused to NA
      
      # Check first few rows to verify
      head(ACC[, c("RR5HRS7I", "cg_hours_iadl")])
      
      # Get summary of the new variable
      summary(ACC$cg_hours_iadl)
    
    
    
    ## Years/caregiving ---------------------------
      ACC$cg_yrs_care <- factor(
        ifelse(ACC$RR7START7I %in% c(1, 2), "1 year or less",
               ifelse(ACC$RR7START7I == 3, "1-2 years",
                      ifelse(ACC$RR7START7I %in% c(4, 7), "3-5 years", # put "other" under 3-5, since most common
                             ifelse(ACC$RR7START7I %in% c(5, 6), "6+ years",
                                    ifelse(ACC$RR7START7I %in% c(8, 9), NA, NA))))),
        levels = c("1 year or less", "1-2 years", "3-5 years", "6+ years"))
      
      table(ACC$cg_yrs_care) #29
    
    
    ## Dementia CR --------------------------- 
      ACC$cr_dementia <- case_when(
        # Definite dementia cases
        ACC$RR6ALZ7I == 1 ~ 1,                    # Has Alzheimer's
        ACC$TOTMMSE7 < 18 ~ 1,                    # Low cognitive score
        ACC$PRXMENT7 == 4 ~ 1,                    # Need proxy for survey
        
        # Missing cases
        ACC$RR6ALZ7I %in% c(8, 9) ~ NA_real_,     # Don't know/Refused
        is.na(ACC$RR6ALZ7I) & 
          is.na(ACC$TOTMMSE7) & 
          is.na(ACC$PRXMENT7) ~ NA_real_,         # All indicators missing
        
        # No dementia (only if we have at least one valid negative indicator)
        TRUE ~ 0
      )
      
      
          ## Binary 
            ACC$cr_dementia_binary <- case_when(
              # Definite dementia cases
              ACC$RR6ALZ7I == 1 ~ 1,                    # Has Alzheimer's
              ACC$TOTMMSE7 < 18 ~ 1,                    # Low cognitive score
              ACC$PRXMENT7 == 4 ~ 1,                    # Need proxy for survey
              
              # No dementia (all other cases)
              TRUE ~ 0
            )
      
      
      
      
      # Check results
      table(ACC$cr_dementia) # 1 = 273
      table(ACC$cr_dementia_binary)
      
      summary_stats <- data.frame(
        Total = sum(!is.na(ACC$cr_dementia)),
        Missing = sum(is.na(ACC$cr_dementia)),
        Dementia = sum(ACC$cr_dementia == 1, na.rm = TRUE),
        No_Dementia = sum(ACC$cr_dementia == 0, na.rm = TRUE)
      )
      print(summary_stats)
      
      
      
      # Cross-tab with financial strain
        table(ACC$cr_dementia, ACC$cg_finc_strain, useNA = "always")
    
    
    
    
#### SAVE 
write.csv(ACC, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/ACC.csv")

      
    
  #===============#
  #=== TABLE 1 ===#
  #===============#
    
    # Set compact theme for gtsummary
    theme_gtsummary_compact()
    
    
    # TABLE 1 
    # Run without table1_acc <- if want to see in the right hand side of the console immediately, otherwise it will save to Word
      table1_acc <- ACC %>% 
      select(cg_finc_strain, AGE7I, SEX7I, cg_educ_binary, cg_married, cg_inc_binary, # demographics
             cg_coreside, cg_HOH, NHOUSE7I, # living arrangements
             cg_health, cg_chronic_any, cg_arth, cg_heart_probs, cg_diab, cg_cancer, # physical health 
             cg_sad, cg_health_util, # mental health + util
             cg_hours_adl, cg_hours_iadl, cg_yrs_care, cr_dementia) %>% # caregiving intensity
      mutate(
        # Ensure continuous variables are numeric
        AGE7I = as.numeric(AGE7I),
        NHOUSE7I = as.numeric(NHOUSE7I),
        cg_hours_adl = as.numeric(cg_hours_adl),
        cg_hours_iadl = as.numeric(cg_hours_iadl),
        # Create factor with specific level order for financial strain
        cg_finc_strain = factor(cg_finc_strain, 
                                levels = c(1, 0),
                                labels = c("Financial strain", "No financial strain"))
      ) %>%
      tbl_summary(
        by = cg_finc_strain,
        type = list(
          "AGE7I" ~ "continuous",
          "NHOUSE7I" ~ "continuous",
          "cg_hours_adl" ~ "continuous",
          "cg_hours_iadl" ~ "continuous"
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
          cg_coreside ~ "Co-reside with parent",
          cg_HOH ~ "Caregiver - head of household",
          NHOUSE7I ~ "Household size", 
          cg_health ~ "Self-rated health", 
          cg_chronic_any ~ "Any chronic condition",
          cg_arth ~ "Arthritis",
          cg_heart_probs ~ "Cardiovascular disease",
          cg_diab ~ "Diabetes",
          cg_cancer ~ "Cancer",
          cg_sad ~ "Felt sad",
          cg_health_util ~ "Healthcare utilization",
          cg_hours_adl ~ "ADL hours/day",
          cg_hours_iadl ~ "IADL hours/day",
          cg_yrs_care ~ "Years caregiving",
          cr_dementia ~ "Care recipient has dementia"
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
      add_overall(col_label = "**Total Sample**, N = 659") %>%
      modify_table_styling(
        columns = starts_with("stat_")
      ) %>%
      add_p(
        test = list(
          all_continuous() ~ "aov",     # ANOVA for continuous variables
          all_categorical() ~ "chisq.test"  # Chi-square for categorical variables
        ),
        pvalue_fun = ~ style_pvalue(.x, digits = 3)
      ) %>%
      modify_caption("**Table 1. Adult Children Caregivers: Demographic, Health, and Caregiving Characteristics**") 
      
      print(table1_acc)
      
      # Add to save to Word
        %>%
        as_flex_table() %>%
      save_as_docx(path = "table1_acc.docx")