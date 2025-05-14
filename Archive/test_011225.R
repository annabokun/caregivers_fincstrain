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
  
  

  
  #===============#
  #=== TABLE 1 ===#
  #===============#
  
  #### TABLE 1: Demographic and Health Characteristics by ACC Financial Strain --------------------------- 
  
  ### A.) Generate w/financial strain and w/o financial strain samples 
  fs <- ACC %>%
    filter(cg_finc_strain==1)
  table(fs$cg_finc_strain) # n = 77
  
  no_fs <- ACC %>%
    filter(cg_finc_strain==0)
  table(no_fs$cg_finc_strain) # n = 582
  
  
  ## 1.) Age --------------------------- 
  summary(ACC$AGE7I)
  hist(ACC$AGE7I)
  
  # By financial strain 
  summary(fs$AGE7I)
  summary(no_fs$AGE7I)
  
  table()
  
  
  
  ## 2.) Female % ---------------------------   
  # 1 = male; 2 = female
  cbind(Frequency = table(ACC$SEX7I, useNA = "ifany"),
        Percentage = round(prop.table(table(ACC$SEX7I, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$SEX7I, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$SEX7I, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$SEX7I, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$SEX7I, useNA = "ifany")) * 100, 2))
  
  # Chi-squared test 
  tab <- table(ACC$SEX7I, ACC$cg_finc_strain)
  chisq.test(tab)
  
  
  
  ## 3.) Highest education --------------------------- 
  # 3a.) Create categorical education variable
  ACC$cg_educ_cat <- cut(ACC$CEDUC7I_1, 
                         breaks = c(-Inf, 11, 12, 15, 16, Inf),  # Keep all breaks
                         labels = c("Less than HS", 
                                    "HS Graduate",
                                    "Some College",
                                    "Bachelor's+",  # Combined last two categories into one label
                                    "Bachelor's+"), # Same label repeated
                         right = TRUE)
  
  # 3b.) Check distribution
  cbind(
    Frequency = table(ACC$cg_educ_cat, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$cg_educ_cat, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$cg_educ_cat, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$cg_educ_cat, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$cg_educ_cat, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$cg_educ_cat, useNA = "ifany")) * 100, 2))
  
  # Chi-squared test 
  tab <- table(ACC$cg_educ_cat, ACC$cg_finc_strain)
  chisq.test(tab)
  
  
  
  
  
  ## 4.) Married % ---------------------------  
  cbind(
    Frequency = table(ACC$CMARSTAT7I_1, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$CMARSTAT7I_1, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$CMARSTAT7I_1, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$CMARSTAT7I_1, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$CMARSTAT7I_1, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$CMARSTAT7I_1, useNA = "ifany")) * 100, 2))
  
  
  ## 5.) Employed ---------------------------  
  
  ## 6.) Unemployed ---------------------------  
  
  ## 7.) Disabled --------------------------- 
  
  ## 8.) Retired ---------------------------  
  
  
  
  ## 10.) HH income ---------------------------    
  table(ACC$LL3A7I)  
  hist(ACC$LL3A7I)  
  
  # 10a.)  Create categorical income variable
  ACC <- ACC %>%
    mutate(
      cg_inc_cat = case_when(
        LL3A7I %in% c(1,2) ~ "< $10,000",
        LL3A7I %in% c(3,4) ~ "$10,000-$19,999",
        LL3A7I == 5 ~ "$20,000-$29,999",
        LL3A7I %in% c(6,7) ~ "$30,000-$49,999",
        LL3A7I == 8 ~ "$50,000+",
        TRUE ~ NA_character_
      )
    )
  
  table(ACC$cg_inc_cat)
  
  
  
  # Check distribution
  cbind(
    Frequency = table(ACC$cg_inc_cat, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$cg_inc_cat, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$cg_inc_cat, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$cg_inc_cat, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$cg_inc_cat, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$cg_inc_cat, useNA = "ifany")) * 100, 2))
  
  
  ## 11.) Living arrangement --------------------------- 
  
  # 11a.) Alone --------------------------- 
  cbind(Frequency = table(ACC$NHOUSE7I, useNA = "ifany"),
        Percentage = round(prop.table(table(ACC$NHOUSE7I, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$NHOUSE7I, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$NHOUSE7I, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$NHOUSE7I, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$NHOUSE7I, useNA = "ifany")) * 100, 2))
  
  
  # 11b.) With parent --------------------------- 
  cbind(
    Frequency = table(ACC$HOUSEK_1, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$HOUSEK_1, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$HOUSEK_1, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$HOUSEK_1, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$HOUSEK_1, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$HOUSEK_1, useNA = "ifany")) * 100, 2))
  
  
  # 11c.) HOH --------------------------- 
  cbind(
    Frequency = table(ACC$HHREL7I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$HHREL7I, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$HHREL7I, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$HHREL7I, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$HHREL7I, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$HHREL7I, useNA = "ifany")) * 100, 2))  
  
  
  ## 11d.) HH size ---------------------------  
  summary(ACC$NHOUSE7I) # 3.241
  
  # Financial strain 
  summary(fs$NHOUSE7I)
  summary(no_fs$NHOUSE7I)
  
  
  
  ## 11e.) CG moved in because they need a place to stay --------------------------- 
  # PLASTAY7
  
  table(ACC$PLASTAY7) # yes = 38
  
  cbind(Frequency = table(ACC$PLASTAY7, useNA = "ifany"),
        Percentage = round(prop.table(table(ACC$PLASTAY7, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$PLASTAY7, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$PLASTAY7, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$PLASTAY7, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$PLASTAY7, useNA = "ifany")) * 100, 2))    
  
  
  
  ## 12.) Self-rated health ---------------------------  
  cbind(Frequency = table(ACC$I_HEALTH7I, useNA = "ifany"),
        Percentage = round(prop.table(table(ACC$I_HEALTH7I, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$I_HEALTH7I, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$I_HEALTH7I, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$I_HEALTH7I, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$I_HEALTH7I, useNA = "ifany")) * 100, 2))   
  
  
  
  ## 13.) Chronic conditions ---------------------------  
  
  # 13a.) Arthritis ---------------------------   
  cbind(
    Frequency = table(ACC$I_ARTHRHEU7I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$I_ARTHRHEU7I, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$I_ARTHRHEU7I, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$I_ARTHRHEU7I, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$I_ARTHRHEU7I, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$I_ARTHRHEU7I, useNA = "ifany")) * 100, 2))  
  
  
  # 13b.) Cardiovascular ---------------------------   
  # Stroke
  cbind(
    Frequency = table(ACC$I_JSTROK71I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$I_JSTROK71I, useNA = "ifany")) * 100, 2))
  
  # Hospitalized for stroke
  cbind(
    Frequency = table(ACC$I_JSTROK75I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$I_JSTROK75I, useNA = "ifany")) * 100, 2))
  
  # Heart attack
  cbind(
    Frequency = table(ACC$I_ICARDI71I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$I_ICARDI71I, useNA = "ifany")) * 100, 2))     
  
  # Hospitalized for heart attack
  cbind(
    Frequency = table(ACC$I_ICARDI75I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$I_ICARDI75I, useNA = "ifany")) * 100, 2))
  
  # Hypertension
  cbind(
    Frequency = table(ACC$I_KHYPER71I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$I_KHYPER71I, useNA = "ifany")) * 100, 2))
  
  # Meds for hypertension
  cbind(
    Frequency = table(ACC$I_KHYPER74I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$I_KHYPER74I, useNA = "ifany")) * 100, 2))
  
  
  # Gen dummy for cardiovascular disease   
  ACC$cg_heart_problems <- ifelse(ACC$I_JSTROK71I==1 | #stroke
                                    ACC$I_JSTROK75I==1 | #stroke hospital
                                    ACC$I_ICARDI71I==1 | #ha
                                    ACC$I_ICARDI75I==1 | #ha hospital
                                    ACC$I_KHYPER71I==1 | #hyper
                                    ACC$I_KHYPER74I==1, 1, 0) #meds for hyper
  
  table(ACC$cg_heart_problems) #265
  
  cbind(Frequency = table(ACC$cg_heart_problems, useNA = "ifany"),
        Percentage = round(prop.table(table(ACC$cg_heart_problems, useNA = "ifany")) * 100, 2))
  
  
  # RENEGATE FS AND NO_FS SAMPLES 
  
  
  # Financial strain
  cbind(Frequency = table(fs$cg_heart_problems, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$cg_heart_problems, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$cg_heart_problems, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$cg_heart_problems, useNA = "ifany")) * 100, 2))  
  
  
  
  
  ## 13c.) Diabetes ---------------------------  
  cbind(
    Frequency = table(ACC$I_MDIAB71I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$I_MDIAB71I, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$I_MDIAB71I, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$I_MDIAB71I, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$I_MDIAB71I, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$I_MDIAB71I, useNA = "ifany")) * 100, 2))  
  
  
  ## 13d.) Cancer --------------------------- 
  cbind(
    Frequency = table(ACC$I_LCANCR71I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$I_LCANCR71I, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$I_LCANCR71I, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$I_LCANCR71I, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$I_LCANCR71I, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$I_LCANCR71I, useNA = "ifany")) * 100, 2)) 
  
  # Chi-squared test 
  tab <- table(ACC$I_LCANCR71I, ACC$cg_finc_strain)
  chisq.test(tab)
  print(tab)
  
  # Using Fisher's exact test:
  tab <- table(ACC$I_LCANCR71I, ACC$cg_finc_strain)
  fisher.test(tab)
  
  
  # ANY chronic condition ---------------------------  
  ACC$cg_chronic_any <- ifelse(ACC$I_ARTHRHEU7I==1 | ACC$cg_heart_problems==1 | ACC$I_MDIAB71I==1 | ACC$I_LCANCR71I==1, 1, 0)
  
  table(ACC$cg_chronic_any)
  
  cbind(
    Frequency = table(ACC$chronic_any, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$chronic_any, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$chronic_any, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$chronic_any, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$chronic_any, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$chronic_any, useNA = "ifany")) * 100, 2)) 
  
  
  # Chi-squared test 
  tab <- table(ACC$chronic_any, ACC$cg_finc_strain)
  chisq.test(tab)
  
  # Using Fisher's exact test:
  tab <- table(ACC$chronic_any, ACC$cg_finc_strain)
  fisher.test(tab)
  
  
  
  ## 14.) Felt sad ---------------------------  
  cbind(
    Frequency = table(ACC$X7CESD6I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$X7CESD6I, useNA = "ifany")) * 100, 2)) 
  
  # Financial strain
  cbind(Frequency = table(fs$X7CESD6I, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$X7CESD6I, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$X7CESD6I, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$X7CESD6I, useNA = "ifany")) * 100, 2)) 
  
  
  ## 15.) Healthcare utilization --------------------------- 
  table(ACC$CC1A7I, ACC$KK27I)
  
  ACC$cg_health_util <- case_when(
    ACC$CC1A7I == 1 | ACC$KK27I >= 12 ~ "High",     # Any hospital stay (yes) or monthly+ visits
    ACC$KK27I > 0 & ACC$CC1A7I == 2 ~ "Moderate",   # Some doctor visits but no hospital stay
    ACC$KK27I == 0 & ACC$CC1A7I == 2 ~ "None")      # No visits and no hospital stay
  
  
  # Convert to factor with ordered levels
  ACC$cg_health_util <- factor(ACC$cg_health_util, 
                            levels = c("None", "Moderate", "High"))
  
  cbind(
    Frequency = table(ACC$cg_health_util, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$cg_health_util, useNA = "ifany")) * 100, 2)) 
  
  # Financial strain
  cbind(Frequency = table(fs$health_util, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$health_util, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$health_util, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$health_util, useNA = "ifany")) * 100, 2)) 
  
  
  
  ## 16.) Hours/day caregiving --------------------------- 
  # Omit 98 (Do not know) & 99 (Refused)
  # 97 = less than 1 hour
  
  # 16a.) ADL
  hist(ACC$RR3HRS7I)
  
  summary(ACC$RR3HRS7I)
  summary(fs$RR3HRS7I)
  summary(no_fs$RR3HRS7I)
  
  # 16b.) IADL
  hist(ACC$RR5HRS7I)
  
  summary(ACC$RR5HRS7I)
  summary(fs$RR5HRS7I)
  summary(no_fs$RR5HRS7I)
  
  
  # 17.) Years/caregiving ---------------------------      
  cbind(
    Frequency = table(ACC$RR7START7I, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$RR7START7I, useNA = "ifany")) * 100, 2)) 
  
  # Financial strain
  cbind(Frequency = table(fs$RR7START7I, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$RR7START7I, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$RR7START7I, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$RR7START7I, useNA = "ifany")) * 100, 2)) 
  
  
  ## 18.) CR has dementia --------------------------- 
  table(ACC$RR6ALZ7I) # Informant provides care because respondent has Alzheimer`s
  table(ACC$TOTMMSE7) # MMSE score < 18
  table(ACC$PRXMENT7) # Proxy used bc respondent is mentally incapacitated or has memory problems such as dementia or Alzheimer’s Disease 
  
  ACC$cr_dementia <- ifelse(ACC$RR6ALZ7I==1 | ACC$TOTMMSE7 < 18 | ACC$PRXMENT7==4, 1, 0)
  
  table(ACC$cr_dementia)
  
  cbind(
    Frequency = table(ACC$cr_dementia, useNA = "ifany"),
    Percentage = round(prop.table(table(ACC$cr_dementia, useNA = "ifany")) * 100, 2))
  
  # Financial strain
  cbind(Frequency = table(fs$cr_dementia, useNA = "ifany"),
        Percentage = round(prop.table(table(fs$cr_dementia, useNA = "ifany")) * 100, 2))
  cbind(Frequency = table(no_fs$cr_dementia, useNA = "ifany"),
        Percentage = round(prop.table(table(no_fs$cr_dementia, useNA = "ifany")) * 100, 2)) 
  
  
  
  
  
  
  
  
  
  #### Need to simplify some categorical variables ---------------------------
  # if they have categories other than the ones we're interested in, e.g., marital status 
  # necessary step for the t-test function to work
  
  # Married
  ACC$cg_married <- ifelse(ACC$CMARSTAT7I_1 == 1, 1,
                           ifelse(ACC$CMARSTAT7I_1 == 9 | is.na(ACC$CMARSTAT7I_1), NA, 0))
  table(ACC$cg_married) #352
  
  
  # HOH
  # HHREL7I: Informant`s relationship to head of household
  ACC$cg_HOH <- ifelse(ACC$HHREL7I == 1, 1,
                       ifelse(ACC$HHREL7I == 98 | ACC$HHREL7I == 99 | is.na(ACC$HHREL7I), NA, 0))
  table(ACC$cg_HOH) #287
  
  
  # Living alone
  ACC$cg_live_alone <- ifelse(ACC$NHOUSE7I == 1, 1,
                              ifelse(is.na(ACC$NHOUSE7I), NA, 0))
  table(ACC$cg_live_alone) #56
  
  
  # Living with parent
  ACC$cg_coreside <- ifelse(ACC$HOUSEK_1 == 1, 1,
                            ifelse(is.na(ACC$HOUSEK_1), NA, 0))
  table(ACC$cg_coreside) #191
  
  
  # Moved in because need place to stay 
  ACC$need_place <- ifelse(ACC$PLASTAY7 == 2, 1,
                           ifelse(is.na(ACC$HOUSEK_1), NA, 0))
  table(ACC$need_place) #38
  
  
  
  # Self-rated health: excellent/good
  ACC$cg_health_xg <- ifelse(ACC$I_HEALTH7I %in% 1:2, 1,
                             ifelse(ACC$I_HEALTH7I == 8 | ACC$I_HEALTH7I == 9 | is.na(ACC$I_HEALTH7I), NA, 0))
  table(ACC$cg_health_xg) #388
  
  
  # Self-rated health: fair/poor
  ACC$cg_health_fp <- ifelse(ACC$I_HEALTH7I %in% 3:4, 1,
                             ifelse(ACC$I_HEALTH7I == 8 | ACC$I_HEALTH7I == 9 | is.na(ACC$I_HEALTH7I), NA, 0))
  table(ACC$cg_health_fp) #267
  
  
  # Self-rated health: categorical combining excellent/good + fair/poor 
  ACC$cg_health <- factor(
    ifelse(ACC$I_HEALTH7I %in% 1:2, "excellent/good",
           ifelse(ACC$I_HEALTH7I %in% 3:4, "fair/poor",
                  ifelse(ACC$I_HEALTH7I %in% 8:9, NA, NA))),
    levels = c("excellent/good", "fair/poor"))
  
  table(ACC$cg_health) #excellent/good: 388; fair/poor: 267
  
  
  # Arthritis
  ACC$cg_arth <- ifelse(ACC$I_ARTHRHEU7I == 1, 1,
                        ifelse(ACC$I_ARTHRHEU7I == 8 | ACC$I_ARTHRHEU7I == 9 | is.na(ACC$I_ARTHRHEU7I), NA, 0))
  table(ACC$cg_arth) #196
  
  
  # Cardio disease
  ACC$cg_heart_probs <- ifelse(ACC$cg_heart_problems == 1, 1,
                               ifelse(is.na(ACC$cg_heart_problems), NA, 0))
  table(ACC$cg_heart_probs) #265
  
  
  # Diabetes
  ACC$cg_diab <- ifelse(ACC$I_MDIAB71I == 1, 1,
                        ifelse(ACC$I_MDIAB71I == 8 | ACC$I_MDIAB71I == 9 | is.na(ACC$I_MDIAB71I), NA, 0))
  table(ACC$cg_diab) #124
  
  
  # Cancer
  ACC$cg_cancer <- ifelse(ACC$I_LCANCR71I == 1, 1,
                          ifelse(ACC$I_LCANCR71I == 8 | ACC$I_LCANCR71I == 9 | is.na(ACC$I_LCANCR71I), NA, 0))
  table(ACC$cg_cancer) #29
  
  
  # Any chronic condition 
  ACC$cg_chronic_any <- ifelse(ACC$cg_arth==1 | ACC$cg_heart_probs==1 | ACC$cg_diab==1 | ACC$cg_cancer==1, 1, 0)
  table(ACC$cg_chronic_any) # 1 = 383; 0 = 1 
  
  
  # Felt sad: Rarely/some
  ACC$cg_sad_rare <- ifelse(ACC$X7CESD6I %in% 0:1, 1,
                            ifelse(ACC$X7CESD6I == 8 | is.na(ACC$I_LCANCR71I), NA, 0))
  table(ACC$cg_sad_rare) #632
  
  
  # Felt sad: Moderate/most
  ACC$cg_sad_most <- ifelse(ACC$X7CESD6I %in% 2:3, 1,
                            ifelse(ACC$X7CESD6I == 8 | is.na(ACC$I_LCANCR71I), NA, 0))
  table(ACC$cg_sad) #25
  
  
  # Feels sad: categorical combining rarely/some + moderate/most
  ACC$cg_sad <- factor(
    ifelse(ACC$X7CESD6I %in% 0:1, "rarely/some",
           ifelse(ACC$X7CESD6I %in% 2:3, "moderate/most",
                  ifelse(ACC$X7CESD6I == 8, NA, NA))),
    levels = c("rarely/some", "moderate/most"))
  
  table(ACC$cg_sad) #rarely/some: 632; moderate/most: 25
  
  
  # ADL: Hours/day caregiving 
    # Create new variable for average care hours
    ACC$cg_hours_adl <- ACC$RR3HRS7I
    
    # Handle special codes
    ACC$cg_hours_adl[ACC$cg_hours_adl == 97] <- 0.5  # "Less than 1 hr" = 0.5
    ACC$cg_hours_adl[ACC$cg_hours_adl >= 98] <- NA   # Set unknown/refused to NA
    
    # Check first few rows to verify
    head(ACC[, c("RR3HRS7I", "cg_hours_adl")])
    
    # Get summary of the new variable
    summary(ACC$cg_hours_adl)
    

    
    
    
  # IADL: Hours/day caregiving 
    # Create new variable for average care hours
    ACC$cg_hours_iadl <- ACC$RR5HRS7I
    
    # Handle special codes
    ACC$cg_hours_iadl[ACC$cg_hours_iadl == 97] <- 0.5  # "Less than 1 hr" = 0.5
    ACC$cg_hours_iadl[ACC$cg_hours_iadl >= 98] <- NA   # Set unknown/refused to NA
    
    # Check first few rows to verify
    head(ACC[, c("RR5HRS7I", "cg_hours_iadl")])
    
    # Get summary of the new variable
    summary(ACC$cg_hours_iadl)
    
  
  
  # Years/caregiving
  ACC$cg_yrs_care <- factor(
    ifelse(ACC$RR7START7I %in% c(1, 2), "1 year or less",
           ifelse(ACC$RR7START7I == 3, "1-2 years",
                  ifelse(ACC$RR7START7I %in% c(4, 7), "3-5 years", # put "other" under 3-5, since most common
                         ifelse(ACC$RR7START7I %in% c(5, 6), "6+ years",
                                ifelse(ACC$RR7START7I %in% c(8, 9), NA, NA))))),
    levels = c("1 year or less", "1-2 years", "3-5 years", "6+ years"))
  
  table(ACC$cg_yrs_care) #29
  
  
  # Dementia CR 
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
  
  # Check results
  summary_stats <- data.frame(
    Total = sum(!is.na(ACC$cr_dementia)),
    Missing = sum(is.na(ACC$cr_dementia)),
    Dementia = sum(ACC$cr_dementia == 1, na.rm = TRUE),
    No_Dementia = sum(ACC$cr_dementia == 0, na.rm = TRUE)
  )
  print(summary_stats)
  
  # Cross-tab with financial strain
  table(ACC$cr_dementia, ACC$cg_finc_strain, useNA = "always")
  
  
  
  
  
  
  
  #### Function to calculate p-values for financial strain vs no financial strain ---------------------------     
  calc_all_pvalues <- function(ACC) {
    
    # For continuous variables using the working approach
    age_p <- t.test(ACC$AGE7I[ACC$cg_finc_strain == 1], 
                    ACC$AGE7I[ACC$cg_finc_strain == 0])$p.value
    
    hours_ADL <- t.test(ACC$RR3HRS7I[ACC$cg_finc_strain == 1], 
                        ACC$RR3HRS7I[ACC$cg_finc_strain == 0])$p.value
    
    hours_IADL <- t.test(ACC$RR5HRS7I[ACC$cg_finc_strain == 1], 
                         ACC$RR5HRS7I[ACC$cg_finc_strain == 0])$p.value
    
    hh_size <- t.test(ACC$NHOUSE7I[ACC$cg_finc_strain == 1], 
                      ACC$NHOUSE7I[ACC$cg_finc_strain == 0])$p.value
    
    
    # For categorical variables, with error handling (Fisher's exact test)
    categorical_vars <- c("SEX7I", "cg_educ_cat", "cg_married", "cg_HOH", 
                          "cg_inc_cat", "cg_live_alone", "cg_coreside", "need_place", 
                          "cg_health", "cg_arth", "cg_heart_probs", "cg_diab", "cg_cancer", 
                          "chronic_any", "cg_sad", "health_util", 
                          "cg_yrs_care", "cr_dementia")
    
    categorical_tests <- sapply(categorical_vars, function(var) {
      tab <- table(ACC[[var]], ACC$cg_finc_strain)
      
      # Check dimensions of table
      if(nrow(tab) < 2 || ncol(tab) < 2) {
        return(NA) # Return NA if table is too small
      }
      tryCatch({
        test_result <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)
        return(test_result$p.value)
      }, error = function(e) {
        # Print which variable caused the error
        print(paste("Error in variable:", var))
        print(table(ACC[[var]], ACC$cg_finc_strain))
        return(NA)
      })
    })
    
    # Combine results
    pvals <- c(age = age_p, 
               hours_ADL = hours_ADL, 
               hours_IADL = hours_IADL,
               hh_size = hh_size, 
               categorical_tests)
    
    # Create nice formatted output
    results_df <- data.frame(
      variable = names(pvals),
      p_value = round(pvals, 3)
    )
    
    return(results_df)
  }
  
  # Run the function
  results <- calc_all_pvalues(ACC)
  
  # View results
  print(results)
  
  
  # Use chi-squared seperately for CR dementia
  tab <- table(ACC$cr_dementia, ACC$cg_finc_strain)
  chisq.test(tab)
  print(tab)
  
  
  # Use chi-squared seperately for need place to stay
  tab <- table(ACC$need_place, ACC$cg_finc_strain)
  chisq.test(tab)
  print(tab)
  table(ACC$need_place, ACC$cg_finc_strain)
  
