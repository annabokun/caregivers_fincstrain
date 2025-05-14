#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

  # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025
  
  # TABLE 3: Aging Parent Characteristics by ACC’s HOH Status

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
    
    ## Gen coreside samples 
    coreside <- ACC %>%
      filter(HOUSEK_1 == 1) # 191
    
    no_coreside <- ACC %>%
      filter(HOUSEK_1 != 1) # 456
   
    
    ACC <- ACC %>%
      mutate(cg_coreside = HOUSEK_1==1) # n = 191
    
     
#### SAVE 
write.csv(ACC, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/ACC.csv")
    saveRDS(ACC, file = "ACC.rds") 
write.csv(coreside, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/coreside.csv")
  saveRDS(coreside, file = "coreside.rds") 
write.csv(no_coreside, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/no_coreside.csv")
  saveRDS(no_coreside, file = "no_coreside.rds")    
    


    ## 1.) Age --------------------------- 
    summary(ACC$AGE7)
    hist(ACC$AGE7)

    
    # By co-reside
    summary(coreside$AGE7)
    summary(no_coreside$AGE7)
    
    table(ACC$cg_coreside) # 1 = 191; = 456
    
    # T-test 
    t.test(AGE7 ~ cg_coreside, data = ACC) 
    
    
    
    
    ## 2.) Female % ---------------------------
    # 1 = male; 2 = female
    cbind(Frequency = table(ACC$SEX7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$SEX7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$SEX7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$SEX7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$SEX7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$SEX7, useNA = "ifany")) * 100, 2))
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$SEX7))   
    
    
    
    ## 3.) Married % ---------------------------  
    cbind(Frequency = table(ACC$MARSTAT7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$MARSTAT7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$MARSTAT7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$MARSTAT7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$MARSTAT7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$MARSTAT7, useNA = "ifany")) * 100, 2))
    
    # Dummy for married 
    ACC$cr_married <- ifelse(ACC$MARSTAT7 == 1, 1,
                             ifelse(ACC$MARSTAT7 == 9 | is.na(ACC$MARSTAT7), NA, 0))
    table(ACC$cr_married) #194
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_married))
    
    
    
    ## 4.) HH size ---------------------------  
    summary(ACC$NHOUSE7) # 2.649
    
    # By co-reside 
    summary(coreside$NHOUSE7) # 3.607
    summary(no_coreside$NHOUSE7) # 2.253
    
    # T-test 
    t.test(NHOUSE7 ~ cg_coreside, data = ACC)  
    
    
    
    
    ## 5.) Born in U.S. ---------------------------
    cbind(Frequency = table(ACC$USBORN7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$USBORN7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$USBORN7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$USBORN7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$USBORN7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$USBORN7, useNA = "ifany")) * 100, 2))
    
    # Dummy
    ACC$cr_native <- ifelse(ACC$USBORN7 == 1, 1,
                            ifelse(is.na(ACC$USBORN7), NA, 0))
    table(ACC$cr_native) #352
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_native))
    
    
    
    
    
    ## 5.) ADL ---------------------------
    cbind(Frequency = table(ACC$ANYADL7I, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$ANYADL7I, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$ANYADL7I, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$ANYADL7I, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$ANYADL7I, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$ANYADL7I, useNA = "ifany")) * 100, 2)) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$ANYADL7I))
    
    
    
    ## 5.) IADL ---------------------------
    cbind(Frequency = table(ACC$ANYIADL7I, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$ANYIADL7I, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$ANYIADL7I, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$ANYIADL7I, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$ANYIADL7I, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$ANYIADL7I, useNA = "ifany")) * 100, 2))
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$ANYIADL7I))
    
    
    
    
    ## 6.) CR has dementia --------------------------- 
    table(ACC$RR6ALZ7I) # Informant provides care becuase respondent has Alzheimer`s
    table(ACC$TOTMMSE7) # MMSE score 
    table(ACC$PRXMENT7) # Proxy used bc respondent is mentally incapacitated or has memory problems such as dementia or Alzheimer’s Disease 
    
    ACC$cr_dementia <- ifelse(ACC$RR6ALZ7I==1 | ACC$TOTMMSE7 < 18 | ACC$PRXMENT7==4, 1, 0)
    
    table(ACC$cr_dementia)
    
    cbind(Frequency = table(ACC$cr_dementia, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$cr_dementia, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$cr_dementia, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$cr_dementia, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$cr_dementia, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$cr_dementia, useNA = "ifany")) * 100, 2))
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_dementia))
    
    
    
    
    ## 7.) Chronic conditions ---------------------------  
    
    # 7a.) Arthritis ---------------------------   
    cbind(Frequency = table(ACC$U73X, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$U73X, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$U73X, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$U73X, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$U73X, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$U73X, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_arth <- ifelse(ACC$U73X == 1, 1,
                          ifelse(ACC$U73X %in% 8:9 | is.na(ACC$U73X), NA, 0))
    table(ACC$cr_arth) #194
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_arth))
    
    
    
    
    
    # 7b.) Cardiovascular ---------------------------   
    # Stroke
    cbind(Frequency = table(ACC$JSTROK71, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$JSTROK71, useNA = "ifany")) * 100, 2))
    
    # Hospitalized for stroke
    cbind(Frequency = table(ACC$JSTROK75, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$JSTROK75, useNA = "ifany")) * 100, 2))
    
    # Heart attack
    cbind(Frequency = table(ACC$ICARDI71, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$ICARDI71, useNA = "ifany")) * 100, 2))     
    
    # Hospitalized for heart attack
    cbind(Frequency = table(ACC$ICARDI75, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$ICARDI75, useNA = "ifany")) * 100, 2))
    
    # Heart disease 
    cbind(Frequency = table(ACC$U73Q, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$U73Q, useNA = "ifany")) * 100, 2)) 
    
    # Hypertension
    cbind(Frequency = table(ACC$R_KHYPER71I, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$R_KHYPER71I, useNA = "ifany")) * 100, 2))
    
    # Meds for hypertension
    cbind(Frequency = table(ACC$R_KHYPER74I, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$R_KHYPER74I, useNA = "ifany")) * 100, 2))
    
    
    # Gen dummy for cardiovascular disease   
    ACC$cr_heart_problems <- ifelse(ACC$JSTROK71==1 | #stroke
                                      ACC$JSTROK75==1 | #hp stroke
                                      ACC$ICARDI71==1 | #ha
                                      ACC$ICARDI75==1 | #hp ha
                                      ACC$U73Q==1 | #heart disease
                                      ACC$R_KHYPER71I==1 | #hyper
                                      ACC$R_KHYPER74I==1, 1, 0) #meds for hyper
    
    table(ACC$cr_heart_problems) # 527
    
    
  # RE-CODE
    ACC <- ACC %>%
      mutate(
        cr_heart_problems2 = case_when(
          JSTROK71 == 1 | JSTROK75 == 1 |
            ICARDI71 == 1 | ICARDI75 == 1 |
            U73Q == 1 | 
            R_KHYPER71I == 1 | R_KHYPER74I == 1 ~ 1,  # Any indicator is positive
          
          JSTROK71 %in% c(1, 2) |
            JSTROK75 %in% c(1, 2) |
            ICARDI71 %in% c(1, 2) |
            ICARDI75 %in% c(1, 2) |
            U73Q %in% c(1, 2) |
            R_KHYPER71I %in% c(1, 2) |
            R_KHYPER74I %in% c(1, 2) ~ 0,  # No indicator is 1, but at least one is 2
          
          TRUE ~ NA_real_  # All variables missing or DK/ref
        )
      )
    
    
    
    table(ACC$cr_heart_problems2) # 527
    table(ACC$cr_heart_problems2, useNA = "ifany")
    
    
    
    
    cbind(Frequency = table(ACC$cr_heart_problems, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$cr_heart_problems, useNA = "ifany")) * 100, 2))
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_heart_problems))
    
    # REGENERATE CO-RESIDE SAMPLES
    
    # By co-reside
    cbind(Frequency = table(coreside$cr_heart_problems, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$cr_heart_problems, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$cr_heart_problems, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$cr_heart_problems, useNA = "ifany")) * 100, 2))
    
    
    
    
    ## 7c.) Diabetes ---------------------------  
    cbind(Frequency = table(ACC$MDIAB71, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$MDIAB71, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$MDIAB71, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$MDIAB71, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$MDIAB71, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$MDIAB71, useNA = "ifany")) * 100, 2)) 
    
    # Dummy 
    ACC$cr_diab <- ifelse(ACC$MDIAB71 == 1, 1,
                          ifelse(ACC$MDIAB71 %in% 8:9 | is.na(ACC$MDIAB71), NA, 0))
    table(ACC$cr_diab) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_diab))
    
    
    
    
    ## 7d.) Cancer --------------------------- 
    cbind(Frequency = table(ACC$LCANCR71, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$LCANCR71, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$LCANCR71, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$LCANCR71, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$LCANCR71, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$LCANCR71, useNA = "ifany")) * 100, 2)) 
    
    # Dummy 
    ACC$cr_cancer <- ifelse(ACC$LCANCR71 == 1, 1,
                            ifelse(ACC$LCANCR71 %in% 8:9 | is.na(ACC$LCANCR71), NA, 0))
    table(ACC$cr_cancer) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_cancer)) # p-value = 1
    
    table(ACC$cg_coreside, ACC$cr_cancer)
    
    
    
    ## ANY chronic condition ---------------------------  
    # Create the variable making explicit both 1s AND 0s
    ACC$cr_chronic_any <- ifelse(ACC$U73X==1 | 
                                   ACC$cr_heart_problems==1 | 
                                   ACC$MDIAB71==1 | 
                                   ACC$LCANCR71 == 1, 
                                 1,  # if ANY condition is true
                                 0)  # if ALL conditions are false
    
    # Check the new distribution
    table(ACC$cr_chronic_any)
    
              
              ACC$cr_chronic_any2 <- ifelse(ACC$U73X==1 | 
                                             ACC$cr_heart_problems2==1 | 
                                             ACC$MDIAB71==1 | 
                                             ACC$LCANCR71 == 1, 
                                           1,  # if ANY condition is true
                                           0)  # if ALL conditions are false
              
              # Check the new distribution
              table(ACC$cr_chronic_any2)
    
              
              
              
              ACC <- ACC %>%
                mutate(
                  cr_chronic_any = case_when(
                    U73X == 1 | cr_heart_problems2 == 1 | MDIAB71 == 1 | LCANCR71 == 1 ~ 1,
                    
                    U73X %in% c(1, 2) | cr_heart_problems %in% c(1, 2) |
                      MDIAB71 %in% c(1, 2) | LCANCR71 %in% c(1, 2) ~ 0,
                    
                    TRUE ~ NA_real_
                  )
                )
              
              table(ACC$cr_chronic_any2)
              table(ACC$cr_chronic_any, useNA = "ifany")
              
              
              
    
    
    
    # Check by coresidence
    table(ACC$cr_chronic_any, ACC$cg_coreside)
    
    table(ACC$cr_chronic_any)
    table(ACC$cr_chronic_any, ACC$cg_coreside) 
    
    cbind(Frequency = table(ACC$cr_chronic_any, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$cr_chronic_any, useNA = "ifany")) * 100, 2))
    
    
    # REGENERATE CO-RESIDENCE SAMPLES 
    
    # By co-reside
    cbind(Frequency = table(coreside$cr_chronic_any, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$cr_chronic_any, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$cr_chronic_any, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$cr_chronic_any, useNA = "ifany")) * 100, 2))
    
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_chronic_any)) 
    
    
    
    
    ## 8.) Exercise (minutes per walk) ---------------------------  
    summary(ACC$QQ1B7) # 28.54
    
    # By co-reside 
    summary(coreside$QQ1B7) # 30.61
    summary(no_coreside$QQ1B7) # 27.69
    
    # T-test 
    t.test(QQ1B7 ~ cg_coreside, data = ACC) 
    
    
    
    
    ## 9.) Owns home ---------------------------  
    cbind(Frequency = table(ACC$LL76, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$LL76, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$LL76, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$LL76, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$LL76, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$LL76, useNA = "ifany")) * 100, 2))
    
    # T-test 
    t.test(LL76 ~ cg_coreside, data = ACC) 
    
    
    # Dummy 
    ACC$cr_own_home <- ifelse(ACC$LL76 == 1, 1,
                              ifelse(ACC$LL76 %in% 8:9 | is.na(ACC$LL76), NA, 0))
    table(ACC$cr_own_home) #1 = 376; 0 = 228
    
    
    
    
    
    ## 9.) Respondent`s income adequate to cover monthly expenses? (per informant) ---------------------------  
    cbind(Frequency = table(ACC$TT3I7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$TT3I7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$TT3I7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$TT3I7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$TT3I7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$TT3I7, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_inc_adequate <- ifelse(ACC$TT3I7 == 1, 1,
                                  ifelse(ACC$TT3I7 %in% 8:9 | is.na(ACC$TT3I7), NA, 0))
    table(ACC$cr_inc_adequate) #1 = 522; 0 = 130
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_inc_adequate))
    
    
    
    
    ## 9a.) Respondent receives income from social security (per informant) ---------------------------  
    cbind(Frequency = table(ACC$TT2BI7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$TT2BI7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$TT2BI7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$TT2BI7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$TT2BI7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$TT2BI7, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_inc_ss <- ifelse(ACC$TT2BI7 == 1, 1,
                            ifelse(ACC$TT2BI7 %in% 8:9 | is.na(ACC$TT2BI7), NA, 0))
    table(ACC$cr_inc_ss) #1 = 594; 0 = 50
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_inc_ss))
    
    
    
    ## 9b.) Respondent receives income from SSI (per informant) ---------------------------  
    cbind(Frequency = table(ACC$TT2DI7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$TT2DI7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$TT2DI7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$TT2DI7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$TT2DI7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$TT2DI7, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_inc_ssi <- ifelse(ACC$TT2DI7 == 1, 1,
                             ifelse(ACC$TT2DI7 %in% 8:9 | is.na(ACC$TT2DI7), NA, 0))
    table(ACC$cr_inc_ssi) #1 = 594; 0 = 50
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_inc_ssi))
    
    
    
    
    ## 9c.) Respondent receives income from private pension (per informant) ---------------------------  
    cbind(Frequency = table(ACC$TT2CI7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$TT2CI7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$TT2CI7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$TT2CI7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$TT2CI7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$TT2CI7, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_inc_pension <- ifelse(ACC$TT2CI7 == 1, 1,
                                 ifelse(ACC$TT2CI7 %in% 8:9 | is.na(ACC$TT2CI7), NA, 0))
    table(ACC$cr_inc_pension) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_inc_pension))
    
    
    
    
    ## 9d.) Respondent rest of income comes from property (per informant) ---------------------------  
    cbind(Frequency = table(ACC$TT43I7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$TT43I7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$TT43I7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$TT43I7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$TT43I7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$TT43I7, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_inc_property <- ifelse(ACC$TT43I7 == 1, 1,
                                  ifelse(ACC$TT43I7 %in% 8:9 | is.na(ACC$TT43I7), NA, 0))
    table(ACC$cr_inc_property) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_inc_property))
    
    # Fisher's 
    fisher.test(table(ACC$cg_coreside, ACC$cr_inc_property))
    
    
    
    
    ## 9e.) Respondent rest of income comes from informant (per informant) ---------------------------  
    cbind(Frequency = table(ACC$TT44I7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$TT44I7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$TT44I7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$TT44I7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$TT44I7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$TT44I7, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_inc_cg <- ifelse(ACC$TT44I7 == 1, 1,
                            ifelse(ACC$TT44I7 %in% 8:9 | is.na(ACC$TT44I7), NA, 0))
    table(ACC$cr_inc_cg) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_inc_cg))
    
    
    
    
    
    ## 9f.) Respondent rest of income comes from other family members (per informant) ---------------------------  
    cbind(Frequency = table(ACC$TT45I7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$TT45I7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$TT45I7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$TT45I7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$TT45I7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$TT45I7, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_inc_fam <- ifelse(ACC$TT45I7 == 1, 1,
                             ifelse(ACC$TT45I7 %in% 8:9 | is.na(ACC$TT45I7), NA, 0))
    table(ACC$cr_inc_fam) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_inc_fam))
    
    
    
    
    
    ## 10.) Health insurance ---------------------------  
    
    ## 10a. Medicare 
    cbind(Frequency = table(ACC$RMM9A7I, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$RMM9A7I, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$RMM9A7I, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$RMM9A7I, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$RMM9A7I, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$RMM9A7I, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_medicare <- ifelse(ACC$RMM9A7I == 1, 1,
                              ifelse(ACC$RMM9A7I %in% 8:9 | is.na(ACC$RMM9A7I), NA, 0))
    table(ACC$cr_medicare) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_medicare))
    
    
    
    ## 10b. Medicaid (Medicaid is misspelled in HEPESE "mdedicaid")
    cbind(Frequency = table(ACC$RMM9B7I, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$RMM9B7I, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$RMM9B7I, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$RMM9B7I, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$RMM9B7I, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$RMM9B7I, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_medicaid_dum <- ifelse(ACC$RMM9B7I == 1, 1,
                                  ifelse(ACC$RMM9B7I %in% 8:9 | is.na(ACC$RMM9B7I), NA, 0))
    table(ACC$cr_medicaid_dum) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_medicaid_dum))
    
    
    
    
    ## 10c. Private
    cbind(Frequency = table(ACC$RMM9C7I, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$RMM9C7I, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$RMM9C7I, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$RMM9C7I, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$RMM9C7I, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$RMM9C7I, useNA = "ifany")) * 100, 2))
    
    # Dummy 
    ACC$cr_private_dum <- ifelse(ACC$RMM9C7I == 1, 1,
                                 ifelse(ACC$RMM9C7I %in% 8:9 | is.na(ACC$RMM9C7I), NA, 0))
    table(ACC$cr_private_dum) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_private_dum))
    
    
    
    ## 10d. Other 
    cbind(Frequency = table(ACC$RMM9E7I, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$RMM9E7I, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$RMM9E7I, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$RMM9E7I, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$RMM9E7I, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$RMM9E7I, useNA = "ifany")) * 100, 2))
    
    
    # Dummy 
    ACC$cr_other_dum <- ifelse(ACC$RMM9E7I == 1, 1,
                               ifelse(ACC$RMM9E7I %in% 8:9 | is.na(ACC$RMM9E7I), NA, 0))
    table(ACC$cr_other_dum) 
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_other_dum))
    
    
    
    ## 10f. Medicare + Medicaid ("duals")
    ACC$cr_duals <- ifelse(ACC$RMM9A7I==1 & ACC$RMM9B7I==1, 1,0)
    
    table(ACC$cr_duals) #367
    
    cbind(Frequency = table(ACC$cr_duals, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$cr_duals, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$cr_duals, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$cr_duals, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$cr_duals, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$cr_duals, useNA = "ifany")) * 100, 2))
    
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_duals))
    
    
    
    
    ## 11. Self-rated health ---------------------------   
    cbind(Frequency = table(ACC$HEALTH7, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$HEALTH7, useNA = "ifany")) * 100, 2))
    
    # By co-reside
    cbind(Frequency = table(coreside$HEALTH7, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$HEALTH7, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$HEALTH7, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$HEALTH7, useNA = "ifany")) * 100, 2))    
    
    
    # Self-rated health: categorical combining excellent/good + fair/poor 
    ACC$cr_health <- factor(
      ifelse(ACC$HEALTH7 %in% 1:2, "excellent/good",
             ifelse(ACC$HEALTH7 %in% 3:4, "fair/poor",
                    ifelse(ACC$HEALTH7 %in% 8:9, NA, NA))),
      levels = c("excellent/good", "fair/poor"))
    
    table(ACC$cr_health) #excellent/good: 204; fair/poor: 452
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_health)) 
    
    
    
    
    ## 12.) Felt sad ---------------------------  
    cbind(Frequency = table(ACC$X7CESD18, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$X7CESD18, useNA = "ifany")) * 100, 2)) 
    
    # By co-reside
    cbind(Frequency = table(coreside$X7CESD18, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$X7CESD18, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$X7CESD18, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$X7CESD18, useNA = "ifany")) * 100, 2)) 
    
    # Feels sad: categorical combining rarely/some + moderate/most
    ACC$cr_sad <- factor(
      ifelse(ACC$X7CESD18 %in% 0:1, "rarely/some",
             ifelse(ACC$X7CESD18 %in% 2:3, "moderate/most",
                    ifelse(ACC$X7CESD18 == 8, NA, NA))),
      levels = c("rarely/some", "moderate/most"))
    
    table(ACC$cr_sad) #rarely/some: 496; moderate/most: 89
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_sad)) 
    
    
    
    
    ## 13.) Healthcare utilization --------------------------- 
    
    # Gen hospital stays binary
    ACC$cr_hospital_any <- ifelse(ACC$P9FALLS7==1 | # falls 
                                    ACC$JSTROK75==1 | # stroke
                                    ACC$NFRAC75==1 | #broken hip
                                    ACC$ICARDI75==1 | #heart attack
                                    ACC$CC71A==1 | #illness
                                    ACC$R_JSTROK75I==1 | #stroke 
                                    ACC$R_ICARDI75I==1 | #heart attack
                                    ACC$R_NFRAC75I==1, 1, 0) #hip fracture 
    table(ACC$cr_hospital_any) #326
    
    
    ACC$cr_health_util <- case_when(
      ACC$KK72 >= 6 & ACC$cr_hospital_any == 1 ~ "High",    # 6+ doctor visits WITH hospitalization 
      ACC$KK72 %in% 4:5 & ACC$cr_hospital_any == 1 ~ "Moderate",  # 4-5 doctor visits AND any hospitalization
      ACC$KK72 %in% 0:3 & ACC$cr_hospital_any == 1 ~ "Low")      # 0-3 doctor visits AND any hospitalization
    
    table(ACC$cr_health_util)
    
    # Convert to factor with ordered levels
    ACC$cr_health_util <- factor(ACC$cr_health_util, 
                                 levels = c("Low", "Moderate", "High"))
    
    cbind(Frequency = table(ACC$cr_health_util, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$cr_health_util, useNA = "ifany")) * 100, 2)) 
    
    # By co-reside
    cbind(Frequency = table(coreside$cr_health_util, useNA = "ifany"),
          Percentage = round(prop.table(table(coreside$cr_health_util, useNA = "ifany")) * 100, 2))
    cbind(Frequency = table(no_coreside$cr_health_util, useNA = "ifany"),
          Percentage = round(prop.table(table(no_coreside$cr_health_util, useNA = "ifany")) * 100, 2))  
    
    
    # Chi-squared 
    chisq.test(table(ACC$cg_coreside, ACC$cr_health_util))
    
    
    
    
    
    
  #===============#
  #=== TABLE 3 ===#
  #===============#
    
    
    ### NOTE: MAKE SURE % ARE USING THE COLUMN SAMPLE AS THE DENOMINATOR 
    ### SOMETIMES IT INCLUDES MISSING DATA IN THE DENOM, MESSING UP THE %S  ??? 
    
    # Set compact theme for gtsummary
    theme_gtsummary_compact()
    
    # ? # Ensure cg_coreside is a factor with missing values labeled
    ACC <- ACC %>%
      mutate(cg_coreside = fct_explicit_na(cg_coreside, na_level = "Missing"))
    
    
    
    
    table3_acc <- ACC_full_imputed %>% 
      select(cg_coreside, AGE7, SEX7, cr_married, NHOUSE7, cr_native,  # demographics
             ANYADL7I, ANYIADL7, cr_dementia2, # health 
             cr_health, cr_sad, cr_chronic_any, cr_arth, cr_heart_problems2, cr_diab, cr_cancer, 
             cr_health_util, cr_medicare, cr_medicaid_dum, cr_duals, cr_private_dum, cr_other_dum,  # insurance     
             cr_own_home, cr_inc_adequate, cr_inc_ss, cr_inc_ssi, cr_inc_pension, QQ1B7) %>% # economics
      mutate(
        # Ensure continuous variables are numeric
        AGE7 = as.numeric(AGE7),
        NHOUSE7 = as.numeric(NHOUSE7),
        # Create factor with specific level order for financial strain
        cg_coreside = factor(cg_coreside, 
                                levels = c(1, 0),
                                labels = c("Co-reside", "No co-reside"))
      ) %>%
      tbl_summary(
        by = cg_coreside,
        type = list(
          "AGE7" ~ "continuous",
          "NHOUSE7" ~ "continuous"
        ),
        statistic = list(
          all_continuous() ~ "{mean}",
          all_categorical() ~ "{n} ({p}%)"
        ),
        label = list(
          AGE7 ~ "Age",
          SEX7 ~ "Female",
          cr_married ~ "Married",
          cr_native ~ "U.S. born", 
          NHOUSE7 ~ "Household size", 
          cr_health ~ "Self-rated health", 
          cr_chronic_any ~ "Any chronic condition",
          cr_arth ~ "Arthritis",
          cr_heart_problems2 ~ "Cardiovascular disease",
          cr_diab ~ "Diabetes",
          cr_cancer ~ "Cancer",
          cr_sad ~ "Felt sad",
          cr_health_util ~ "Healthcare utilization",
          ANYADL7I ~ "ADL",
          ANYIADL7 ~ "IADL",
          QQ1B7 ~ "Exercise minutes", 
          cr_dementia2 ~ "Dementia", 
          cr_medicare ~ "Medicare", 
          cr_medicaid_dum ~ "Medicaid", 
          cr_duals ~ "Medicare + Medicaid (“duals”)", 
          cr_private_dum ~ "Private", 
          cr_other_dum ~ "Other",
          cr_own_home ~ "Owns home",
          cr_inc_adequate ~ "Income adequate to cover expenses", 
          cr_inc_ss ~ "Social security", 
          cr_inc_ssi ~ "SSI", 
          cr_inc_pension ~ "Pension"
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
      modify_caption("**Table 3. Aging Parents: Demographic, Health, and Economic Characteristics**") 
    
    print(table3_acc)
    
    
    %>%
      as_flex_table() %>%
      save_as_docx(path = "table3_acc.docx")
