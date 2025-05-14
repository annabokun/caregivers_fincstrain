#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

  # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025
  
  # Regression Analyses

# Script created in RStudio ("Cranberry Hibiscus")
# updated 12/16/2024
#=======================================================================================================#


  #==============#
  #=== SET-UP ===#
  #==============#
  
  #### Load packages 
    library(tidyverse)
    library(gtsummary)
    
    

  #### Set working directory --------------------------- 
    setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files")
  
  
  
  #### Load df (see acc_table1 for data source & cleaning)
    # ACC
  
  
  
  #================#
  #=== MEASURES ===#
  #================#
  
    
    #### 1.) Gen sex dummy ---------------------------  
      table(ACC$SEX7I)  # 1 (MALE) = 193
                        # 2 (FEMALE) = 466
      
      ACC$cg_female <- ifelse(ACC$SEX7I == 2, 1, 0)
      
      table(ACC$cg_female) # 1 = 466 (FEMALE); 0 (MALE) = 193

    
    
    
    #### 2.) Collapse age ---------------------------  
      summary(ACC$AGE7I) # 26:75
      hist(ACC$AGE7I)
      
      ACC$cg_age_cat <- dplyr::case_when(
        ACC$AGE7I %in% 18:44 ~ "Early Adulthood (18-44)",
        ACC$AGE7I %in% 45:54 ~ "Middle Age (45-54)",
        ACC$AGE7I %in% 55:64 ~ "Late Middle Age (55-64)",
        ACC$AGE7I %in% 65:75 ~ "Early Retirement (65-75)")
      
      table(ACC$cg_age_cat)
      # Early Adulthood (18-44): 77
      # Middle Age (45-54): 224
      # Late Middle Age (55-64): 286
      # Early Retirement (65-75): 71
      
        # Age categories too unbalanced? 

  
      # Create centered age and age-squared terms to reduce multicollinearity
      ACC <- ACC %>%
        mutate(
          cg_age_centered = AGE7I - mean(AGE7I, na.rm = TRUE),
          cg_age_sq = AGE7I^2)
      
      table(ACC$cg_age_centered)
      table(ACC$cg_age_sq)
      
      
      
      
      
    #### 3.) Gen marital status dummy ---------------------------
      
      ## Use case_when, since ifelse will capture IDKs & NAs into "not married" instead of NA
        ACC$cg_marital_stat <- dplyr::case_when(
          ACC$CMARSTAT7I_1 == 1 ~ "1",
          ACC$CMARSTAT7I_1 == 2 ~ "0", 
          ACC$CMARSTAT7I_1 == 3 ~ "0",
          ACC$CMARSTAT7I_1 == 4 ~ "0",
          ACC$CMARSTAT7I_1 == 5 ~ "0",
          ACC$CMARSTAT7I_1 == 9 ~ "Do not know/refused") 
    
          table(ACC$cg_marital_stat) # 1 = 352; 0 = 302; Do not know/refused = 1
          
          
          ## Make into a dummy 
            ACC$cg_marital_stat <- ifelse(ACC$cg_marital_stat == 1, 1, 0)
            table(ACC$cg_marital_stat) # 1 = 352; 0 = 303
            
            
            
            
    #### 4.) Collapse highest education --------------------------- 
            
        ## Already done in script: acc_table1 
            
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
            
            
    
    #### 5.) Collapse HH income (annual) --------------------------- 
                
          ## HH income   
            table(ACC$LL3A7I)  
            hist(ACC$LL3A7I)  
            
          ## Create categorical income variable
              ACC <- ACC %>%
                mutate(
                  cg_inc_cat = case_when(
                    LL3A7I %in% c(1,2) ~ "< $10,000",
                    LL3A7I %in% c(3,4) ~ "$10,000-$19,999", # largest group
                    LL3A7I == 5 ~ "$20,000-$29,999",
                    LL3A7I %in% c(6,7) ~ "$30,000-$49,999",
                    LL3A7I == 8 ~ "$50,000+",
                    TRUE ~ NA_character_))
              

          ## $20k binary
              ACC <- ACC %>%
                mutate(
                  cg_inc_3cat = case_when(
                    LL3A7I %in% c(1,2,3,4) ~ "< $20,000",
                    LL3A7I %in% c(5,6,7,8) ~ "$20,000+",
                    is.na(LL3A7I) | LL3A7I == 9 ~ "Missing", # Assuming 9 is missing code
                    TRUE ~ NA_character_
                  )
                )
              
              # Verify the categories
              table(ACC$cg_inc_3cat, useNA = "always")
              
                
              

              
              
              
              
              
            
  #### 6.) Head of household status --------------------------  
    ACC$cg_HOH <- ifelse(ACC$HHREL7I == 1, 1,
      ifelse(ACC$HHREL7I == 98 | ACC$HHREL7I == 99 | is.na(ACC$HHREL7I), NA, 0))
    
    table(ACC$cg_HOH) #1 = 287; 0 = 371

         
         
    
  #### 7.) Gen CG intensity dummy -------------------------- 
    
    ACC <- ACC %>%
      mutate(
        # First create the separate ADL and iADL variables as before
        cg_adl_intensity = case_when(
          RR3HRS7I %in% c(0, 97) ~ "Low intensity",
          RR3HRS7I %in% c(1, 2, 3, 4) ~ "Moderate intensity",
          RR3HRS7I >= 5 & RR3HRS7I < 98 ~ "High intensity",
          RR3HRS7I %in% c(98, 99) ~ NA_character_,
          TRUE ~ NA_character_
        ),
        cg_iadl_intensity = case_when(
          RR5HRS7I %in% c(0, 97) ~ "Low intensity",
          RR5HRS7I %in% c(1, 2, 3, 4) ~ "Moderate intensity",
          RR5HRS7I >= 5 & RR5HRS7I < 98 ~ "High intensity",
          RR5HRS7I %in% c(98, 99) ~ NA_character_,
          TRUE ~ NA_character_
        ),
        
        # Then combine them into a single intensity measure
        cg_intensity = case_when(
          # High intensity if either ADL or iADL is high
          cg_adl_intensity == "High intensity" | cg_iadl_intensity == "High intensity" ~ "High intensity",
          
          # Moderate intensity if either is moderate (and neither is high)
          cg_adl_intensity == "Moderate intensity" | cg_iadl_intensity == "Moderate intensity" ~ "Moderate intensity",
          
          # Low intensity if both are low
          cg_adl_intensity == "Low intensity" & cg_iadl_intensity == "Low intensity" ~ "Low intensity",
          
          # NA if both are NA
          TRUE ~ NA_character_
        ),
        
        # Convert to factor with meaningful order
        cg_intensity = factor(cg_intensity, 
                              levels = c("Low intensity", 
                                         "Moderate intensity", 
                                         "High intensity")))
    
    # Check distribution
    table(ACC$cg_intensity, useNA = "always")
    
    # Optional: check cross-tabulation to see how ADL and iADL intensities combine
    table(ACC$cg_adl_intensity, ACC$cg_iadl_intensity, useNA = "always")
 
    
    # Let's check the distributions
    table(ACC$cg_adl_intensity, useNA = "always")
      # Low: 122
      # Moderate: 201
      # High: 158 
      # NA: 178 
    
    table(ACC$cg_iadl_intensity, useNA = "always")
      # Low: 106
      # Moderate: 279
      # High: 219 
      # NA: 55  
    
    
    
    
  #### 8.) Care recipient self-rated health --------------------------  
    
    # Self-rated health: categorical combining excellent/good + fair/poor 
      ACC$cr_health <- factor(
        ifelse(ACC$HEALTH7 %in% 1:2, "excellent/good",
               ifelse(ACC$HEALTH7 %in% 3:4, "fair/poor",
                      ifelse(ACC$HEALTH7 %in% 8:9, NA, NA))),
        levels = c("excellent/good", "fair/poor"))
      
      table(ACC$cr_health) #excellent/good: 204; fair/poor: 452    
    
    
    
    
### Save df
write.csv(ACC, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/ACC.csv")
                    




            
  #===================#
  #=== REGRESSIONS ===#
  #===================#

  #### Set reference categories ---------------------------

      ## 1.) Age groups     
        ACC$cg_age_cat <- factor(ACC$cg_age_cat, levels = c("Early Retirement (65-75)", # ref 
                                                    "Early Adulthood (18-44)", 
                                                    "Middle Age (45-54)",
                                                    "Late Middle Age (55-64)"))
      ## 2.) Education 
        ACC$cg_educ_cat <- factor(ACC$cg_educ_cat, levels = c("Less than HS", # ref 
                                                    "HS Graduate", 
                                                    "Some College",
                                                    "Bachelor's+"))
        
      ## 3.) HH income
        ACC$cg_inc_cat <- factor(ACC$cg_inc_cat, levels = c("$10,000-$19,999", # ref 
                                                              "< $10,000", 
                                                              "$20,000-$29,999",
                                                              "$30,000-$49,999",
                                                              "$50,000+"))
        
      ## 4.) Gender 
        ACC$cg_female <- factor(ACC$cg_female, levels = c(0, 1)) # ref 0 (MEN)
        
        
      ## 5.) Marital stat 
        ACC$cg_marital_stat <- factor(ACC$cg_marital_stat, levels = c(0, 1)) # ref 0 (not married)
        
        
      ## 6.) CG intensity 
        ACC$cg_intensity <- factor(ACC$cg_intensity, levels = c("Moderate intensity", # ref = moderate intensity (largest group)
                                                                "High intensity",
                                                                "Low intensity")) 
        
      ## 7.) CR health
        ACC$cr_health <- factor(ACC$cr_health, levels = c("excellent/good", # ref 
                                                                "fair/poor")) 
        
        
        
      ## Check reference levels ---------------------------
        levels(ACC$cg_age_cat) 
        levels(ACC$cg_educ_cat) 
        levels(ACC$cg_inc_cat) 
        levels(ACC$cg_intensity)
        levels(ACC$cr_health)

  
  #### M1:) Baseline model: co-residence on financial strain --------------------------- 
    M1 <- glm(cg_finc_strain ~ cg_coreside + 
                    cg_age_cat + 
                    cg_female + 
                    cg_marital_stat + 
                    cg_educ_cat + 
                    cg_inc_cat, 
                  family = binomial(link = "logit"), 
                  data = ACC)
    
        M1_table <- tbl_regression(
          M1, 
          exponentiate = TRUE,
          label = list(
            cg_coreside ~ "Co-residence with parent",
            cg_age_cat ~ "Age group",
            cg_female ~ "Female",
            cg_marital_stat ~ "Marital status",
            cg_educ_cat ~ "Education",
            cg_inc_cat ~ "Household income (annual)"
          )
        ) %>%
          modify_caption("**M1. Logistic Regression of Financial Strain on Co-residence**")
        
        M1_table
    
    
    
        
        
    #### M2:) Baseline model: co-residence*HOH on financial strain --------------------------- 
      M2 <- glm(cg_finc_strain ~ cg_coreside*cg_HOH + 
                  cg_age_cat + 
                  cg_female + 
                  cg_marital_stat + 
                  cg_educ_cat + 
                  cg_inc_cat, 
                family = binomial(link = "logit"), 
                data = ACC)
          
          M2_table <- tbl_regression(
          M2, 
          exponentiate = TRUE,
          label = list(
            cg_coreside ~ "Co-residence with parent",
            cg_HOH ~ "Caregiver as head of household", 
            cg_age_cat ~ "Age group",
            cg_female ~ "Female",
            cg_marital_stat ~ "Marital status",
            cg_educ_cat ~ "Education",
            cg_inc_cat ~ "Household income (annual)"
          )
          ) %>%
        modify_caption("**M2. Logistic Regression of Financial Strain on Co-residence x HOH**")
      
      M2_table
  
  
  
      
      #### M3:) Baseline model: co-residence*CG intensity on financial strain --------------------------- 
        M3 <- glm(cg_finc_strain ~ cg_coreside*cg_intensity + 
                    cg_age_cat + 
                    cg_female + 
                    cg_marital_stat + 
                    cg_educ_cat + 
                    cg_inc_cat, 
                  family = binomial(link = "logit"), 
                  data = ACC)
        
        M3_table <- tbl_regression(
          M3, 
          exponentiate = TRUE,
          label = list(
            cg_coreside ~ "Co-residence with parent",
            cg_intensity ~ "Caregiver intensity", 
            cg_age_cat ~ "Age group",
            cg_female ~ "Female",
            cg_marital_stat ~ "Marital status",
            cg_educ_cat ~ "Education",
            cg_inc_cat ~ "Household income (annual)"
          )
        ) %>%
          modify_caption("**M3. Logistic Regression of Financial Strain on Co-residence x CG Intensity**")
        
        M3_table
  
  
        
        
  #### M4:) Baseline model: co-residence*HOH + co-residence*CG intensity on financial strain --------------------------- 
        M4 <- glm(cg_finc_strain ~ cg_coreside*cg_HOH + cg_coreside*cg_intensity +  
                    cg_age_cat + 
                    cg_female + 
                    cg_marital_stat + 
                    cg_educ_cat + 
                    cg_inc_cat, 
                  family = binomial(link = "logit"), 
                  data = ACC)
        
        M4_table <- tbl_regression(
          M4, 
          exponentiate = TRUE,
          label = list(
            cg_coreside ~ "Co-residence with parent",
            cg_HOH ~ "Caregiver as head of household",
            cg_intensity ~ "Caregiver intensity", 
            cg_age_cat ~ "Age group",
            cg_female ~ "Female",
            cg_marital_stat ~ "Marital status",
            cg_educ_cat ~ "Education",
            cg_inc_cat ~ "Household income (annual)"
          )
        ) %>%
          modify_caption("**M4. Logistic Regression of Financial Strain on Co-residence x HOH + Co-residence x CG Intensity**")
        
        M4_table
        
        
        
               
        
        
        
  #### M5:) Baseline model: co-residence*health status of the parent --------------------------- 
        M5 <- glm(cg_finc_strain ~ cg_coreside*cr_health +  
                    cg_age_cat + 
                    cg_female + 
                    cg_marital_stat + 
                    cg_educ_cat + 
                    cg_inc_cat, 
                  family = binomial(link = "logit"), 
                  data = ACC)
        
        M5_table <- tbl_regression(
          M5, 
          exponentiate = TRUE,
          label = list(
            cg_coreside ~ "Co-residence with parent",
            cr_health ~ "Health of the parent",
            cg_age_cat ~ "Age group",
            cg_female ~ "Female",
            cg_marital_stat ~ "Marital status",
            cg_educ_cat ~ "Education",
            cg_inc_cat ~ "Household income (annual)"
          )
        ) %>%
          modify_caption("**M5. Logistic Regression of Financial Strain on Co-residence x Health Status of Parent**")
        
        M5_table
        
        
        
    #### Merge them into a single document --------------------------- 
        # Merge them
        merged_tables <- tbl_merge(
          tbls = list(M1_table, M2_table, M3_table, M4_table, M5_table),
          tab_spanner = c("M1_table", "M2_table", "M3_table", "M4_table", "M5_table")
        )
        
        # Save to Word
        merged_tables %>%
          as_flex_table() %>%
          flextable::save_as_docx(path = "acc_regs.docx")
        
        