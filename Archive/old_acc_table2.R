#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

  # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025
  
    # TABLE 2: ACC Financial Strain by Co-residence Status

# Script created in RStudio ("Cranberry Hibiscus")
# updated 12/9/2024
#=======================================================================================================#


  #==============#
  #=== SET-UP ===#
  #==============#
  
  #### Set working directory --------------------------- 
    setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files")
  
  
  
  #### Load df (see acc_table1 for data source & cleaning)
    # ACC
  
  
  #================#
  #=== CLEANING ===#
  #================#
  
  
  #### Generate co-residence samples
  
    ## Co-resident 
      coreside <- ACC %>%
        filter(HOUSEK_1==1)
      
      length(unique(coreside$Q_NO)) # n = 191
  
    ## Not co-resident 
      no_coreside <- ACC %>%
        filter(HOUSEK_1 == 2)
      
      length(unique(no_coreside$Q_NO)) # n = 456
    
        
    # Already gen binary in 04_logits
      table(ACC$cg_coreside) # = 191; 0 = 456
      
      
      
      
      
  #### 1.) Financial strain ---------------------------   
      
      # By co-residence
      cbind(Frequency = table(coreside$cg_finc_strain, useNA = "ifany"),
            Percentage = round(prop.table(table(coreside$cg_finc_strain, useNA = "ifany")) * 100, 2))
      cbind(Frequency = table(no_coreside$cg_finc_strain, useNA = "ifany"),
            Percentage = round(prop.table(table(no_coreside$cg_finc_strain, useNA = "ifany")) * 100, 2))
    
      # Chi-squared
        chisq.test(table(ACC$cg_coreside, ACC$cg_finc_strain)) 
      
    
  #### 1a.) Difficult to help my own kids ---------------------------   
      cbind(Frequency = table(ACC$TT57I_1, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$TT57I_1, useNA = "ifany")) * 100, 2))
      
      # By co-residence
      cbind(Frequency = table(coreside$TT57I_1, useNA = "ifany"),
            Percentage = round(prop.table(table(coreside$TT57I_1, useNA = "ifany")) * 100, 2))
      cbind(Frequency = table(no_coreside$TT57I_1, useNA = "ifany"),
            Percentage = round(prop.table(table(no_coreside$TT57I_1, useNA = "ifany")) * 100, 2))
    
      # Generate dummy 
      ACC$cg_diff_help_kids <- ifelse(ACC$TT57I_1 == 1, 1,
                                      ifelse(ACC$TT57I_1 == 8 | ACC$TT57I_1 == 9, NA, 0))
      
      print(table(ACC$cg_diff_help_kids, useNA = "ifany")) # n = 33
      
      # Examine the contingency table for this specific variable
      print(table(ACC$cg_diff_help_kids, ACC$cg_coreside, useNA = "ifany"))


      
      
  #### 1b.) Difficult for my own housing expenses ---------------------------   
      
      cbind(Frequency = table(ACC$TT57I_2, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$TT57I_2, useNA = "ifany")) * 100, 2))
      
      # By co-residence
      cbind(Frequency = table(coreside$TT57I_2, useNA = "ifany"),
            Percentage = round(prop.table(table(coreside$TT57I_2, useNA = "ifany")) * 100, 2))
      cbind(Frequency = table(no_coreside$TT57I_2, useNA = "ifany"),
            Percentage = round(prop.table(table(no_coreside$TT57I_2, useNA = "ifany")) * 100, 2))
      
      # Generate dummy 
      ACC$cg_diff_house <- ifelse(ACC$TT57I_2 == 2, 1,
                                      ifelse(ACC$TT57I_2 == 8 | ACC$TT57I_2 == 9 | is.na(ACC$TT57I_2), NA, 0))
      table(ACC$cg_diff_house) #51
      table(ACC$TT57I_2)
      
      
      
      
  #### 1c.) Difficult for me to travel/vacation ---------------------------   
      
      cbind(Frequency = table(ACC$TT57I_3, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$TT57I_3, useNA = "ifany")) * 100, 2))
      
      # By co-residence
      cbind(Frequency = table(coreside$TT57I_3, useNA = "ifany"),
            Percentage = round(prop.table(table(coreside$TT57I_3, useNA = "ifany")) * 100, 2))
      cbind(Frequency = table(no_coreside$TT57I_3, useNA = "ifany"),
            Percentage = round(prop.table(table(no_coreside$TT57I_3, useNA = "ifany")) * 100, 2))
    
      # Generate dummy 
      ACC$cg_diff_travel <- ifelse(ACC$TT57I_3 == 3, 1,
                                  ifelse(ACC$TT57I_3 == 8 | ACC$TT57I_3 == 9 | is.na(ACC$TT57I_3), NA, 0))
      table(ACC$cg_diff_travel) #48
    
      
      
      
  #### 1d.) Difficult for me to do other things ---------------------------   
      
      cbind(Frequency = table(ACC$TT57I_4, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$TT57I_4, useNA = "ifany")) * 100, 2)) # n = 14
                               
      # By co-residence
      cbind(Frequency = table(coreside$TT57I_4, useNA = "ifany"),
            Percentage = round(prop.table(table(coreside$TT57I_4, useNA = "ifany")) * 100, 2))
      cbind(Frequency = table(no_coreside$TT57I_4, useNA = "ifany"),
            Percentage = round(prop.table(table(no_coreside$TT57I_4, useNA = "ifany")) * 100, 2))
      
      
      
      
      
  #### 1e.) Difficult for other specified reasons ---------------------------   
      
      cbind(Frequency = table(ACC$TT57SPECI, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$TT57SPECI, useNA = "ifany")) * 100, 2))
      
      # By co-residence
      cbind(Frequency = table(coreside$TT57SPECI, useNA = "ifany"),
            Percentage = round(prop.table(table(coreside$TT57SPECI, useNA = "ifany")) * 100, 2))
      cbind(Frequency = table(no_coreside$TT57SPECI, useNA = "ifany"),
            Percentage = round(prop.table(table(no_coreside$TT57SPECI, useNA = "ifany")) * 100, 2))
      
      
    # Generate dummy for other + other/unspecified ---------------------------   
      ACC$cg_other_unspec <- ifelse(ACC$TT57I_4 == 4 | ACC$TT57SPECI == "FOOD" | ACC$TT57SPECI == "HER HOUSE" | 
                                    ACC$TT57I_4 == "HOME REPAIRS" | ACC$TT57I_4 == "PROTECTIVE SERVICES WON'T LET ME WORK FULL TIME" | 
                                    ACC$TT57I_4 == "UNIFORMS FOR SCHOOL" | ACC$TT57I_4 == "groceries" | ACC$TT57I_4 == "no money left for personal expenses", 1,
                                   ifelse(ACC$TT57I_4 == 8 | ACC$TT57I_4 == 9 | is.na(ACC$TT57I_4), NA, 0))
      table(ACC$cg_other_unspec) #14
      
      
      cbind(Frequency = table(ACC$cg_other_unspec, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$cg_other_unspec, useNA = "ifany")) * 100, 2))
      
      # By co-residence
      cbind(Frequency = table(coreside$cg_other_unspec, useNA = "ifany"),
            Percentage = round(prop.table(table(coreside$cg_other_unspec, useNA = "ifany")) * 100, 2))
      cbind(Frequency = table(no_coreside$cg_other_unspec, useNA = "ifany"),
            Percentage = round(prop.table(table(no_coreside$cg_other_unspec, useNA = "ifany")) * 100, 2))
      
      
      
      
    # Create binary (0/1) variables for each category ---------------------------   
      ACC$cg_diff_help_kids <- ifelse(ACC$TT57I_1 == 1, 1, 0)  # instead of NA, use 0
      ACC$cg_diff_house <- ifelse(ACC$TT57I_2 == 2, 1, 0)
      ACC$cg_diff_travel <- ifelse(ACC$TT57I_3 == 3, 1, 0)
      ACC$cg_diff_other_unspec <- ifelse(ACC$TT57I_4 == 4 | 
                                           ACC$TT57SPECI %in% c("FOOD", "HER HOUSE", "HOME REPAIRS",
                                                                "PROTECTIVE SERVICES WON'T LET ME WORK FULL TIME",
                                                                "UNIFORMS FOR SCHOOL", "groceries", 
                                                                "no money left for personal expenses"), 
                                         1, 0)
      
      table(ACC$cg_diff_help_kids) # 1=33; 0=5
      table(ACC$cg_diff_house) #1=51
      table(ACC$cg_diff_travel) #1=48
      table(ACC$cg_diff_other_unspec) #1=14
      
      
  #### 2.) Has the adult child caregiver (ACC) received financial help from the care recipient in the past year? --------------------------- 
      # cg_receive_help generated in 04_logits script 
      
      # Gen receive help
      ACC$cg_receive_help <- ifelse(ACC$SS3I7 %in% 2:4, 1,
                                ifelse(ACC$SS3I7 == 8 | is.na(ACC$SS3I7), NA, 0))
      table(ACC$cg_receive_help) #1=89; 0=567
      
      
      
      # By co-residence
      cbind(Frequency = table(coreside$cg_receive_help, useNA = "ifany"),
            Percentage = round(prop.table(table(coreside$cg_receive_help, useNA = "ifany")) * 100, 2))
      cbind(Frequency = table(no_coreside$cg_receive_help, useNA = "ifany"),
            Percentage = round(prop.table(table(no_coreside$cg_receive_help, useNA = "ifany")) * 100, 2))
      
      
      
  #### 3.) HH income (annual) --------------------------- 
      
      # By co-residence
      cbind(Frequency = table(coreside$inc_cat, useNA = "ifany"),
            Percentage = round(prop.table(table(coreside$inc_cat, useNA = "ifany")) * 100, 2))
      cbind(Frequency = table(no_coreside$inc_cat, useNA = "ifany"),
            Percentage = round(prop.table(table(no_coreside$inc_cat, useNA = "ifany")) * 100, 2))
      
      table(ACC$inc_cat, ACC$cg_coreside)
      
      
  #### 3.) Medicaid health insurance ---------------------------
    # MM9B7I: Informant has medicaid
      
      ACC$cg_medicaid_dum <- dplyr::case_when(
        ACC$MM9B7I == 1 ~ "1",
        ACC$MM9B7I == 2 ~ "0", 
        ACC$MM9B7I == 8 | ACC$MM9B7I == 9 ~ "Do not know/refused")
      
      table(ACC$cg_medicaid_dum) # 1 = 72; 0 = 568; NA: 7
      
      
      cbind(Frequency = table(ACC$MM9B7I, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$MM9B7I, useNA = "ifany")) * 100, 2))
      
      # By co-residence
      cbind(Frequency = table(coreside$MM9B7I, useNA = "ifany"),
            Percentage = round(prop.table(table(coreside$MM9B7I, useNA = "ifany")) * 100, 2))
      cbind(Frequency = table(no_coreside$MM9B7I, useNA = "ifany"),
            Percentage = round(prop.table(table(no_coreside$MM9B7I, useNA = "ifany")) * 100, 2))
      
      table(ACC$medicaid_dum)
      
      
      
      
      
  #### Function to calculate p-values for coreside vs non-coreside ---------------------------     
      
      # List of categorical variables
      categorical_vars <- c("cg_finc_strain", "cg_diff_help_kids", "cg_diff_house", "cg_diff_travel", "cg_other_unspec",   
                            "cg_receive_help", "inc_cat", "medicaid_dum")
      
      
      # Perform Chi-Square Tests and Collect Results
      chi_square_results <- lapply(categorical_vars, function(var) {
        chisq_test <- chisq.test(table(ACC[[var]], ACC$cg_coreside))
        data.frame(
          Variable = var,
          Chi_Square = round(chisq_test$statistic, 3),
          Degrees_of_Freedom = chisq_test$parameter,
          P_Value = round(chisq_test$p.value, 3)  # Round p-value to 3 decimal places
        )
      })
      
      # Combine results into a single data frame
      chi_square_results_df <- do.call(rbind, chi_square_results)
      
      # Display results
      chi_square_results_df
      
      
      
      
      
      
      # With error handling
      chi_square_results <- lapply(categorical_vars, function(var) {
        tryCatch({
          # Create complete cases only table
          contingency_table <- table(ACC[[var]][!is.na(ACC[[var]]) & !is.na(ACC$cg_coreside)], 
                                     ACC$cg_coreside[!is.na(ACC[[var]]) & !is.na(ACC$cg_coreside)])
          
          chisq_test <- chisq.test(contingency_table)
          data.frame(
            Variable = var,
            Chi_Square = round(chisq_test$statistic, 3),
            Degrees_of_Freedom = chisq_test$parameter,
            P_Value = round(chisq_test$p.value, 3)
          )
        }, error = function(e) {
          data.frame(
            Variable = var,
            Chi_Square = NA,
            Degrees_of_Freedom = NA,
            P_Value = NA
          )
        })
      })
      
      # Combine results into a single data frame
      chi_square_results_df <- do.call(rbind, chi_square_results)
      
      # Display results
      chi_square_results_df