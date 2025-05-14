#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

  # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025

    # TABLE 2: Household Composition and Structure by ACC's HOH Status

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
  
  #### Generate head of HH (HOH) samples
      # HOH 
        HOH <- ACC %>%
          filter(HHREL7I==1)
        
        length(unique(HOH$Q_NO)) # n = 287
  
      # Not HOH
        no_HOH <- ACC %>%
          filter(HHREL7I %in% 2:17)
        
        length(unique(no_HOH$Q_NO)) # n = 371
        
      # Gen binary (already done in acc_table1 script) 
        table(ACC$cg_HOH)
        
        
  #### Generate co-residence samples
        # Co-resident 
          coreside <- ACC %>%
            filter(HOUSEK_1==1)
          
          length(unique(coreside$Q_NO)) # n = 191
        
        # Not co-resident 
          no_coreside <- ACC %>%
            filter(HOUSEK_1 == 2)
          
          length(unique(no_coreside$Q_NO)) # n = 456
        
        # Gen binary (already done in acc_table1 script) 
          table(ACC$cg_coreside)
  
  
  
  
  #### 1.) Household size ---------------------------  
    ##: NHOUSE7I: Number of people living informant`s house
  
   summary(ACC$NHOUSE7I) # 3.241

     # By HOH status
       summary(HOH$NHOUSE7I) # 2.854
       summary(no_HOH$NHOUSE7I) # 3.544 
       
    # By co-residence
       summary(coreside$NHOUSE7I) # 3.623
       summary(no_coreside$NHOUSE7I) # 3.094 
       
   
  
   
   
   
  #### 2. Living Arrangement ---------------------------
   
   ## 2a.) Alone
    # NHOUSE7I: Number of people living informant's house
     
     # Total 
       cbind(Frequency = table(ACC$NHOUSE7I, useNA = "ifany"),
             Percentage = round(prop.table(table(ACC$NHOUSE7I, useNA = "ifany")) * 100, 2))

     # By HOH status
       cbind(Frequency = table(HOH$NHOUSE7I, useNA = "ifany"),
             Percentage = round(prop.table(table(HOH$NHOUSE7I, useNA = "ifany")) * 100, 2))
       cbind(Frequency = table(no_HOH$NHOUSE7I, useNA = "ifany"),
             Percentage = round(prop.table(table(no_HOH$NHOUSE7I, useNA = "ifany")) * 100, 2))
       
       
      # By co-residence
       cbind(Frequency = table(coreside$NHOUSE7I, useNA = "ifany"),
             Percentage = round(prop.table(table(coreside$NHOUSE7I, useNA = "ifany")) * 100, 2))
       cbind(Frequency = table(no_coreside$NHOUSE7I, useNA = "ifany"),
             Percentage = round(prop.table(table(no_coreside$NHOUSE7I, useNA = "ifany")) * 100, 2))

   
       
   
   ## 2b.) With parent
    # HOUSEK_1: Child 1 lives in same house (child 1 = informant/CG)
   
       # Total 
         cbind(Frequency = table(ACC$HOUSEK_1, useNA = "ifany"),
               Percentage = round(prop.table(table(ACC$HOUSEK_1, useNA = "ifany")) * 100, 2))
       
       # By HOH status
         cbind(Frequency = table(HOH$HOUSEK_1, useNA = "ifany"),
               Percentage = round(prop.table(table(HOH$HOUSEK_1, useNA = "ifany")) * 100, 2))
         cbind(Frequency = table(no_HOH$HOUSEK_1, useNA = "ifany"),
               Percentage = round(prop.table(table(no_HOH$HOUSEK_1, useNA = "ifany")) * 100, 2))
   
   
   #### 3.) Type of housing 
     # OO712: Type of housing where interview took plase (as per interviewer)
         
      # Total 
        cbind(Frequency = table(ACC$OO712, useNA = "ifany"),
               Percentage = round(prop.table(table(ACC$OO712, useNA = "ifany")) * 100, 2))
         
      # By HOH status
        cbind(Frequency = table(HOH$OO712, useNA = "ifany"),
              Percentage = round(prop.table(table(HOH$OO712, useNA = "ifany")) * 100, 2))
        cbind(Frequency = table(no_HOH$OO712, useNA = "ifany"),
              Percentage = round(prop.table(table(no_HOH$OO712, useNA = "ifany")) * 100, 2))
   

      # Restructure into categorical for t-tests
        table(ACC$OO712)
        
          ACC$cg_house_type <- factor(case_when(
            ACC$OO712 == 1 ~ "single-family house",
            ACC$OO712 == 3 ~ "apartment",
            ACC$OO712 == 2 | ACC$OO712 %in% 4:7 ~ "other"))
          
          table(ACC$cg_house_type) 
          
          
          #### Regenerate HOH and non-HOH samples bc of new variable
          
          # Total 
            cbind(Frequency = table(ACC$cg_house_type, useNA = "ifany"),
                  Percentage = round(prop.table(table(ACC$cg_house_type, useNA = "ifany")) * 100, 2))
          
          # By HOH status
            cbind(Frequency = table(HOH$cg_house_type, useNA = "ifany"),
                  Percentage = round(prop.table(table(HOH$cg_house_type, useNA = "ifany")) * 100, 2))
            cbind(Frequency = table(no_HOH$cg_house_type, useNA = "ifany"),
                  Percentage = round(prop.table(table(no_HOH$cg_house_type, useNA = "ifany")) * 100, 2))
          
          
          
  
  
  #### Function to calculate p-values for HOH vs non HOH ---------------------------     
          
          # Perform statistical tests
          
          # 1. Household size (continuous) - t-test or ANOVA
            household_size_test <- t.test(NHOUSE7I ~ cg_HOH, data = ACC)
          
          # 2. Living Arrangement - Chi-square tests
            # For living alone
            live_alone_test <- chisq.test(table(ACC$cg_HOH, ACC$cg_live_alone))
          
          # For coresiding
            coreside_test <- chisq.test(table(ACC$cg_HOH, ACC$cg_coreside))
          
          # 3. Housing type - Chi-square test
            housing_type_test <- chisq.test(table(ACC$cg_HOH, ACC$cg_house_type))
            table(ACC$cg_HOH, ACC$cg_house_type)
          
          # Function to format p-values
            format_pvalue <- function(p) {
              if (p < 0.001) return("< 0.001")
              return(sprintf("%.3f", p))
            }
          
          # Create summary table with test statistics and p-values
            summary_table <- data.frame(
              Variable = c("Household size", "Living alone", "Coresiding", "Housing type"),
              Test = c("t-test", "Chi-square", "Chi-square", "Chi-square"),
              Statistic = c(
                sprintf("%.3f", household_size_test$statistic),
                sprintf("%.3f", live_alone_test$statistic),
                sprintf("%.3f", coreside_test$statistic),
                sprintf("%.3f", housing_type_test$statistic)
              ),
              `P-value` = c(
                format_pvalue(household_size_test$p.value),
                format_pvalue(live_alone_test$p.value),
                format_pvalue(coreside_test$p.value),
                format_pvalue(housing_type_test$p.value)
              )
            )
          
          # Print descriptive statistics
            cat("\nDescriptive Statistics:\n")
          
          # For household size
            cat("\nHousehold Size:\n")
            ACC %>%
              group_by(cg_HOH) %>%
              summarise(
                n = n(),
                mean = mean(NHOUSE7I, na.rm = TRUE),
                sd = sd(NHOUSE7I, na.rm = TRUE)
              ) %>%
              print()
          
          # For living arrangements
            cat("\nLiving Arrangements:\n")
            # Living alone frequencies
            cat("\nLiving Alone:\n")
            table(ACC$cg_HOH, ACC$cg_live_alone) %>%
              prop.table(margin = 1) %>%
              round(4) * 100 %>%
              print()
          
          # Coresiding frequencies
            cat("\nCoresiding:\n")
            table(ACC$cg_HOH, ACC$cg_coreside) %>%
              prop.table(margin = 1) %>%
              round(4) * 100 %>%
              print()
          
          # Housing type
            cat("\nHousing Type:\n")
            table(ACC$cg_HOH, ACC$cg_house_type) %>%
              prop.table(margin = 1) %>%
              round(4) * 100 %>%
              print()
          
          # Print statistical test results
            cat("\nStatistical Test Results:\n")
            print(summary_table)