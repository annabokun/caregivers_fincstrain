#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

  # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025

# Script created in RStudio ("Cranberry Hibiscus")
# updated 12/4/2024
#=======================================================================================================#


  #==============#
  #=== SET-UP ===#
  #==============#
  
    #### Set working directory --------------------------- 
    setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files")
  
  
  
    #### Download HEPESE waves 7 ---------------------------
      ## Source: https://www.icpsr.umich.edu/web/NACDA/series/546
  
  
  
    #### Import files ---------------------------
      ## Wave 7 
      load("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/Wave 7/Caregivers/36537-0002-Data.rda")
      load("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/Wave 7/Care Recipients/36537-0001-Data.rda")
  
  
    #### Rename files --------------------------- 
      w7_cr <- da36537.0001
      w7_cg <- da36537.0002
  
  
  
    #### Save new dfs
      write.csv(w7_cr, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/w7_cr.csv")
      write.csv(w7_cg, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/w7_cg.csv")


    #### Turn of scientific notation
      options(scipen = 999) 

    
      
      
  #================#
  #=== CLEANING ===#
  #================#
    
      
    #### 1.) Merge caregivers + recipients by Q_NO ---------------------------
      w7 <- merge(w7_cg, w7_cr, by = "Q_NO", all = TRUE)

    
    
    #### 2.) Generate analytic sample --------------------------- 
    
      ## Use RELSUBJ7I: Informant's relationship to respondent
        # RELSUBJ7I == 3	Son/daughter (including stepchildren)
        # RELSUBJ7I == 4	Children-in-law
          # Source: https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/RELSUBJ7I?archive=nacda

      ## 2a.) Check RELSUBJ7I
        table(w7_cg$RELSUBJ7I)
    
      ## 2b.) Restrict to children & step-children (==3) and children-in-law (==4)
        ACC <- wave7_all %>%
          filter(RELSUBJ7I == 3 | RELSUBJ7I == 4)
      
        length(unique(ACC$Q_NO)) # n = 659 unique adult children caregivers (ACCs)
        
        
        
        
  #================#
  #=== MEASURES ===#
  #================#
        
    #### 1.) DEPENDENT VARIABLE: FINANCIAL STRAIN --------------------------- 
      ## Index of 5 various financial strain "TT" variables  
        
        # TT57I_1: Difficult to help my own kids (per informant)
        # TT57I_2: Difficult for my own housing expenses (per informant)
        # TT57I_3: Difficult for me to travel/vacation (per informant)
        # TT57I_4: Difficult for me to do other things (per informant)
        # TT57SPECI: Difficult for other specified reasons (per informant)
        
        
      ## 1a.) Check the structure of the "TT" variables --------------------------- 
        # Note: TT57I_1-TT57I_4 all have the same categorical responses (below)
        
          # 1 = Helping out kids: 33
            table(ACC$TT57I_1)
        
          # 2 = Housing expenses: 51 
            table(ACC$TT57I_2) 
        
          # 3 = Travel/vacation: 48
            table(ACC$TT57I_3) 
            
          # 4 = Other: 14 
            table(ACC$TT57I_4)
            
        
        table(ACC$TT57SPECI)
          # FOOD: 1
          # HER HOUSE
          # HOME REPAIRS
          # It does not cause any difficulties: 1
          # PROTECTIVE SERVICES WON'T LET ME WORK FULL TIME: 2
          # UNIFORMS FOR SCHOOL
          # groceries: 1
          # no money left for personal expenses
        
        
        
      ## 1b.) Generate financial strain (binary) --------------------------- 
        # 1 = reports difficulty on any item; 0 = none
        ACC$cg_finc_strain <- ifelse(
          ACC$TT57I_1 == 1 | ACC$TT57I_2 == 2 | ACC$TT57I_3 == 3 | ACC$TT57I_4 == 4 |
            ACC$TT57SPECI %in% c("FOOD", "HER HOUSE", "HOME REPAIRS",
                                 "PROTECTIVE SERVICES WON'T LET ME WORK FULL TIME",
                                 "UNIFORMS FOR SCHOOL", "groceries", "no money left for personal expenses"), 1, 0)

        
      ## 1c.) Check binary: cg_finc_strain --------------------------- 
        # Convert NAs to zeroes 
        ACC$cg_finc_strain[is.na(ACC$cg_finc_strain)] <- 0 
        
        table(ACC$cg_finc_strain) # 1 = 77; 0 = 582
                                  # 77/659 = 0.1168437
                                  # ~12% of ACCs experience financial strain
        

        
      ## 1d.) Generate financial strain (dichotomized: low vs high)
        
        # Low strain: difficulty in 0-2 items AND no difficulties with essential expenses
        # High strain: difficulty in 3+ items OR difficulties with essential expenses
        
        # First create binary versions of each difficulty variable
          ACC$help_kids_diff <- ifelse(ACC$TT57I_1 == 1, 1, 0)
          ACC$housing_diff <- ifelse(ACC$TT57I_2 == 2, 1, 0)
          ACC$travel_diff <- ifelse(ACC$TT57I_3 == 3, 1, 0)
          ACC$other_diff <- ifelse(ACC$TT57I_4 == 4, 1, 0)
          
          # Check n 
            table(ACC$help_kids_diff) #33
          # Check freqs and %s
            cbind(
              Frequency = table(ACC$help_kids_diff, useNA = "ifany"),
              Percentage = round(prop.table(table(ACC$help_kids_diff, useNA = "ifany")) * 100, 1)) 
            
          table(ACC$housing_diff) #51; 
          cbind(
            Frequency = table(ACC$housing_diff, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$housing_diff, useNA = "ifany")) * 100, 1)) #
          
          table(ACC$travel_diff) #48
          cbind(
            Frequency = table(ACC$travel_diff, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$travel_diff, useNA = "ifany")) * 100, 1))
          
          table(ACC$other_diff) #14
          cbind(
            Frequency = table(ACC$other_diff, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$other_diff, useNA = "ifany")) * 100, 1))
          
          
          
          
        # For TT57SPECI, create binary for essential expenses (food, groceries, home repairs)
          ACC$other_essential_diff <- ifelse(grepl("FOOD|groceries|HOME REPAIRS|HOUSE", 
                                                   ACC$TT57SPECI, 
                                                   ignore.case = TRUE), 1, 0)
          table(ACC$other_essential_diff) #2
          
          
          
        # Calculate total number of difficulties
          ACC$total_diff <- rowSums(ACC[, c("help_kids_diff", "housing_diff", "travel_diff", #rowSums calculates the sum of values across specific columns for each row of a df
                                            "other_diff", "other_essential_diff")], 
                                    na.rm = TRUE)
        
          table(ACC$total_diff)  # reports how many people have 1, 2, 3, etc difficulties
          #   0   1   2   3   4   5 
          # 582  39  10  24   3   1 = 77 (matches cg_finc_strain indicator) 
          
            # Distribution
              hist(ACC$total_diff, main = "Distribution of Financial Difficulties", xlab = "Difficulties", col = "blue")
          
          
          
        # Essential expenses flag (housing, kids, food/groceries)
          ACC$has_essential_diff <- ifelse(ACC$help_kids_diff == 1 | 
                                             ACC$housing_diff == 1 | 
                                             ACC$other_essential_diff == 1, 1, 0)
          table(ACC$has_essential_diff) #59
          
          
        # Discretionary expenses flag (housing, kids, food/groceries)
          ACC$discretionary_diff <- ifelse(ACC$travel_diff==1 | ACC$other_diff == 1, 1, 0)
          table(ACC$discretionary_diff) #52  

        
        # Create dichotomized strain variable based on count threshold
          ACC$finc_strain_level <- ifelse(ACC$total_diff >= 2, "High", "Low")
          table(ACC$finc_strain_level) 
          # High  Low 
          # 38    621 

        
        # Create combined strain variable
          ACC$finc_strain_level <- ifelse(
            (ACC$total_diff >= 3) | (ACC$has_essential_diff == 1), 
            "High", 
            "Low")
          table(ACC$finc_strain_level) 
          # High  Low  
          # 59    600
          
          

    
#### Save new dfs
write.csv(ACC, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/ACC.csv")
          
          
                 
          
  #================#
  #=== TABLES =====#
  #================#
          
  #### TABLE 1: Demographic and Health Characteristics by ACC Financial Strain --------------------------- 
          
    ### A.) Generate w/financial strain and w/o financial strain samples 
          fs <- ACC %>%
            filter(cg_finc_strain==1)
          table(fs$cg_finc_strain) # n=77
          
          no_fs <- ACC %>%
            filter(cg_finc_strain==0)
          table(no_fs$cg_finc_strain) # n=582
          
          
    ## 1.) Age --------------------------- 
      summary(ACC$AGE7I)
      hist(ACC$AGE7I)
      
        # Financial strain 
          summary(fs$AGE7I)
          summary(no_fs$AGE7I)

      
          
          
    ## 2.) Female % ---------------------------   
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
          ACC$educ_cat <- cut(ACC$CEDUC7I_1, 
                              breaks = c(-Inf, 11, 12, 15, 16, Inf),  # Keep all breaks
                              labels = c("Less than HS", 
                                         "HS Graduate",
                                         "Some College",
                                         "Bachelor's+",  # Combined last two categories into one label
                                         "Bachelor's+"), # Same label repeated
                              right = TRUE)
      
      # 3b.) Check distribution
        cbind(
          Frequency = table(ACC$educ_cat, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$educ_cat, useNA = "ifany")) * 100, 2))
        
          # Financial strain
          cbind(Frequency = table(fs$educ_cat, useNA = "ifany"),
                Percentage = round(prop.table(table(fs$educ_cat, useNA = "ifany")) * 100, 2))
          cbind(Frequency = table(no_fs$educ_cat, useNA = "ifany"),
                Percentage = round(prop.table(table(no_fs$educ_cat, useNA = "ifany")) * 100, 2))
          
          # Chi-squared test 
          tab <- table(ACC$educ_cat, ACC$cg_finc_strain)
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
        
        # 10a.)  Create categorical income variable
        ACC$inc_cat <- dplyr::case_when(     
          ACC$LL3A7I == 1 ~ "1", # $0-$4,999
          ACC$LL3A7I == 2 ~ "2", # $5,000-$9,999
          ACC$LL3A7I == 3 ~ "3", # $10,000--$19,999
          ACC$LL3A7I == 5 ~ "4", # $20,000-$29,999
          ACC$LL3A7I %in% 6:8 ~ "5", # $30,000-$39,999
          ACC$LL3A7I == 98 | ACC$LL3A7I == 99 ~ "Do not know/refused")
          
          table(ACC$inc_cat)
        
        # Check distribution
          cbind(
            Frequency = table(ACC$inc_cat, useNA = "ifany"),
            Percentage = round(prop.table(table(ACC$inc_cat, useNA = "ifany")) * 100, 2))
          
            # Financial strain
            cbind(Frequency = table(fs$inc_cat, useNA = "ifany"),
                  Percentage = round(prop.table(table(fs$inc_cat, useNA = "ifany")) * 100, 2))
            cbind(Frequency = table(no_fs$inc_cat, useNA = "ifany"),
                  Percentage = round(prop.table(table(no_fs$inc_cat, useNA = "ifany")) * 100, 2))

        
    ## 11.) Living arrangement --------------------------- 
          
        # 11a.) Alone --------------------------- 
          cbind(
            Frequency = table(ACC$NHOUSE7I, useNA = "ifany"),
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
          
              
        # HOH --------------------------- 
              cbind(
                Frequency = table(ACC$HHREL7I, useNA = "ifany"),
                Percentage = round(prop.table(table(ACC$HHREL7I, useNA = "ifany")) * 100, 2))
              
              # Financial strain
              cbind(Frequency = table(fs$HHREL7I, useNA = "ifany"),
                    Percentage = round(prop.table(table(fs$HHREL7I, useNA = "ifany")) * 100, 2))
              cbind(Frequency = table(no_fs$HHREL7I, useNA = "ifany"),
                    Percentage = round(prop.table(table(no_fs$HHREL7I, useNA = "ifany")) * 100, 2))  
              
              
          
    ## 12.) Self-rated health ---------------------------  
          cbind(
            Frequency = table(ACC$I_HEALTH7I, useNA = "ifany"),
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
                ACC$heart_problems <- ifelse(ACC$I_JSTROK71I==1 | #stroke
                                            ACC$I_JSTROK75I==1 | #hp stroke
                                            ACC$I_ICARDI71I==1 | #ha
                                            ACC$I_ICARDI75I==1 | #hp ha
                                            ACC$I_ICARDI71I==1 | #hyper
                                            ACC$I_ICARDI75I==1 | #meds for hyper
                                            ACC$I_KHYPER74I==1, 1, 0)

                table(ACC$heart_problems)
                
                cbind(
                  Frequency = table(ACC$heart_problems, useNA = "ifany"),
                  Percentage = round(prop.table(table(ACC$heart_problems, useNA = "ifany")) * 100, 2))
                
                  # Financial strain
                  cbind(Frequency = table(fs$heart_problems, useNA = "ifany"),
                        Percentage = round(prop.table(table(fs$heart_problems, useNA = "ifany")) * 100, 2))
                  cbind(Frequency = table(no_fs$heart_problems, useNA = "ifany"),
                        Percentage = round(prop.table(table(no_fs$heart_problems, useNA = "ifany")) * 100, 2))  

                  
                  
                
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
                  
                
            # ANY chronic condition
                ACC$chronic_any <- ifelse(ACC$I_ARTHRHEU7I==1 | ACC$heart_problems==1 | ACC$I_MDIAB71I==1 | ACC$I_LCANCR71I==1, 1, 0)
                
                table(ACC$chronic_any)
                
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
        
        ACC$health_util <- case_when(
          ACC$CC1A7I == 1 | ACC$KK27I >= 12 ~ "High",    # Any hospital stay (yes) or monthly+ visits
          ACC$KK27I > 0 & ACC$CC1A7I == 2 ~ "Moderate",  # Some doctor visits but no hospital stay
          ACC$KK27I == 0 & ACC$CC1A7I == 2 ~ "None")      # No visits and no hospital stay
        
        
        # Convert to factor with ordered levels
        ACC$health_util <- factor(ACC$health_util, 
                                  levels = c("None", "Moderate", "High"))

        cbind(
          Frequency = table(ACC$health_util, useNA = "ifany"),
          Percentage = round(prop.table(table(ACC$health_util, useNA = "ifany")) * 100, 2)) 
          
          # Financial strain
          cbind(Frequency = table(fs$health_util, useNA = "ifany"),
                Percentage = round(prop.table(table(fs$health_util, useNA = "ifany")) * 100, 2))
          cbind(Frequency = table(no_fs$health_util, useNA = "ifany"),
                Percentage = round(prop.table(table(no_fs$health_util, useNA = "ifany")) * 100, 2)) 
                

          
    ## 16.) Hours/caregiving --------------------------- 
        
        # 16a.) ADL
          summary(ACC$RR3HRS7I)
          summary(fs$RR3HRS7I)
          summary(no_fs$RR3HRS7I)
        
        # 16b.) IADL
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
        table(ACC$RR6ALZ7I) #Informant provides care becuase respondent has Alzheimer`s
        table(ACC$TOTMMSE7) #MMSE score 
        table(ACC$PRXMENT7) #Proxy used bc respondent is mentally incapacitated or has memory problems such as dementia or Alzheimer’s Disease 
        
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




            
        
        

            
        ## Need to simplify some categorical variables ---------------------------
          # if they have categories other than the ones we're interested in, e.g., marital status 
          # necessary step for the t-test function to work
            
            # Married
              ACC$cg_married <- ifelse(ACC$CMARSTAT7I_1 == 1, 1,
                                    ifelse(ACC$CMARSTAT7I_1 == 9 | is.na(ACC$CMARSTAT7I_1), NA, 0))
              table(ACC$cg_married) #352

              
            # HOH
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
              ACC$cg_heart_probs <- ifelse(ACC$heart_problems == 1, 1,
                                          ifelse(is.na(ACC$heart_problems), NA, 0))
              table(ACC$cg_heart_probs) #235
              
            
            # Diabetes
              ACC$cg_diab <- ifelse(ACC$I_MDIAB71I == 1, 1,
                                    ifelse(ACC$I_MDIAB71I == 8 | ACC$I_MDIAB71I == 9 | is.na(ACC$I_MDIAB71I), NA, 0))
              table(ACC$cg_diab) #124
            
              
            # Cancer
              ACC$cg_cancer <- ifelse(ACC$I_LCANCR71I == 1, 1,
                                    ifelse(ACC$I_LCANCR71I == 8 | ACC$I_LCANCR71I == 9 | is.na(ACC$I_LCANCR71I), NA, 0))
              table(ACC$cg_cancer) #29
            
                
            
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
     
            
            # Years/caregiving
              ACC$cg_yrs_care <- factor(
                ifelse(ACC$RR7START7I %in% c(1, 2), "1 year or less",
                       ifelse(ACC$RR7START7I == 3, "1-2 years",
                              ifelse(ACC$RR7START7I %in% c(4, 7), "3-5 years",
                                     ifelse(ACC$RR7START7I %in% c(5, 6), "6+ years",
                                            ifelse(ACC$RR7START7I %in% c(8, 9), NA, NA))))),
                levels = c("1 year or less", "1-2 years", "3-5 years", "6+ years"))
              
                table(ACC$cg_yrs_care) #29
       
                
            # Dementia CR 
              ACC$cr_dementia <- ifelse(ACC$RR6ALZ7I==1 | ACC$TOTMMSE7 < 18 | ACC$PRXMENT7==4, 1, 
                                        ifelse(ACC$RR6ALZ7I == 8 | ACC$RR6ALZ7I == 9 | is.na(ACC$RR6ALZ7I), NA, 0))

              table(ACC$cr_dementia) #273
              
)

              

              
              
            
  #### Function to calculate p-values for financial strain vs no financial strain ---------------------------     
            calc_all_pvalues <- function(ACC) {
              
              # For continuous variables using the working approach
              age_p <- t.test(ACC$AGE7I[ACC$cg_finc_strain == 1], 
                              ACC$AGE7I[ACC$cg_finc_strain == 0])$p.value
              
              hours_ADL <- t.test(ACC$RR3HRS7I[ACC$cg_finc_strain == 1], 
                                  ACC$RR3HRS7I[ACC$cg_finc_strain == 0])$p.value
              
              hours_IADL <- t.test(ACC$RR5HRS7I[ACC$cg_finc_strain == 1], 
                                   ACC$RR5HRS7I[ACC$cg_finc_strain == 0])$p.value
              
              # For categorical variables, with error handling (Fisher's exact test)
              categorical_vars <- c("SEX7I", "educ_cat", "cg_married", "cg_HOH", 
                                    "inc_cat", "cg_live_alone", "cg_coreside", "cg_health", 
                                    "cg_arth", "cg_heart_probs", "cg_diab", "cg_cancer", 
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
            
  