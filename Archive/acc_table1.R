#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

    # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
    # PAA 2025
  
    # Cleaning
    # Gen measures

# Script created in RStudio ("Cranberry Hibiscus")
# updated 1/13/2025
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
          

    
#### Save df
write.csv(ACC, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/ACC.csv")
          
          
                 
          
  