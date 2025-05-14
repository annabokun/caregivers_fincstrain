#=======================================================================================================#
# Anna Bokun: bokun001@umn.edu
# Sociology PhD Candidate, University of Minnesota
# Postdoc, Population Research Center, University of Texas-Austin

  # ICAA 2024
  # "Living with An Aging Parent in the U.S. and Mexico (or California and Texas): Who benefits, Mom, Dad, (Elderly Parent) or her Children?”  
  # Authors: Anna Bokun, Jacqui Angel, Sunshine Rote, and Phil Cantu

    # Demographic Tables

# Script created in RStudio ("Chocolate Cosmos")
# updated 7/8/2024
#=======================================================================================================#



  #==============#
  #=== SET-UP ===#
  #==============#

    #### Load packages 
      library(tidyverse)
      library(vtable)
      library(tabulator)
      
   
    
    #### Load data 
      # HEPESE, wave 7 (2010-2011): https://www.icpsr.umich.edu/web/NACDA/studies/36537  
      # Google Drive --> UT-Austin --> ICAA 2024 --> Caregivers - Housing Health Paradox --> Data
      # Originally, Suyoung Kim shared the files with me
        # 01 = recipients
        # 02 = caregivers 
      
      recipients <- read.delim("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/36537_0001_Data.tsv")
      caregivers <- read.delim("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/36537_0002_Data.tsv")
  
    
    #### See variables 
      vtable(caregivers)
      vtable(recipients)
  

  
    
    
    
    
    
  #===================#
  #=== HEPESE DATA ===#
  #===================#

    ## Main variables (more on this on the relevant_variables tab --> https://docs.google.com/spreadsheets/d/1yRhiz0OiPTiGEzBcbvrXgOWqxxR7I_C2PCkNnpZs-fI/edit?usp=sharing
    
      # NHOUSE7I: Number of people living in informant's house --- https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/NHOUSE7I?archive=nacda 
        
      # RELSUBJ7I: Informant`s relationship to respondent --- https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/RELSUBJ7I?archive=nacda 
        
      # LL3A7i: Informant`s yearly household income -- https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/LL3A7I?archive=nacda
    
      # Informant's living arrangement:
          # [NHOUSE7i] -- B1. How many people live in this household?
          # [HHREL7i] -- B2. Who is the head of this household, what is their relationship to you?
        
      # Informant's marital status
          # Marital status is available in the informant file only if the caregiver is a child of the informant. 
          # I think its reasonable to include marital status of child caregivers but as a subset of the total sample. 
          # Either that or subset the sample to people who had children caregivers.

      
    ## Is child, parent, or other informant head or non-head of household?

    ## Phil (5/30/24): I think for the sake of preliminary analysis you should focus on 2 groups:
        # 1.) Older adults who live as head of household or spouse of HOH
        # 2.) Those who live in a house with others as HOH            
            
            
           
      
      
      
      
      
  #==============#
  #=== TABLES ===#
  #==============#

    # Tables are located here: https://docs.google.com/spreadsheets/d/1yRhiz0OiPTiGEzBcbvrXgOWqxxR7I_C2PCkNnpZs-fI/edit?usp=sharing
  
      
    
  #### TABLE 1. SUMMARY SAMPLE STATS, BY HEADSHIP STATUS ---------------------------
  
      
    ## Panel A: Caregiver/informant demographics --------------------------- 
      
      ## Column A1.) HOH or spouse of HOH ---------------------------
    
          # See relationships       
            relate <- caregiver %>%
              tab(HHREL7I)
      
          
          # A1a.) Age 
            mean_age <- caregiver %>%
              filter(HHREL7I == 1 | HHREL7I == 2) %>%
              summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 57.85604
            
              # Alternative code 
                mean_agex <- mean(caregiver$AGE7I[caregiver$HHREL7I %in% c(1, 2)], na.rm = TRUE)
                  
                  
                            
          # A1b.) % Female
              # 1	= Male
              # 2	= Female
                      
              # Subset the df to only include rows where HOH or spouse of HOH
                HOH_spouse_of_HOH <- caregiver[caregiver$HHREL7I %in% c(1, 2), ]
              
              # Calculate the % of women
                female_percent <- mean(HOH_spouse_of_HOH$SEX7I == 2, na.rm = TRUE) * 100
                  # 77.12519

                
                
          # A1c.) HH income 
            inc <- caregiver %>%
              tab(LL3A7I)
            
                # 1	$0-$4,999
                # 2	$5,000-$9,999
                # 3	$10,000-$14,999
                # 4	$15,000-$19,999
                # 5	$20,000-$29,999
                # 6	$30,000-$39,999
                # 7	$40,000-$49,999
                # 8	$50,000 & Over
            
            # Midpoint coding (double-check with team on this) 
              income_categories <- c(2500, 7500, 12500, 17500, 25000, 35000, 45000, 75000)
            
            # Subset the data frame to only include rows where HHREL7I == 1 or HHREL7I == 2
              subset_df <- caregiver[caregiver$HHREL7I %in% c(1, 2), ]
            
            # Calculate the mean income
              mean_income <- mean(income_categories[subset_df$HHREL7I], na.rm = TRUE)
              # 4,416.5378
              
                    # OR just mean of the category 
                            # Filter the data frame based on the conditions
                            filtered_df <- caregiver %>%
                              filter((HHREL7I == 1 | HHREL7I == 2) & !(LL3A7I %in% c(98, 99)))
                            
                            # Calculate the mean income category
                            mean_income_category <- filtered_df %>%
                              summarise(mean_income_category = mean(LL3A7I, na.rm = TRUE)) # 4.831919
                            
      
              
          # A1d.) HH size  
            mean_hh_size <- caregiver %>%
              filter(HHREL7I == 1 | HHREL7I == 2) %>%
              summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 3.044822

            
                            

                            
                            
                            
                                        
    ## Column A2.) Non HOH ---------------------------
                            
        # A2a.) Age 
            mean_age <- caregiver %>%
              filter(!(HHREL7I == 1 | HHREL7I == 2)) %>%
              summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 51.17329
            
            # Alternative code 
              mean_agex <- mean(caregiver$AGE7I[caregiver$HHREL7I %in% c(1, 2)], na.rm = TRUE)
                
          
          
          
          
      # A2b.) % Female 
          # 1	= Male
          # 2	= Female
          
        # Subset the df to only include rows where HOH or spouse of HOH
          HOH_spouse_of_HOH <- caregiver[!(caregiver$HHREL7I %in% c(1, 2)), ]
          
        # Calculate the % of women
          female_percent <- mean(HOH_spouse_of_HOH$SEX7I == 2, na.rm = TRUE) * 100
          # 66.1870
          

          
          
      # A2c.) HH income 
          inc <- caregiver %>%
            tab(LL3A7I)
          
          # 1	$0-$4,999
          # 2	$5,000-$9,999
          # 3	$10,000-$14,999
          # 4	$15,000-$19,999
          # 5	$20,000-$29,999
          # 6	$30,000-$39,999
          # 7	$40,000-$49,999
          # 8	$50,000 & Over
          
          # Midpoint coding 
            income_categories <- c(2500, 7500, 12500, 17500, 25000, 35000, 45000, 75000)
          
          # Subset the data frame to only include rows where HHREL7I == 1 or HHREL7I == 2
            subset_df <- caregiver[!(caregiver$HHREL7I %in% c(1, 2)), ]
          
          # Calculate the mean income
            mean_income <- mean(income_categories[subset_df$HHREL7I], na.rm = TRUE)
            # 32295.2586
  
            
            # OR just mean of the category 
                        # Filter the data frame based on the conditions
                        filtered_df <- caregiver %>%
                          filter((!HHREL7I == 1 | HHREL7I == 2) & !(LL3A7I %in% c(98, 99)))
                        
                        # Calculate the mean income category
                        mean_income_category <- filtered_df %>%
                          summarise(mean_income_category = mean(LL3A7I, na.rm = TRUE)) # 4.557447
                      
                      
                      
                        
                        
      # A2d.) HH size 
          mean_hh_size <- caregiver %>%
            filter(!HHREL7I == 1 | HHREL7I == 2) %>%
            summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 3.554286                             
                            
                            
                            
                            
                            
                            
                            
                            
                            
    #### Panel B.) Care recipient demographics --------------------------- 
                        
      ## Column B1.) HOH or spouse of HOH ---------------------------
                            
        # See Relationship to head of household  
          relate_recipients <- recipient %>%
            tab(HHREL7)  
                            
          # n of HOH or spouse of HOH care recipients 
          recipient_hoh_spouse <- recipient %>%
            filter(HHREL7 == 1 | HHREL7 == 2) # n = 849
      
      
                                
          # B1a.) Age 
            mean_age <- recipient %>%
              filter(HHREL7 == 1 | HHREL7 == 2) %>%
              summarise(mean_age = mean(AGE7, na.rm = TRUE)) # 85.71025
            
              # Alternative code 
                mean_agex <- mean(recipient$AGE7[recipient$HHREL7 %in% c(1, 2)], na.rm = TRUE)
              
      
                              
          # B1b.) % Female 
              # 1	= Male
              # 2	= Female
                      
              # Subset the df to only include rows where HOH or spouse of HOH
                HOH_spouse_of_HOH <- recipient[recipient$HHREL7 %in% c(1, 2), ]
              
              # Calculate the % of women
                female_percent <- mean(HOH_spouse_of_HOH$SEX7 == 2, na.rm = TRUE) * 100
                  # 62.308
              
              
                  
          # B1c.) HH income 
            inc <- recipient %>%
              tab(LL3A7I)
            
                # 1	$0-$4,999
                # 2	$5,000-$9,999
                # 3	$10,000-$14,999
                # 4	$15,000-$19,999
                # 5	$20,000-$29,999
                # 6	$30,000-$39,999
                # 7	$40,000-$49,999
                # 8	$50,000 & Over
            
            # Midpoint coding 
              income_categories <- c(2500, 7500, 12500, 17500, 25000, 35000, 45000, 75000)
            
            # Subset the data frame to only include rows where HHREL7I == 1 or HHREL7I == 2
              subset_df <- caregiver[caregiver$HHREL7I %in% c(1, 2), ]
            
            # Calculate the mean income
              mean_income <- mean(income_categories[subset_df$HHREL7I], na.rm = TRUE)
              # 4,416.5378
              
                    # OR just mean of the category 
                            # Filter the data frame based on the conditions
                            filtered_df <- caregiver %>%
                              filter((HHREL7I == 1 | HHREL7I == 2) & !(LL3A7I %in% c(98, 99)))
                            
                            # Calculate the mean income category
                            mean_income_category <- filtered_df %>%
                              summarise(mean_income_category = mean(LL3A7I, na.rm = TRUE)) # 4.831919
         
              
              
          # B1d.) HH size 
            mean_hh_size <- recipient %>%
              filter(HHREL7 == 1 | HHREL7 == 2) %>%
              summarise(mean_hh_size = mean(NHOUSE7, na.rm = TRUE)) # 2.055425  
                                   

                                                        
          # B1e.) Nativity 
                # 1 = Yes
                # 0 = No
            count_nativity <- recipient %>% 
              filter(USBORN7 == 1 & (HHREL7 == 1 | HHREL7 == 2))
                            # N = 478
            length(unique(recipient$Q_NO)) # n = 1078
                            # 478/1078 = 0.4434137 are U.S. born 
     
            
            
         # B1f.) Married 
            # 1 = Yes
            married <- recipient %>% 
              filter(MARSTAT7 == 1 & (HHREL7 == 1 | HHREL7 == 2))
            # N = 305
            length(unique(recipient$Q_NO)) # n = 1078
            # 305/1078 = 0.2829314 are married
                              
     
            
            
            
            
                   
                       
  #### Column B2.) Non HOH --------------------------- 
    
        # See relationships       
          relate <- caregiver %>%
            tab(HHREL7I)
            
          # n of NON HOH or spouse of HOH care recipients 
            recipient_hoh_spouse <- recipient %>%
              filter(!HHREL7 == 1 | HHREL7 == 2) # n = 323 
     
     
                 
          # B2a.) Age 
            mean_age <- recipient %>%
              filter(!HHREL7 == 1 | HHREL7 == 2) %>%
              summarise(mean_age = mean(AGE7, na.rm = TRUE)) # 85.89474
            
                # Alternative code 
                  mean_agex <- mean(recipient$AGE7[recipient$HHREL7 %in% c(1, 2)], na.rm = TRUE)
              
              
                              
          # B2b.) % Female 
                # 1	= Male
                # 2	= Female
                      
              # Subset the df to only include rows where HOH or spouse of HOH
                HOH_spouse_of_HOH <- recipient %>%
                  filter(!HHREL7 == 1 | HHREL7 == 2) # n = 323
              
              # Calculate the % of women
                female_percent <- mean(HOH_spouse_of_HOH$SEX7 == 2, na.rm = TRUE) * 100
                  # 80.4953
              
        
                
          # B2c.) HH income  
            inc <- recipient %>%
              tab(LL3A7I)
            
                # 1	$0-$4,999
                # 2	$5,000-$9,999
                # 3	$10,000-$14,999
                # 4	$15,000-$19,999
                # 5	$20,000-$29,999
                # 6	$30,000-$39,999
                # 7	$40,000-$49,999
                # 8	$50,000 & Over
            
            # Midpoint coding 
              income_categories <- c(2500, 7500, 12500, 17500, 25000, 35000, 45000, 75000)
            
            # Subset the data frame to only include rows where HHREL7I == 1 or HHREL7I == 2
              subset_df <- caregiver[caregiver$HHREL7I %in% c(1, 2), ]
            
            # Calculate the mean income
              mean_income <- mean(income_categories[subset_df$HHREL7I], na.rm = TRUE)
              # 4,416.5378
              
                    # OR just mean of the category 
                            # Filter the data frame based on the conditions
                            filtered_df <- caregiver %>%
                              filter((HHREL7I == 1 | HHREL7I == 2) & !(LL3A7I %in% c(98, 99)))
                            
                            # Calculate the mean income category
                            mean_income_category <- filtered_df %>%
                              summarise(mean_income_category = mean(LL3A7I, na.rm = TRUE)) # 4.831919
                            
         
              
          # B2d.) HH size 
            mean_hh_size <- recipient %>%
              filter(!HHREL7 == 1 | HHREL7 == 2) %>%
              summarise(mean_hh_size = mean(NHOUSE7, na.rm = TRUE)) # 3.606918 
                                    
                           
             
          # B2e.) Nativity 
                # 1 = Yes
                # 0 = No
            count_nativity <- recipient %>% 
              filter(USBORN7 == 1 & (!HHREL7 == 1 | HHREL7 == 2)) # n = 168
            length(unique(recipient$Q_NO)) # n = 1078
                            # 168/1078 = 0.1558442 are U.S. born 
            
            
            
            
         # B2f.) Married 
            # 1 = Yes
            married <- recipient %>% 
              filter(MARSTAT7 == 1 & (!HHREL7 == 1 | HHREL7 == 2)) # n = 133
            length(unique(recipient$Q_NO)) # n = 1078
            # 133/1078 = 0.1233766 are married
                              
      
                            
                            
  
            
            
            
            
   
            
            
            
            
  #### TABLE 2. ADULT CHILDREN AS CAREGIVERS, DEMOGRAPHICS ---------------------------       
  
    ## A.) All caregivers ---------------------------   
            
      ## How many unique caregivers?
        length(unique(caregivers$Q_NO)) # n = 925
            
            
            
      ## 2A1.) Age
          # AGE7I
          mean_age <- caregivers %>%
            summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 55.8474

     
          
      ## 2A2.) % Female
              # SEX7I
                # 1	= Male
                # 2	= Female

          female_percent <- mean(caregivers$SEX7I == 2, na.rm = TRUE) * 100
            print(female_percent) # 73.83784

        
    
                    
      ## 2A3.) HH income
            # LL3A7I: Informant's yearly household income
            
            inc <- caregivers %>%
              tab(LL3A7I)
            
                # 1	$0-$4,999
                # 2	$5,000-$9,999
                # 3	$10,000-$14,999
                # 4	$15,000-$19,999
                # 5	$20,000-$29,999
                # 6	$30,000-$39,999
                # 7	$40,000-$49,999
                # 8	$50,000 & Over
                # 98 Do not know
                # 99 Refused 



                
           
                            
      ## 2A4.) HH size
          # NHOUSE7I: Number of people living informant`s house
            
              mean_hh_size <- caregivers %>%
                summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 3.205405

            
     
              
      ## 2A5.) % Married 
        # CMARSTAT7I_1: CMARSTAT7I_16 --> Marital status of respondent`s child #1 (per informant)
        # Only available in the informant file if the caregiver is a child of the informant      
              
              # 1	Married
              # 2	Separated
              # 3	Divorced
              # 4	Widowed
              # 5	Never married
              # 9	Do not know / refused
           
              married <- caregivers %>%
                tab(CMARSTAT7I_1) # 53%

    
            
            
            
            
            
            
            
            
            
            
            
            
            
    ## B&C.) Adult children and children-in-law caregivers ---------------------------
              
              
      ## Who is an adult child caregiver?
            
          # Sons/daughters 
            adult_child_caregiver <- caregivers %>%
              filter(RELSUBJ7I == 3)
                
            length(unique(adult_child_caregiver$Q_NO)) # n = 629
                
          # Sons/daughters PLUS sons-in-law/daughters-in-law
            adult_child_caregiver_inlaw <- caregivers %>%
              filter(RELSUBJ7I == 3 | RELSUBJ7I == 4)
            
            length(unique(adult_child_caregiver_inlaw$Q_NO)) # n = 659
            
          
              # RELSUBJ7I: Informant`s relationship to respondent
                #01 = Head of Household (for B2 only)
                #02 = Spouse 
                #03 = Son/Daughter (including Stepchildren)
                #04 = Son-In-Law/Daughter-In-Law 13 = Friend
                #05 = Grandchild 14 = Boarder or Roomer
                #06 = Parent 15 = Paid Employee
                #07 = Brother or Sister 16 = All Others (SPECIFY):
                #08 = Nephew or Niece
                #09 = Cousin 17 = Sister/Brother In-Law
                #10 = Aunt/Uncle 98 = Don't Know
                #11 = Great Grandchild 99 = Refused
                #12 = Other Relative (SPECIFY):
                #13 = Friend
                #14 = Boarder or Roomer
                #15 = Paid Employee
                #16 = All Others (SPECIFY):
                #17 = Sister/Brother In-Law
                #98 = Don't Know
                #99 = Refused
            
          
          
      ## 2a.) Age
          # AGE7I
        
        # Adult children
          mean_age <- adult_child_caregiver %>%
            filter(RELSUBJ7I == 3) %>%
            summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 55.04299
            
        # Adult children + children-in-law
          mean_age <- adult_child_caregiver_inlaw %>%
            filter(RELSUBJ7I == 3 | RELSUBJ7I == 4) %>%
            summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 54.77356
            
         
      
          
          
      # 2b.) % Female
              # SEX7I
                # 1	= Male
                # 2	= Female
          
          # Adult children
            female_percent <- mean(adult_child_caregiver$SEX7I == 2, na.rm = TRUE) * 100
            print(female_percent) # 69.47536
          
          # Adult children + children-in-law
            female_percent <- mean(adult_child_caregiver_inlaw$SEX7I == 2, na.rm = TRUE) * 100
            print(female_percent) # 70.7132
        
   
                
        
                    
      # 2c.) HH income
            # LL3A7I: Informant's yearly household income
            
            inc <- caregivers %>%
              tab(LL3A7I)
            
                # 1	$0-$4,999
                # 2	$5,000-$9,999
                # 3	$10,000-$14,999
                # 4	$15,000-$19,999
                # 5	$20,000-$29,999
                # 6	$30,000-$39,999
                # 7	$40,000-$49,999
                # 8	$50,000 & Over
                # 98 Do not know
                # 99 Refused 
            
            # Adult children
                inc_kids <- adult_child_caregiver %>%
                  tab(LL3A7I, round = 3)
      
            # Adult children + children-in-law
                inc_kids_plus <- adult_child_caregiver_inlaw %>%
                  tab(LL3A7I, round = 3) 
                
                
                
                
                            
      # 2d.) HH size
          # NHOUSE7I: Number of people living informant`s house
                
          # Adult children
              mean_hh_size <- adult_child_caregiver %>%
                summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 3.18124
      
            # Adult children + children-in-law
              mean_hh_size <- adult_child_caregiver_inlaw %>%
                summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 3.241275
            
            
          
              
              
    # 2e.) % Married 
        # CMARSTAT7I_1: CMARSTAT7I_16 --> Marital status of respondent`s child #1 (per informant)
        # Only available in the informant file if the caregiver is a child of the informant      
              
              # 1	Married
              # 2	Separated
              # 3	Divorced
              # 4	Widowed
              # 5	Never married
              # 9	Do not know / refused
              
              
        # Tally 
          married_ac <- adult_child_caregiver_inlaw %>%
            tab(CMARSTAT7I_1)
              
          
        # Generate indicator based on ICPSR guidelines: https://www.icpsr.umich.edu/web/pages/instructors/setups/exercises/notes/recode-marital-status.html
              # 1 = Married/widowed 
              # 0 = Separated, divorced, never married, single
       
              adult_child_caregiver_inlaw$married <- as.numeric(
                apply(adult_child_caregiver_inlaw[, paste0("CMARSTAT7I_", 1:16)], 1, function(row) {
                  any(row %in% c(1, 4))
                })
              )
              
              table(adult_child_caregiver_inlaw$married) # 0:29 | 1: 630 

              married_ac_inlaws <- adult_child_caregiver_inlaw %>%
                tab(married) # 0.96 = married

            
          table(adult_child_caregiver_inlaw$CMARSTAT7I_1)

          
          
          
          
          

  

    

  
  
          
          
          
          
          
          
          
          
          
  

  #### TABLE 3a. TIME SPENT CAREGIVING (ADULT CHILDREN CAREGIVERS), BY AGE GROUPS ---------------------------
              
    ## RR3HRS7I: Number of hours per-day providing ADL-related assistance to respondent --> https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/RR3HRS7I?archive=nacda
      ADLs <- adult_child_caregiver %>%
        tab(RR3HRS7I)
      ADLs <- adult_child_caregiver_inlaw %>%
        tab(RR3HRS7I)
 
              
      
    ## RR5HRS7I: Number of hours per-day providing iADL-related assistance to respondent --> https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/RR5HRS7I?archive=nacda
      iADLs <- adult_child_caregiver %>%
        tab(RR5HRS7I)
      iADLs <- adult_child_caregiver_inlaw %>%
        tab(RR5HRS7I)
           
         
      
    ## Age x ADL time
      table(adult_child_caregiver_inlaw$AGE7I, adult_child_caregiver_inlaw$RR3HRS7I)
    
      
      
    ## How many in each age group? 
      
      # Count 25-44
      acc_25_44 <- adult_child_caregiver_inlaw %>%
        filter(AGE7I %in% 25:44) # n = 77
      
      # Count 25-34
        acc_25_34 <- adult_child_caregiver_inlaw %>%
          filter(AGE7I %in% 25:34) # n = 13
      
      # Count 35-44
        acc_35_44 <- adult_child_caregiver_inlaw %>%
          filter(AGE7I %in% 35:44) # n = 64
        
      # Count 45-54
        acc_45_54 <- adult_child_caregiver_inlaw %>%
          filter(AGE7I %in% 45:54) # n = 224
        
      # Count 55-64
        acc_55_64 <- adult_child_caregiver_inlaw %>%
          filter(AGE7I %in% 55:64) # n = 286  
        
      # Count 65-75
        acc_65_75 <- adult_child_caregiver_inlaw %>%
          filter(AGE7I %in% 65:75) # n = 71
        
      # Count 25-75 (total)
        acc_25_75 <- adult_child_caregiver_inlaw %>%
          filter(AGE7I %in% 25:75) # n = 658
      
      
      
        
       
        
        
        
    ## 3.) 25-44 (early adults) ---------------------------

      # Provide assistance with ADLs (RR3HRS7I)
          ADL_25_44 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 25:44) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
        # Provide assistance with iADLS (RR5HRS7I)
          iADL_25_44 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 25:44) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
                  
          
        # Provide Care to an Adult for a Condition Related to Aging in the Last Three Months
        
        
        
        
        
        
                 
    ## 3a.) 25-34 ---------------------------

      # Provide assistance with ADLs (RR3HRS7I)
          ADL_25_34 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 25:34) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
        # Provide assistance with iADLS (RR5HRS7I)
          iADL_25_34 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 25:34) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
                  
          
        # Provide Care to an Adult for a Condition Related to Aging in the Last Three Months
              
          
              
              
              
    ## 3b.) 35-44 ---------------------------          
              
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_35_44 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 35:44) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_35_44 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 35:44) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)     
       
          
          
          
          
          
          
    ## 3c.) 45-54 ---------------------------          
              
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_45_54 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 45:54) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_45_54 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 45:54) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)        
              
             
          
          
          
          
          
          
      ## 3d.) 55-64 ---------------------------          
              
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_55_64 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 55:64) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_55_64 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 55:64) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100) 
          
          
          
          
          
          
    ## 3d.) 65-75 ---------------------------          
          
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_65_75 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 65:75) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_65_75 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 65:75) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100) 
          
          
          
          
          
      ## 3e.) 25-75 ---------------------------          
          
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_25_75 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 25:75) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_25_75 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 25:75) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100) 
          
          
          
          
          
          
          
          
  #### TABLE 3b. TIME SPENT CAREGIVING (ADULT CHILDREN CAREGIVERS), BY NATIVITY STATUS ---------------------------         
          
    table(recipients$USBORN7) # 0 = 490 | 1 = 588 
          
          
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_us_born <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 25:75) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_25_75 <- adult_child_caregiver_inlaw %>%
            filter(AGE7I %in% 25:75) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100) 
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
  #### TABLE 3a. TIME SPENT CAREGIVING (ALL CAREGIVERS), BY AGE GROUPS ---------------------------
              
    ## RR3HRS7I: Number of hours per-day providing ADL-related assistance to respondent --> https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/RR3HRS7I?archive=nacda
      ADLs <- caregivers %>%
        tab(RR3HRS7I)

 
              
    ## RR5HRS7I: Number of hours per-day providing iADL-related assistance to respondent --> https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/RR5HRS7I?archive=nacda
      iADLs <- caregivers %>%
        tab(RR5HRS7I)
           
         
      
    ## Age x ADL time
      table(caregivers$AGE7I, caregivers$RR3HRS7I)
    
      
      
    ## How many in each age group? 
      
      # Count 25-44
      acc_25_44 <- caregivers %>%
        filter(AGE7I %in% 25:44) # n = 138
      
      # Count 25-34
        acc_25_34 <- caregivers %>%
          filter(AGE7I %in% 25:34) # n = 40
      
      # Count 35-44
        acc_35_44 <- caregivers %>%
          filter(AGE7I %in% 35:44) # n = 98
        
      # Count 45-54
        acc_45_54 <- caregivers %>%
          filter(AGE7I %in% 45:54) # n = 260
        
      # Count 55-64
        acc_55_64 <- caregivers %>%
          filter(AGE7I %in% 55:64) # n = 332  
        
      # Count 65-75
        acc_65_75 <- caregivers %>%
          filter(AGE7I %in% 65:75) # n = 119
        
      # Count 25-75 (total)
        acc_25_75 <- caregivers %>%
          filter(AGE7I %in% 25:75) # n = 849
      
      
      
        
       
        
        
        
    ## 3.) 25-44 (early adults) ---------------------------

      # Provide assistance with ADLs (RR3HRS7I)
          ADL_25_44 <- caregivers %>%
            filter(AGE7I %in% 25:44) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
        # Provide assistance with iADLS (RR5HRS7I)
          iADL_25_44 <- caregivers %>%
            filter(AGE7I %in% 25:44) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
                  
          
        # Provide Care to an Adult for a Condition Related to Aging in the Last Three Months
        
        
        
        
        
        
                 
    ## 3a.) 25-34 ---------------------------

      # Provide assistance with ADLs (RR3HRS7I)
          ADL_25_34 <- caregivers %>%
            filter(AGE7I %in% 25:34) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
        # Provide assistance with iADLS (RR5HRS7I)
          iADL_25_34 <- caregivers %>%
            filter(AGE7I %in% 25:34) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
                  
          
        # Provide Care to an Adult for a Condition Related to Aging in the Last Three Months
              
          
              
              
              
    ## 3b.) 35-44 ---------------------------          
              
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_35_44 <- caregivers %>%
            filter(AGE7I %in% 35:44) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_35_44 <- caregivers %>%
            filter(AGE7I %in% 35:44) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)     
       
          
          
          
          
          
          
    ## 3c.) 45-54 ---------------------------          
              
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_45_54 <- caregivers %>%
            filter(AGE7I %in% 45:54) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_45_54 <- caregivers %>%
            filter(AGE7I %in% 45:54) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)        
              
             
          
          
          
          
          
          
      ## 3d.) 55-64 ---------------------------          
              
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_55_64 <- caregivers %>%
            filter(AGE7I %in% 55:64) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_55_64 <- caregivers %>%
            filter(AGE7I %in% 55:64) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100) 
          
          
          
          
          
          
    ## 3d.) 65-75 ---------------------------          
          
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_65_75 <- caregivers %>%
            filter(AGE7I %in% 65:75) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_65_75 <- caregivers %>%
            filter(AGE7I %in% 65:75) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100) 
          
          
          
          
          
      ## 3e.) 25-75 ---------------------------          
          
          # Provide assistance with ADLs (RR3HRS7I)
          ADL_25_75 <- caregivers %>%
            filter(AGE7I %in% 25:75) %>%
            mutate(hour_category = cut(RR3HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100)
          
          
          # Provide assistance with iADLS (RR5HRS7I)
          iADL_25_75 <- caregivers %>%
            filter(AGE7I %in% 25:75) %>%
            mutate(hour_category = cut(RR5HRS7I, 
                                       breaks = c(0, 1, 3, 5, 7, Inf), 
                                       labels = c("0-1", "2-3", "4-5", "6-7", "8+"))) %>%
            group_by(hour_category) %>%
            summarise(count = n()) %>%
            mutate(percentage = count / sum(count) * 100) 
   
          
        
          
          
         
        
          
          
          
          
          
          
          
          
    #### TABLE 4. Caregiver health (Private, Medicare, SSDI) ---------------------------
          
          # MM9A7I: Informant has medicare
          # MM9B7I: Informant has medicaid
          # MM9C7I: Informant has private insurance
          # MM9E7I: Informant has VA insurance
          # MM9D7I: Informant belongs to an HMO
          # MM9F7I: Informant has other insurance
            # MM9F7ISPEC: Informant has other insurance specified

          medicare <- caregivers %>%
            tab(MM9A7I, round = 3)
          
          medicaid <- caregivers %>%
            tab(MM9B7I, round = 3)
          
          private <- caregivers %>%
            tab(MM9C7I, round = 3)
          
          VA <- caregivers %>%
            tab(MM9E7I, round = 3)
    
          other <- caregivers %>%
            tab(MM9F7I, round = 3)
          
          HMO <- caregivers %>%
            tab(MM9D7I, round = 3)
          
          
          time <- caregivers %>%
            tab(RR3HRS7I)