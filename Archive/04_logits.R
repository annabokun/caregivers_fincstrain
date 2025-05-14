#=======================================================================================================#
# Anna Bokun: bokun001@umn.edu
# Postdoc, Population Research Center, University of Texas-Austin

# ICAA 2024
  # Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"

  # Anna Bokun, Jacqui Angel, Sunshine Rote, Phil Cantu, Flavia Cristina Drumond Andrade, Mariana Lopez-Ortega
    
  # A.) Generate DV and main variable of interest
  # B.) Cleaning
  # C.) Descriptives
  # D.) Estimate logit regression models

# Script created in RStudio ("Chocolate Cosmos")
# updated 9/25/2024
#=======================================================================================================#



#==============#
#=== SET-UP ===#
#==============#

  #### Load packages 
    library(tidyverse)
    library(vtable)
    library(tabulator)
    library(tinytable)
    library(gtsummary)
    library(stargazer)

    

  #### Turn off scientific notation
    options(scipen = 999) 
      
    
  #### Load data 
    # HEPESE, wave 7 (2010-2011): https://www.icpsr.umich.edu/web/NACDA/studies/36537  
    # Google Drive --> UT-Austin --> ICAA 2024 --> Caregivers - Housing Health Paradox --> Data --> Raw Data
    # wave7_all = care recipients + caregivers appended together by Person ID (done in the 02_mvmt_vars script) 
    
    wave7_all <- read_csv("Data/wave7_all.csv", col_types = cols(...1 = col_skip()))
    
    
    
    
    
#======================================================#
#=== GENERATE MAIN VARIABLES --------------------------- 
#======================================================#
    
    
  #### GENERATE ANALYTIC SAMPLE --------------------------- 
    
      ## Use RELSUBJ7I: Informant'ss relationship to respondent
      ## Source: https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/RELSUBJ7I?archive=nacda
    
        # RELSUBJ7I == 3	Son/daughter (including stepchildren)
    
        ## Restrict to adult child caregivers (ACC)
          ACC <- wave7_all %>%
            filter(RELSUBJ7I == 3)
      
          length(unique(ACC$Q_NO)) # n = 629
    
    
    
    
    
  #### DV #1.) Has the adult child caregiver (ACC) received financial help from the care recipient in the past year? --------------------------- 
     ## 1 = any help; 0 = none
           
    ## Based on SS3I7: Has informant received financial help from respondent in past year? (per informant)
    ## 1a.) Check SS3I7
          finc_help <- ACC %>%
            tab(SS3I7)
    
          table(wave7_all$SS3I7)
        
            # 1 = Not at all
            # 2 = Infrequent, occasionally
            # 3 = Regularly, he/she partially supports me
            # 4 = Regularly, he/she mostly supports me
            # 8 = Do know know 
            # 9 = Refused 
        
          
        
      ## 1b.) Generate DV1: CG received financial help (1 = any help; 0 = none)  
         ACC$cg_receive_help <- ifelse(
           ACC$SS3I7 %in% c(2, 3, 4) &
             ACC$RELSUBJ7I == 3,
              1, 0)
          
          
          
          
      ## 1c.) Check new variable: cg_receive_help
            table(ACC$cg_receive_help)  # 1 = 89; 0 = 540
                                        # 89/629 = 0.1414944
                                        # ~14% of ACCs receive help from the care recipient
          
    
          
          
          
          
  #### DV #2.) Financial Strain - index of various financial strain "TT" variables --------------------------- 
      ## 1 = reports difficulty on any item; 0 = none
            
      # TT57I_1: Financial responsibility for respondent makes it difficult to help my own kids (per informant)
      # TT57I_2: Financial responsibility for respondent makes it difficult for my own housing expenses (per informant)
      # TT57I_3: Financial responsibility for respondent makes it difficult for me to travel/vacation (per informant)
      # TT57I_4: Financial responsibility for respondent makes it difficult for me to do other things (per informant)
      # TT57SPECI: Financial responsibility for respondent makes it difficult for other specified reasons (per informant)
          
          
      ## 2a.) Check the structure of the "TT" variables
          # Note: TT57I_1-TT57I_4 all have the same categorical responses (below)
          TT57I_1 <- ACC %>%
            tab(TT57I_1) 
              # 1 = Helping out kids
              # 2 = Housing expenses
              # 3 = Travel / vacation
              # 4 = Other 
              # 8 = Do know know 
              # 9 = Refused 
          
          TT57SPECI <- ACC %>%
            tab(TT57SPECI)
              # FOOD
              # HER HOUSE
              # HOME REPAIRS
              # It does not cause any difficulties
              # PROTECTIVE SERVICES WON'T LET ME WORK FULL TIME
              # UNIFORMS FOR SCHOOL
              # groceries
              # no money left for personal expenses
          
          
          
      ## 2b.) Generate financial strain (1 = reports difficulty on any item; 0 = none)
          ACC$cg_finc_strain <- ifelse(
            rowSums(sapply(ACC[, c("TT57I_1", "TT57I_2", "TT57I_3", "TT57I_4")], function(x) x %in% 1:4)) > 0 |
              ACC$TT57SPECI %in% c("FOOD", "HER HOUSE", "HOME REPAIRS", "PROTECTIVE SERVICES WON'T LET ME WORK FULL TIME", "UNIFORMS FOR SCHOOL", "groceries", "no money left for personal expenses") & 
              ACC$RELSUBJ7I == 3,
            1,0)
          
         
          
      ## 2c.) Check new variable: cg_finc_strain
          table(ACC$cg_finc_strain) # 1 = 74; 0 = 555
                                    # 74/629 = 0.1176471
                                    # ~12% of ACCs experience financial strain
    
          
          
          
          
          
    
  #### 3.) A. Main variable of interest: ACCs co-reside with their aging parent ---------------------------
      
      ## Use HOUSEK_1: Child 1 lives in same house as care recipient
      # Source: https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0001/variables/HOUSEK_1?archive=nacda 
            # 1 = yes 
            # 2 = no 
          
          ACC$coreside <- ifelse(ACC$HOUSEK_1==1, 1, 0)
          
          table(ACC$coreside) # 1 = 185; 0 = 432
                              # 185/629 = 0.2941176
                              # ~ 30% of ACCs co-reside

          
      
      ## OR, use CLIVES7I_1
        # Residence of respondent's child #1 (per informant)
        # Source: https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/CLIVES7I_1?archive=nacda
          
        # LIVES7I_1 : CLIVES7I_16
          # 1 = With parent / relative *****SINCE IT LUMPS TOGETHER PARENTS + RELATIVES, BEST TO STICK WITH USING HOUSEK_1
          # 2 = Within 2 blocks
          # 3 = 3-8 blocks
          # 4 = 8 blocks to a mile
          # 5 = Other city
          # 6 = Other state
          # 7 = Mexico
          # 8 = Other  
          
      ACC$coreside2 <- ifelse(ACC$CLIVES7I_1==1, 1, 0)
      
      table(ACC$coreside2)  # 1 = 292; 0 = 331
                            # 292/629 = 0.4642289
                            # ~ 46% of ACCs co-reside
                            # We gain a lot more...
        
      

      
      
      
    #### NOTE: The old version is below (where I assigned a 1=coreside if ANY HOUSEK_1:HOUSEK_16==1)
      ## This is before Phil clarified that child1 refers to the ACC specifically
      
      ACC$coreside <- ifelse(
        apply(ACC[, paste0("HOUSEK_", 1:16)], 1, function(row) any(row == 1)) & 
          ACC$RELSUBJ7I == 3, 
        1, 0)
      
      
      ## 3a.) Check new variable: co-reside
        table(ACC$coreside) # 1 = 332; 0 = 296; NA = 450
                            # 332/629 = 0.5278219
                            # ~53% of ACCs co-reside with their aging parents
      

        
    #### Alternative version with CLIVES7I_
        
          #### Main variable of interest: adult children caregivers live with their aging parent
            ACC$coreside2 <- ifelse(
              apply(ACC[, paste0("CLIVES7I_", 1:16)], 1, function(row) any(row == 1)) & 
                ACC$RELSUBJ7I == 3, 
              1, 0)
          
          
          ## Check new variable: coreside2
            coreside2 <- ACC %>%
              tab(coreside2)  # 1 = 338; 0 = 296; NA = 444
                              # 338/634 = 0.533123
                                # ~53% of informants/adult children caregivers co-reside with their aging parents
                                # Very similar as original co-reside variable
            
          
        
            
            
           
            
            
  #### 3.) B. Main variable of interest: ACCs as HOH ---------------------------
            
        ## Use HHREL7I: Informant's relationship to head of household
        ## Source: https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/HHREL7I?archive=nacda
            
            # 1	Head of household (for B2 only)
            # 2	Spouse
            # 3	Son/daughter (including stepchildren)
            # 4	Son/daughter in law
            # etc 
            
          ACC$HoH_cg <- ifelse(ACC$HHREL7I == 1, 1, 0)

          ## 3a.) Check new variable: co-reside
            table(ACC$HoH_cg)       # 1 = 281; 0 = 347
                                    # 281/629 = 0.4467409
                                    # 45% of ACCs are the HOH



            
            
            
            
            
  #### 3.) C. Main variable of interest: ACCs NOT as HOH ---------------------------
            
          ## Use HHREL7I: Informant's relationship to head of household
          ## Source: https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/HHREL7I?archive=nacda
            
            # 1	Head of household (for B2 only)
            # 2	Spouse
            # 3	Son/daughter (including stepchildren)
            # 4	Son/daughter in law
            
          ACC$HoH_cg_no <- ifelse(ACC$HHREL7I != 1, 1, 0)
            
          ## 3a.) Check new variable: co-reside
            table(ACC$HoH_cg_no)    # 1 = 347; 0 = 281
                                    # 347/629 = 0.5516693
                                    # 55% of ACCs are NOT the HOH   
            
            
            
            
            
            
  #### 4.) C. Main variable of interest: Aging parent as HOH ---------------------------
            
          ## Use HHREL7: Relationship to head of household
          ## Source: https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0001/variables/HHREL7?archive=nacdaa
            
            #  1	Respondent is head
            #  2	Spouse
            #  3	Son/daughter

          ACC$HoH_parent <- ifelse(ACC$HHREL7 == 1, 1, 0)
            
          ## 3a.) Check new variable: co-reside
            table(ACC$HoH_parent)   # 1 = 407; 0 = 218
                                    # 407/629 = 0.6470588
                                    # 65% of parents are the HoH
            
             
            
            
            
            
            
#===========================================#
#=== CLEANING ---------------------------
#===========================================#        

    
    
  #### 1.) Generate age categories ---------------------------
            
    summary(ACC$AGE7I) # 26:75
    hist(ACC$AGE7I)
            
    ACC$age_cat2 <- dplyr::case_when(
      ACC$AGE7I %in% 18:44 ~ "Early Adulthood (18-44)",
      ACC$AGE7I %in% 45:54 ~ "Middle Age (45-54)",
      ACC$AGE7I %in% 55:64 ~ "Late Middle Age (55-64)",
      ACC$AGE7I %in% 65:75 ~ "Early Retirement (65-75)")
    
    table(ACC$age_cat2)
      # Early Adulthood (18-44): 66
      # Middle Age (45-54): 217
      # Late Middle Age (55-64): 276
      # Early Retirement (65-75): 69
    
          ## Age categories too inbalanced? 

    
    
    
    
    
  #### 2.) Generate ACC marital status dummy ---------------------------
      
  ## Use case_when, since ifelse will capture IDKs & NAs into "not married" instead of NA
    ACC$marital_cg <- dplyr::case_when(
      ACC$CMARSTAT7I_1 == 1 ~ "1",
      ACC$CMARSTAT7I_1 == 2 ~ "0", 
      ACC$CMARSTAT7I_1 == 3 ~ "0",
      ACC$CMARSTAT7I_1 == 4 ~ "0",
      ACC$CMARSTAT7I_1 == 5 ~ "0",
      ACC$CMARSTAT7I_1 == 9 ~ "Do not know/refused") 

      table(ACC$marital_cg) # 1 = 326; 0 = 299; NA = 1
      
      
      ## Make into a dummy 
        ACC$marital_cg <- ifelse(ACC$marital_cg == 1, 1, 0)
        table(ACC$marital_cg) # 1 = 326; 0 = 300
                



      
              
  #### 3.) Collapse medicaid status (MM9B7I) to dummy ---------------------------
    table(ACC$MM9B7I) 
        # 1 = yes; 2 = no Medicaid 
    
    ACC$medicaid_dum <- dplyr::case_when(
      ACC$MM9B7I == 1 ~ "1",
      ACC$MM9B7I == 2 ~ "0", 
      ACC$MM9B7I == 8 | ACC$MM9B7I == 9 ~ "Do not know/refused")

    table(ACC$medicaid_dum) # 1 = 70; 0 = 542; Do not know/refused = 6
    
      ## Make into a dummy 
        ACC$medicaid_dum <- ifelse(ACC$MM9B7I == 1, 1, 0)
        table(ACC$medicaid_dum) # 1 = 70 ; 0 = 548
        
        
        
        
        
  #### 4.) Education dummy ---------------------------
    ACC$educ_cg <- ifelse(ACC$CEDUC7I_1 > 5, 1, 0)
        
    table(ACC$educ_cg) # 1 = 569; 0 = 50
    
    

    
    
    
    
    
  #### 5.) Dichotomize annual HH income ---------------------------
      ## See original HH income 
        table(ACC$LL3A7I)
        
      ## Dichotomize (1 = < $15k) 
        ACC$inc_dich <- dplyr::case_when(     # 1 = < $15k; 0 = >$15k annual HH income
          ACC$LL3A7I == 1 ~ "1", # $0-$4,999
          ACC$LL3A7I == 2 ~ "1", # $5,000-$9,999
          ACC$LL3A7I == 3 ~ "1", # $10,000-$14,999
          ACC$LL3A7I == 4 ~ "0", # $15,000-$19,999
          ACC$LL3A7I == 5 ~ "0", # $20,000-$29,999
          ACC$LL3A7I == 6 ~ "0", # $30,000-$39,999
          ACC$LL3A7I == 7 ~ "0", # $40,000-$49,999
          ACC$LL3A7I == 8 ~ "0", # $50,000 & Over
          ACC$LL3A7I == 98 | ACC$LL3A7I == 99 ~ "Do not know/refused")
   
      ## See new variable: inc_dich 
        table(ACC$inc_dich) # 1 = 174; 0 = 392; Do not know/refused = 63
                            # 174/629 = 0.2766296
                            # 28% of ACCs have < $15k annual HH income

        
        
        
  #### 6.) Recode CG INC ---------------------------
        
        ## Cross-tab PLASTAY7 x LL3A7I 
        
        # 1	= $0-$4,999
        # 2	= $5,000-$9,999
        # 3	= $10,000-$14,999
        # 4	= $15,000-$19,999
        # 5	= $20,000-$29,999
        # 6	= $30,000-$39,999
        # 7	= $40,000-$49,999
        # 8	= $50,000 & Over
        # 98 = Don't Know
        # 99 = Refused
        
        
        ACC$LL3A7I_recode <- dplyr::case_when(
          ACC$LL3A7I == 1 ~ "$0-$4,999", 
          ACC$LL3A7I == 2 ~ "$5,000-$9,999", 
          ACC$LL3A7I == 3 ~ "$10,000-$14,999", 
          ACC$LL3A7I == 4 ~ "$15,000-$19,999", 
          ACC$LL3A7I == 5 ~ "$20,000-$29,999", 
          ACC$LL3A7I == 6 ~ "$30,000-$39,999",
          ACC$LL3A7I == 7 ~ "$40,000-$49,999", 
          ACC$LL3A7I == 8 ~ "$50,000+",
          ACC$LL3A7I == 98 ~ "Don't know",
          ACC$LL3A7I == 99 ~ "Refused")
        
        
        
        ## Check new variable: LL3A7I_recode
        LL3A7I_recode <- ACC %>%
          tab(LL3A7I_recode)
        
        
        
      ## Tab: PLASTAY7 x LL3A7I_recode
        ## PLASTAY7: Person moved in (with respondent) because they needed a place to stay (per respondent)
        ## LL3A7I: Informant’s yearly household income
        
        livexinc <- ACC %>%
          tab(PLASTAY7, LL3A7I_recode) 
        
        tograph <- livexinc %>%
          filter(PLASTAY7==2)
        
        
        
      ## Generate bar chart: caregiver's income x caregiver moved in with parent bc they needed a place to stay
        ggplot(tograph, 
               aes(x = LL3A7I_recode, 
                   y = N)) +
          geom_bar(stat = "identity", position = "dodge") +
          geom_text(aes(label = N), vjust = -0.5) +
          ggtitle("Cross tab: Caregiver income x caregiver moved in with parent (needed a place to stay)") +
          labs(subtitle = "Source: HEPESE, Wave 7 (2010-2011)") + 
          xlab("Caregiver Income") +
          ylab("Unique Caregivers") +
          theme_minimal() +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + #y-axis as integers (no decimals)
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                plot.title = element_text(size = 18, face = "bold"),
                plot.subtitle = element_text(size = 14, color = "gray30"),
                legend.title = element_text(face = "bold"))        
    
    
    
        
        
#### Save cleaned df ---------------------------
write.csv(ACC, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/ACC.csv")
        
    
            
        
        
    
        
#===========================================#
#=== DESCRIPTIVES ---------------------------
#===========================================#

  #### Table 1 ---------------------------
  
  ## Summary sample statistics
  ## NOTE: From Phil (9/23/2024):
        # "If the informant was a child, than the first child in the roster was the informant, so any information about child 1 was about the caregiver.  
        # So when limiting the sample to only child caregivers you have information about their marital status. 
        # We do not have marital status information on other types of caregivers."
        
        
      # Caregiver's demographics: 
        # Age: agecat2
        # Gender: SEX7I
        # Marital status: CMARSTAT7I_1
        # Medicaid status: medicaid_dum
        # HH income: inc_dich
        # Education: educ_cg
            
            
            
    ## Sample sizes ---------------------------
        
      # ACCs -- Overall 
        overall <- ACC %>%
          filter(RELSUBJ7I == 3) # n = 629
          
      # ACCs -- Received financial help from parents
        got_help <- ACC %>%
          filter(RELSUBJ7I == 3 & cg_receive_help==1) # n = 89
        
      # ACCs -- Experienced financial strain
        finc_strain <- ACC %>%
          filter(RELSUBJ7I==3 & cg_finc_strain==1) # n = 74
        
      # ACCs -- Co-reside with aging parent
        coresides <- ACC %>%
          filter(RELSUBJ7I==3 & coreside==1) # n = 185
        
      # ACCs -- Co-reside with aging parent + financial strain
        coresides_finc <- ACC %>%
          filter(RELSUBJ7I==3 & coreside==1 & cg_finc_strain==1) # n = 29
        
      # ACCs -- who are HOH
        HoH <- ACC %>%
          filter(RELSUBJ7I==3 & HHREL7I == 1) # n = 281
        
      # ACCs -- who are NON-HOH
        no_HoH <- ACC %>%
          filter(RELSUBJ7I==3 & HHREL7I != 1) # n = 347
        
      # Parents are the HIH
        HOH_parent <- ACC %>%
          filter(HoH_parent==1) # n = 407
          

        
            
        
            
  #### A. DESCRIPTIVES: OVERALL SAMPLE ---------------------------
     
        ## Tabulate descriptives
          descriptives <- overall %>%
          # Calculate mean for AGE7I
            summarise(AGE7I_mean = mean(AGE7I, na.rm = TRUE),
          # Calculate percentage of females
                      SEX7I_female_pct = mean(SEX7I == 2, na.rm = TRUE) * 100) %>%
          # Add percentages for MM9B7I where 1 indicates having Medicaid
            bind_cols(overall %>%
                        summarise(MM9B7I_pct = mean(MM9B7I == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for CMARSTAT7I_1 where 1 indicates being married
            bind_cols(overall %>%
                      summarise(CMARSTAT7I_1_pct = mean(CMARSTAT7I_1 == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for education where > 5 years (low educ < 6; high educ > 6)
            bind_cols(overall %>%
                      summarise(CEDUC7I_1_pct = mean(CEDUC7I_1 > 5, na.rm = TRUE) * 100))
        
        # Add inc_dich as a categorical variable
          inc_dich_table <- overall %>%
            count(inc_dich) %>%
            mutate(percentage = n / sum(n) * 100)
          
        # Display the descriptive table
          desc <- print(descriptives)
          print(inc_dich_table)
     
        # Convert descriptives to the long format
        descriptives_long <- desc %>%
          pivot_longer(cols = everything(),
                       names_to = "Variable",
                       values_to = "Value")
        
        # Convert LL3A7I_recode_table to the long format
          inc_dich_table_long <- inc_dich_table %>%
              pivot_longer(cols = -inc_dich,
                           names_to = "Statistic",
                           values_to = "Value")
        
        # Combine the two long format data frames into a list
          output_list_long <- list(
            "Descriptives_Long" = descriptives_long,
            "inc_dich_table_long" = inc_dich_table_long)
          
          print(output_list_long)
     
        # What % of educ is missing?
          percent_missing_CEDUC7I_1 <- mean(is.na(overall$CEDUC7I_1)) * 100
          percent_missing_CEDUC7I_1
            
          
          
          
          
          
          
          
  #### B. DESCRIPTIVES: CAREGIVERS WHO RECEIVED FINANCIAL HELP FROM PARENTS ---------------------------
          
          ## Tabulate descriptives
          descriptives <- got_help %>%
            # Calculate mean for AGE7I
            summarise(AGE7I_mean = mean(AGE7I, na.rm = TRUE),
                      # Calculate percentage of females
                      SEX7I_female_pct = mean(SEX7I == 2, na.rm = TRUE) * 100) %>%
            # Add percentages for MM9B7I where 1 indicates having Medicaid
            bind_cols(got_help %>%
                        summarise(MM9B7I_pct = mean(MM9B7I == 1, na.rm = TRUE) * 100)) %>%
            # Add percentages for CMARSTAT7I_1 where 1 indicates being married
            bind_cols(got_help %>%
                        summarise(CMARSTAT7I_1_pct = mean(CMARSTAT7I_1 == 1, na.rm = TRUE) * 100)) %>%
            # Add percentages for education where > 5 years (low educ < 6; high educ > 6)
            bind_cols(got_help %>%
                        summarise(CEDUC7I_1_pct = mean(CEDUC7I_1 > 5, na.rm = TRUE) * 100))
          
          # Add inc_dich as a categorical variable
          inc_dich_table <- got_help %>%
            count(inc_dich) %>%
            mutate(percentage = n / sum(n) * 100)
          
          # Display the descriptive table
          desc <- print(descriptives)
          print(inc_dich_table)
          
          # Convert descriptives to the long format
          descriptives_long <- desc %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable",
                         values_to = "Value")
          
          # Convert LL3A7I_recode_table to the long format
          inc_dich_table_long <- inc_dich_table %>%
            pivot_longer(cols = -inc_dich,
                         names_to = "Statistic",
                         values_to = "Value")
          
          # Combine the two long format data frames into a list
          output_list_long <- list(
            "Descriptives_Long" = descriptives_long,
            "inc_dich_table_long" = inc_dich_table_long)
          
          print(output_list_long)
    
    
    
    
         
          
          
          
  #### C. DESCRIPTIVES: CAREGIVERS WHO EXPERIENCED FINANCIAL STRAIN  ---------------------------

        ## Tabulate descriptives
          descriptives <- finc_strain %>%
          # Calculate mean for AGE7I
            summarise(AGE7I_mean = mean(AGE7I, na.rm = TRUE),
                      # Calculate percentage of females
                      SEX7I_female_pct = mean(SEX7I == 2, na.rm = TRUE) * 100) %>%
          # Add percentages for MM9B7I where 1 indicates having Medicaid
            bind_cols(finc_strain %>%
                        summarise(MM9B7I_pct = mean(MM9B7I == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for CMARSTAT7I_1 where 1 indicates being married
            bind_cols(finc_strain %>%
                        summarise(CMARSTAT7I_1_pct = mean(CMARSTAT7I_1 == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for education where > 5 years (low educ < 6; high educ > 6)
            bind_cols(finc_strain %>%
                        summarise(CEDUC7I_1_pct = mean(CEDUC7I_1 > 5, na.rm = TRUE) * 100))
          
        # Add inc_dich as a categorical variable
          inc_dich_table <- finc_strain %>%
            count(inc_dich) %>%
            mutate(percentage = n / sum(n) * 100)
          
        # Display the descriptive table
          desc <- print(descriptives)
          print(inc_dich_table)
          
        # Convert descriptives to the long format
          descriptives_long <- desc %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable",
                         values_to = "Value")
          
        # Convert LL3A7I_recode_table to the long format
          inc_dich_table_long <- inc_dich_table %>%
            pivot_longer(cols = -inc_dich,
                         names_to = "Statistic",
                         values_to = "Value")
          
        # Combine the two long format data frames into a list
          output_list_long <- list(
            "Descriptives_Long" = descriptives_long,
            "inc_dich_table_long" = inc_dich_table_long)
          
          print(output_list_long)
          
          
        # What % of educ is missing?
          percent_missing_CEDUC7I_1 <- mean(is.na(finc_strain$CEDUC7I_1)) * 100
          percent_missing_CEDUC7I_1
          
        
          
          
        
        
        
  #### D. DESCRIPTIVES: CAREGIVERS WHO CO-RESIDE WITH AGING PARENTS  ---------------------------
          
        ## Tabulate descriptives
          descriptives <- coresides %>%
          # Calculate mean for AGE7I
            summarise(AGE7I_mean = mean(AGE7I, na.rm = TRUE),
                      # Calculate percentage of females
                      SEX7I_female_pct = mean(SEX7I == 2, na.rm = TRUE) * 100) %>%
          # Add percentages for MM9B7I where 1 indicates having Medicaid
            bind_cols(coresides %>%
                        summarise(MM9B7I_pct = mean(MM9B7I == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for CMARSTAT7I_1 where 1 indicates being married
            bind_cols(coresides %>%
                        summarise(CMARSTAT7I_1_pct = mean(CMARSTAT7I_1 == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for education where > 5 years (low educ < 6; high educ > 6)
            bind_cols(coresides %>%
                        summarise(CEDUC7I_1_pct = mean(CEDUC7I_1 > 5, na.rm = TRUE) * 100))
          
        # Add inc_dich as a categorical variable
          inc_dich_table <- coresides %>%
            count(inc_dich) %>%
            mutate(percentage = n / sum(n) * 100)
          
        # Display the descriptive table
          desc <- print(descriptives)
          print(inc_dich_table)
          
        # Convert descriptives to the long format
          descriptives_long <- desc %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable",
                         values_to = "Value")
          
        # Convert LL3A7I_recode_table to the long format
          inc_dich_table_long <- inc_dich_table %>%
            pivot_longer(cols = -inc_dich,
                         names_to = "Statistic",
                         values_to = "Value")
          
        # Combine the two long format data frames into a list
          output_list_long <- list(
            "Descriptives_Long" = descriptives_long,
            "inc_dich_table_long" = inc_dich_table_long)
          
          print(output_list_long)
          
          
        # What % of educ is missing?
          percent_missing_CEDUC7I_1 <- mean(is.na(coresides$CEDUC7I_1)) * 100
          percent_missing_CEDUC7I_1
          
          
          
          
          
          
   
          
          
  #### E. DESCRIPTIVES: CAREGIVERS AS HOH ---------------------------
          
        ## Tabulate descriptives
          descriptives <- HoH %>%
          # Calculate mean for AGE7I
            summarise(AGE7I_mean = mean(AGE7I, na.rm = TRUE),
                      # Calculate percentage of females
                      SEX7I_female_pct = mean(SEX7I == 2, na.rm = TRUE) * 100) %>%
          # Add percentages for MM9B7I where 1 indicates having Medicaid
            bind_cols(HoH %>%
                        summarise(MM9B7I_pct = mean(MM9B7I == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for CMARSTAT7I_1 where 1 indicates being married
            bind_cols(HoH %>%
                        summarise(CMARSTAT7I_1_pct = mean(CMARSTAT7I_1 == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for education where > 5 years (low educ < 6; high educ > 6)
            bind_cols(HoH %>%
                        summarise(CEDUC7I_1_pct = mean(CEDUC7I_1 > 5, na.rm = TRUE) * 100))
          
        # Add inc_dich as a categorical variable
          inc_dich_table <- HoH %>%
            count(inc_dich) %>%
            mutate(percentage = n / sum(n) * 100)
          
        # Display the descriptive table
          desc <- print(descriptives)
          print(inc_dich_table)
          
        # Convert descriptives to the long format
          descriptives_long <- desc %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable",
                         values_to = "Value")
          
        # Convert LL3A7I_recode_table to the long format
          inc_dich_table_long <- inc_dich_table %>%
            pivot_longer(cols = -inc_dich,
                         names_to = "Statistic",
                         values_to = "Value")
          
        # Combine the two long format data frames into a list
          output_list_long <- list(
            "Descriptives_Long" = descriptives_long,
            "inc_dich_table_long" = inc_dich_table_long)
          
          print(output_list_long)
          
        # What % of educ is missing?
          percent_missing_CEDUC7I_1 <- mean(is.na(HoH$CEDUC7I_1)) * 100
          percent_missing_CEDUC7I_1
        
          
          
          
          
          
  #### F. DESCRIPTIVES: CAREGIVERS AS NON-HOH ---------------------------
          
        ## Tabulate descriptives
          descriptives <- no_HoH %>%
          # Calculate mean for AGE7I
            summarise(AGE7I_mean = mean(AGE7I, na.rm = TRUE),
                      # Calculate percentage of females
                      SEX7I_female_pct = mean(SEX7I == 2, na.rm = TRUE) * 100) %>%
          # Add percentages for MM9B7I where 1 indicates having Medicaid
            bind_cols(no_HoH %>%
                        summarise(MM9B7I_pct = mean(MM9B7I == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for CMARSTAT7I_1 where 1 indicates being married
            bind_cols(no_HoH %>%
                        summarise(CMARSTAT7I_1_pct = mean(CMARSTAT7I_1 == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for education where > 5 years (low educ < 6; high educ > 6)
            bind_cols(no_HoH %>%
                        summarise(CEDUC7I_1_pct = mean(CEDUC7I_1 > 5, na.rm = TRUE) * 100))
          
        # Add inc_dich as a categorical variable
          inc_dich_table <- no_HoH %>%
            count(inc_dich) %>%
            mutate(percentage = n / sum(n) * 100)
          
        # Display the descriptive table
          desc <- print(descriptives)
          print(inc_dich_table)
          
        # Convert descriptives to the long format
          descriptives_long <- desc %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable",
                         values_to = "Value")
          
        # Convert LL3A7I_recode_table to the long format
          inc_dich_table_long <- inc_dich_table %>%
            pivot_longer(cols = -inc_dich,
                         names_to = "Statistic",
                         values_to = "Value")
          
        # Combine the two long format data frames into a list
          output_list_long <- list(
            "Descriptives_Long" = descriptives_long,
            "inc_dich_table_long" = inc_dich_table_long)
          
        print(output_list_long)
          
        
      # What % of educ is missing?
        percent_missing_CEDUC7I_1 <- mean(is.na(no_HoH$CEDUC7I_1)) * 100
        percent_missing_CEDUC7I_1
        

        
        
          
  #### G. DESCRIPTIVES: PARENTS AS THE HOH ---------------------------
        
        ## Tabulate descriptives
        descriptives <- HOH_parent %>%
          # Calculate mean for AGE7I
          summarise(AGE7I_mean = mean(AGE7I, na.rm = TRUE),
                    # Calculate percentage of females
                    SEX7I_female_pct = mean(SEX7I == 2, na.rm = TRUE) * 100) %>%
          # Add percentages for MM9B7I where 1 indicates having Medicaid
          bind_cols(HOH_parent %>%
                      summarise(MM9B7I_pct = mean(MM9B7I == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for CMARSTAT7I_1 where 1 indicates being married
          bind_cols(HOH_parent %>%
                      summarise(CMARSTAT7I_1_pct = mean(CMARSTAT7I_1 == 1, na.rm = TRUE) * 100)) %>%
          # Add percentages for education where > 5 years (low educ < 6; high educ > 6)
          bind_cols(HOH_parent %>%
                      summarise(CEDUC7I_1_pct = mean(CEDUC7I_1 > 5, na.rm = TRUE) * 100))
        
        # Add inc_dich as a categorical variable
        inc_dich_table <- HOH_parent %>%
          count(inc_dich) %>%
          mutate(percentage = n / sum(n) * 100)
        
        # Display the descriptive table
        desc <- print(descriptives)
        print(inc_dich_table)
        
        # Convert descriptives to the long format
        descriptives_long <- desc %>%
          pivot_longer(cols = everything(),
                       names_to = "Variable",
                       values_to = "Value")
        
        # Convert LL3A7I_recode_table to the long format
        inc_dich_table_long <- inc_dich_table %>%
          pivot_longer(cols = -inc_dich,
                       names_to = "Statistic",
                       values_to = "Value")
        
        # Combine the two long format data frames into a list
        output_list_long <- list(
          "Descriptives_Long" = descriptives_long,
          "inc_dich_table_long" = inc_dich_table_long)
        
        print(output_list_long)
        
        
        # What % of educ is missing?
        percent_missing_CEDUC7I_1 <- mean(is.na(HOH_parent$CEDUC7I_1)) * 100
        percent_missing_CEDUC7I_1    
        
        
        
        
        
        
        
        
        
          
          
  #### Table 2 ---------------------------
    # Cross tab of CG/informant as head and nonhead of household (two columns) 

          # Use HHREL7I: Informant's relationship to head of household
          # Source: https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0002/variables/HHREL7I?archive=nacda
          
              # 1	Head of household (for B2 only)
              # 2	Spouse
              # 3	Son/daughter (including stepchildren)
              # 4	Son/daughter in law
    
          
    ## Restrict to ACCs
      ACC <- wave7_all %>%
        filter(RELSUBJ7I == 3) # n = 629
          
          
    ## Summarize the data
        table_summary <- ACC %>%
        # Group by HHREL7I (head vs. non-head of household)
          group_by(HHREL7I == 1) %>%
        # Summarize counts for cg_receive_help and cg_finc_strain
          summarise(
            cg_receive_help_1 = sum(cg_receive_help == 1, na.rm = TRUE),
            cg_finc_strain_1 = sum(cg_finc_strain == 1, na.rm = TRUE)) %>%
        
      ## Rename the columns for clarity
          rename(
            Caregiver_as_Head_of_HH = `HHREL7I == 1`,
            Caregiver_Receive_Help = cg_receive_help_1,
            Caregiver_Financial_Strain = cg_finc_strain_1) %>%
          
          # Pivot the table to have "Receive_Help" and "Financial_Strain" as rows
          pivot_longer(cols = c(Caregiver_Receive_Help, Caregiver_Financial_Strain),
                       names_to = "Condition",
                       values_to = "Count")
      
      ## Spread the table to create separate columns for Head and Non-Head of HH
        final_table <- table_summary %>%
          pivot_wider(names_from = Caregiver_as_Head_of_HH,
                      values_from = Count,
                      names_prefix = "Caregiver_as_Head_",
                      values_fill = list(Count = 0)) %>%
          rename("Caregiver as Head of HH" = Caregiver_as_Head_TRUE,
                 "Caregiver as Non-Head of HH" = Caregiver_as_Head_FALSE)
      
      ## Display the final table
        print(final_table)   
          
        check <- wave7_all %>%
          tab(wave7_all$cg_receive_help, wave7_all$HHREL7I)
          
          
         
          
          
          
 
#### Save df      
write.csv(wave7_all, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/wave7_all.csv")
        
          
          
          
          

          
#================================================#
#=== LOGIT REGRESSIONS ---------------------------
#================================================#
          
          
  #### MODEL SET_UP ---------------------------      
    
    # DV 1: cg_finc_strain 
      # Index of 5 various financial strain "TT" variables (1 = at least one of these items; 0 = none)
    
      # A.) Main variable of interest: co-reside
        # ACC living together with aging parent/care recipient
  
      # B.) Main variable of interest: HoH_cg
        # ACC is the HOH they share with the aging parent

      # C.) Main variable of interest: HoH_cg_no
        # ACC is the NOT the HOH they share with the aging parent 
                    
    # Controls (caregiver's)
      # Age: age_cat2     
      # Gender: SEX7I (1 = female; 2 = male)
      # Marital status: marital_cg (1 = married)
      # Medicaid status: medicaid_dum (1 = medicaid)
      # HH income: inc_dich
      # Education: educ_cg (1 = more than 5 yrs of education)

          
          
          
  #### 1.) Check classes of variables used in regressions 
    class(ACC$cg_finc_strain)
    class(ACC$coreside)
    class(ACC$HoH_cg)
    class(ACC$HoH_cg_no)
    
    class(ACC$age_cat2) # character
    class(ACC$SEX7I)
    class(ACC$marital_cg)
    class(ACC$medicaid_dum)
    class(ACC$inc_dich) # character
    class(ACC$educ_cg)
    
    
    
    
  #### 2.) Set as factors --------------------------- 
    ACC$cg_finc_strain <- as.factor(ACC$cg_finc_strain)
    ACC$coreside <- as.factor(ACC$coreside)
    ACC$HoH_cg <- as.factor(ACC$HoH_cg)
    ACC$HoH_cg_no <- as.factor(ACC$HoH_cg_no)
    
    ACC$age_cat2 <- as.factor(ACC$age_cat2)
    ACC$SEX7I <- as.factor(ACC$SEX7I)
    ACC$marital_cg <- as.factor(ACC$marital_cg)
    ACC$medicaid_dum <- as.factor(ACC$medicaid_dum)
    ACC$inc_dich <- as.factor(ACC$inc_dich)
    ACC$educ_cg <- as.factor(ACC$educ_cg)

    
    
    
    
  #### 3.) Set reference categories ---------------------------
    #ACC$cg_finc_strain <- factor(ACC$cg_finc_strain, levels = c(0, 1)) # ref = no financial strain (0)
    
    #ACC$coreside <- factor(ACC$coreside, levels = c(0, 1)) # ref = not co-residing (0)
    
    ACC$age_cat2 <- factor(ACC$age_cat2, levels = c("Early Retirement (65-75)", # ref == Early Retirement (65-74)
                                                                "Early Adulthood (18-44)", 
                                                                "Middle Age (45-54)",
                                                                "Late Middle Age (55-64)"))

    
    ACC$SEX7I <- factor(ACC$SEX7I, levels = c(1, 2)) # ref == men (1) 
    
    ACC$marital_cg <- factor(ACC$marital_cg, levels = c(0, 1)) # ref == not married (0)
    
    ACC$inc_dich <- factor(ACC$inc_dich, levels = c(0, 1)) # ref == earning > $15k (0)
    
    ACC$medicaid_dum <- factor(ACC$medicaid_dum, levels = c(0, 1)) # ref == not on medicaid coverage (0)
    
    ACC$educ_cg <- factor(ACC$educ_cg, levels = c(0, 1)) # ref == less than 6yrs of educ (0) 

    
    
    
  ### 4.) Check reference levels ---------------------------
    levels(ACC$cg_finc_strain) # "0" "1"
    levels(ACC$coreside) # "0" "1"
    levels(ACC$HoH_cg) # "0" "1"
    levels(ACC$HoH_cg_no) # "0" "1"
    
    levels(ACC$age_cat2) # "Early Retirement (65-74)" 
    levels(ACC$SEX7I) # "1" "2"
    levels(ACC$marital_cg) # "0" "1"
    levels(ACC$medicaid_dum) # "0" "1"
    levels(ACC$inc_dich) # "0" "1"
    levels(ACC$educ_cg) # "0" "1"

    
    
    
    
  ### 5.) Double-check key vars ---------------------------
    table(ACC$cg_finc_strain) # 1 = 74; 0 = 555
    table(ACC$coreside) # 1 = 185; 0 = 432
    table(ACC$HoH_cg) # 1 = 281; 0 = 347
    table(ACC$HoH_cg_no) # 1 = 347; 0 - 281
    
    table(ACC$age_cat2) # "Early Retirement (65-74)" # 68
    table(ACC$SEX7I) # (1) male = 192; (2) female = 437
    table(ACC$marital_cg) # 1 = 326; 0 = 300
    table(ACC$medicaid_dum) # 1 = 70; 0 = 548
    table(ACC$inc_dich) # 1 = 174; 0 = 392
    table(ACC$educ_cg) # 1 = 569; 0 = 50

 


    
    
    
#### Save cleaned df ---------------------------
write.csv(ACC, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/ACC.csv")
    
    
    
    
    
    
    

  #### MODEL A (ACC co-residence on financial strain) ---------------------------
          
    logitA <- glm(cg_finc_strain ~ coreside + 
                    age_cat2 + 
                    SEX7I + 
                    marital_cg + 
                    medicaid_dum +
                    inc_dich + 
                    educ_cg,  
                  data = ACC,
                  family = binomial(link = "logit"))
    
          
    ## Model summary (log odds)
      summary(logitA) 

      
    #### Logit resources for interpreting:
      ## StackOverflow: Odds ratios instead of logits in stargazer() LaTeX output
        # https://stackoverflow.com/questions/16236560/odds-ratios-instead-of-logits-in-stargazer-latex-output
      ## Producing stargazer tables with odds ratios and standard errors in R
        # https://cimentadaj.github.io/blog/2016-08-22-producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/
      
    
    ## Get the odds ratio
      exp(coefficients(logitA))
      OR <- exp(coef(logitA)) 
    
      
      
    ## Get the SEs
      # Define a helper function to extract SE from glm output
      se.coef <- function(logitA){sqrt(diag(vcov(logitA)))}
      
      # Or, you can use the arm package
      se.coef <- arm::se.coef
      
      # Then, we can get the `StdErr.OR` by multiplying the two:
      Std.Error.OR <- OR * se.coef(logitA)
      
      # using Std Errors
      stargazer(logitA, type="text", coef=list(OR), se = list(Std.Error.OR), t.auto=F, p.auto=F)
      
      
    
    ## Calculate CIs
      # Have to compute from the original log odds exp(coef ± 1.96*SE)
      CI.OR <- as.matrix(exp(confint.default(logitA)))
      stargazer(logitA, type="text", coef=list(OR), ci.custom = list(CI.OR), t.auto=F, p.auto=F, ci = T)
      
      exp(confint(logitA))
      exp(confint.default(logitA)) #this one matches the above stargazer CI output
      
    
    ## Calculate P-values
      library(arm)
      z <- coef(logitA)/se.coef(logitA)
      pvalue <- 2*pnorm(abs(coef(logitA)/se.coef(logitA)), lower.tail = F)
      print(pvalue, digits = 3)
    
    
    
    ## Function for extracting the statistics separately and applying the appropriate conversions
      stargazer2 <- function(logitA, odd.ratio = F, ...) {
        if(!("list" %in% class(logitA))) logitA <- list(logitA)
        
        if (odd.ratio) {
          coefOR2 <- lapply(logitA, function(x) exp(coef(x)))
          seOR2 <- lapply(logitA, function(x) exp(coef(x)) * summary(x)$coef[, 2])
          p2 <- lapply(logitA, function(x) summary(x)$coefficients[, 4])
          stargazer(logitA, coef = coefOR2, se = seOR2, p = p2, ...)
          
        } else {
          stargazer(logitA, ...)
        }
      }
    
    
    ## Display 
      stargazer(logitA, type = "text") # Our standard log odds
      stargazer2(logitA, odd.ratio = T, type = "text") # Now the coefficients and significance is correct!

          
          
          
          
    
    
    
    
    
    
    
    
    
  #### MODEL B (ACC HOH status on financial strain) ---------------------------

    # Restrict sample to co-residing ACCs 
      ACC2 <- ACC %>%
        filter(coreside==1) # n = 185
    
    
    logitB <- glm(cg_finc_strain ~ HoH_cg + 
                    age_cat2 + 
                    SEX7I + 
                    marital_cg + 
                    medicaid_dum +
                    inc_dich + 
                    educ_cg,  
                  data = ACC2,
                  family = binomial(link = "logit"))
    
    
    ## Model summary (log odds)
      summary(logitB) 
          

    
    #### Logit resources for interpreting:
      ## StackOverflow: Odds ratios instead of logits in stargazer() LaTeX output
        # https://stackoverflow.com/questions/16236560/odds-ratios-instead-of-logits-in-stargazer-latex-output
      ## Producing stargazer tables with odds ratios and standard errors in R
        # https://cimentadaj.github.io/blog/2016-08-22-producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/
      
    
    ## Get the odds ratio
      exp(coefficients(logitB))
      OR <- exp(coef(logitB)) 
    
      
      
    ## Get the SEs
      # Define a helper function to extract SE from glm output
      se.coef <- function(logitB){sqrt(diag(vcov(logitB)))}
      
      # Or, you can use the arm package
      se.coef <- arm::se.coef
      
      # Then, we can get the `StdErr.OR` by multiplying the two:
      Std.Error.OR <- OR * se.coef(logitB)
      
      # using Std Errors
      stargazer(logitB, type="text", coef=list(OR), se = list(Std.Error.OR), t.auto=F, p.auto=F)
      
      
    
    ## Calculate CIs
      # Have to compute from the original log odds exp(coef ± 1.96*SE)
      CI.OR <- as.matrix(exp(confint.default(logitB)))
      stargazer(logitB, type="text", coef=list(OR), ci.custom = list(CI.OR), t.auto=F, p.auto=F, ci = T)
      
      exp(confint(logitB))
      exp(confint.default(logitB)) #this one matches the above stargazer CI output
      
    
    ## Calculate P-values
      library(arm)
      z <- coef(logitB)/se.coef(logitB)
      pvalue <- 2*pnorm(abs(coef(logitB)/se.coef(logitB)), lower.tail = F)
      print(pvalue, digits = 3)
    
    
    
    ## Function for extracting the statistics separately and applying the appropriate conversions
      stargazer2 <- function(logitB, odd.ratio = F, ...) {
        if(!("list" %in% class(logitB))) logitB <- list(logitB)
        
        if (odd.ratio) {
          coefOR2 <- lapply(logitB, function(x) exp(coef(x)))
          seOR2 <- lapply(logitB, function(x) exp(coef(x)) * summary(x)$coef[, 2])
          p2 <- lapply(logitB, function(x) summary(x)$coefficients[, 4])
          stargazer(logitB, coef = coefOR2, se = seOR2, p = p2, ...)
          
        } else {
          stargazer(logitB, ...)
        }
      }
    
    
    ## Display 
      stargazer(logitB, type = "text") # Our standard log odds
      stargazer2(logitB, odd.ratio = T, type = "text") # Now the coefficients and significance is correct!
    
    
    
    
    
    
    
    
    
    
    
    
    
  #### MODEL C (ACC NOT HOH status on financial strain) ---------------------------
    
    # Restrict sample to co-residing ACCs 
    ACC2 <- ACC %>%
      filter(coreside==1) # n = 185
    
    
    logitC <- glm(cg_finc_strain ~ HoH_cg_no + 
                    age_cat2 + 
                    SEX7I + 
                    marital_cg + 
                    medicaid_dum +
                    inc_dich + 
                    educ_cg,  
                  data = ACC2,
                  family = binomial(link = "logit"))
    
    
    ## Model summary (log odds)
    summary(logitC) 
    
    
    
    #### Logit resources for interpreting:
      ## StackOverflow: Odds ratios instead of logits in stargazer() LaTeX output
        # https://stackoverflow.com/questions/16236560/odds-ratios-instead-of-logits-in-stargazer-latex-output
      ## Producing stargazer tables with odds ratios and standard errors in R
        # https://cimentadaj.github.io/blog/2016-08-22-producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/
      
    
    ## Get the odds ratio
      exp(coefficients(logitC))
      OR <- exp(coef(logitC)) 
    
      
      
    ## Get the SEs
      # Define a helper function to extract SE from glm output
      se.coef <- function(logitC){sqrt(diag(vcov(logitC)))}
      
      # Or, you can use the arm package
      se.coef <- arm::se.coef
      
      # Then, we can get the `StdErr.OR` by multiplying the two:
      Std.Error.OR <- OR * se.coef(logitC)
      
      # using Std Errors
      stargazer(logitC, type="text", coef=list(OR), se = list(Std.Error.OR), t.auto=F, p.auto=F)
      
      
    
    ## Calculate CIs
      # Have to compute from the original log odds exp(coef ± 1.96*SE)
      CI.OR <- as.matrix(exp(confint.default(logitC)))
      stargazer(logitC, type="text", coef=list(OR), ci.custom = list(CI.OR), t.auto=F, p.auto=F, ci = T)
      
      exp(confint(logitC))
      exp(confint.default(logitC)) #this one matches the above stargazer CI output
      
    
    ## Calculate P-values
      library(arm)
      z <- coef(logitC)/se.coef(logitC)
      pvalue <- 2*pnorm(abs(coef(logitC)/se.coef(logitC)), lower.tail = F)
      print(pvalue, digits = 3)
    
    
    
    ## Function for extracting the statistics separately and applying the appropriate conversions
      stargazer2 <- function(logitC, odd.ratio = F, ...) {
        if(!("list" %in% class(logitC))) logitC <- list(logitC)
        
        if (odd.ratio) {
          coefOR2 <- lapply(logitC, function(x) exp(coef(x)))
          seOR2 <- lapply(logitC, function(x) exp(coef(x)) * summary(x)$coef[, 2])
          p2 <- lapply(logitC, function(x) summary(x)$coefficients[, 4])
          stargazer(logitC, coef = coefOR2, se = seOR2, p = p2, ...)
          
        } else {
          stargazer(logitC, ...)
        }
      }
    
    
    ## Display 
      stargazer(logitC, type = "text") # Our standard log odds
      stargazer2(logitC, odd.ratio = T, type = "text") # Now the coefficients and significance is correct!

    
    
    
    
    
    
    
    
    
    
  #### MODEL D (Parent as the HOH on financial strain) ---------------------------
    
    ## Restrict sample to co-residing ACCs 
      ACC2 <- ACC %>%
        filter(coreside==1) # n = 185
    
    
    ## Estimate logit 
      logitD <- glm(cg_finc_strain ~ HoH_parent + 
                      age_cat2 + 
                      SEX7I + 
                      marital_cg + 
                      medicaid_dum +
                      inc_dich + 
                      educ_cg,  
                    data = ACC2,
                    family = binomial(link = "logit"))
    
    
    ## Model summary (log odds)
      summary(logitD) 
    
    
    #### Logit resources for interpreting:
      ## StackOverflow: Odds ratios instead of logits in stargazer() LaTeX output
        # https://stackoverflow.com/questions/16236560/odds-ratios-instead-of-logits-in-stargazer-latex-output
      ## Producing stargazer tables with odds ratios and standard errors in R
        # https://cimentadaj.github.io/blog/2016-08-22-producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/
      
    
    ## Get the odds ratio
      exp(coefficients(logitD))
      OR <- exp(coef(logitD)) 
    
      
      
    ## Get the SEs
      # Define a helper function to extract SE from glm output
      se.coef <- function(logitD){sqrt(diag(vcov(logitD)))}
      
      # Or, you can use the arm package
      se.coef <- arm::se.coef
      
      # Then, we can get the `StdErr.OR` by multiplying the two:
      Std.Error.OR <- OR * se.coef(logitD)
      
      # using Std Errors
      stargazer(logitD, type="text", coef=list(OR), se = list(Std.Error.OR), t.auto=F, p.auto=F)
      
      
    
    ## Calculate CIs
      # Have to compute from the original log odds exp(coef ± 1.96*SE)
      CI.OR <- as.matrix(exp(confint.default(logitD)))
      stargazer(logitD, type="text", coef=list(OR), ci.custom = list(CI.OR), t.auto=F, p.auto=F, ci = T)
      
      exp(confint(logitD))
      exp(confint.default(logitD)) #this one matches the above stargazer CI output
      
    
    ## Calculate P-values
      library(arm)
      z <- coef(logitD)/se.coef(logitD)
      pvalue <- 2*pnorm(abs(coef(logitD)/se.coef(logitD)), lower.tail = F)
      print(pvalue, digits = 3)
    
    
    
    ## Function for extracting the statistics separately and applying the appropriate conversions
      stargazer2 <- function(logitD, odd.ratio = F, ...) {
        if(!("list" %in% class(logitD))) logitD <- list(logitD)
        
        if (odd.ratio) {
          coefOR2 <- lapply(logitD, function(x) exp(coef(x)))
          seOR2 <- lapply(logitD, function(x) exp(coef(x)) * summary(x)$coef[, 2])
          p2 <- lapply(logitD, function(x) summary(x)$coefficients[, 4])
          stargazer(logitD, coef = coefOR2, se = seOR2, p = p2, ...)
          
        } else {
          stargazer(logitD, ...)
        }
      }
    
    
    ## Display 
      stargazer(logitD, type = "text") # Our standard log odds
      stargazer2(logitD, odd.ratio = T, type = "text") # Now the coefficients and significance is correct!
      
          
    