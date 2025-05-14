#### 3.) MOVEMENT VARIABLES - RECIPIENTS ---------------------------
            
        ## 3a.)  WHOMOV71 - Person-1 who moved in since last contact
            
            
            # Remove rows with NA in WHOMOV71
            df_no_na <- recipient[!is.na(recipient$WHOMOV71), ]
            
            # Create a summary dataframe with counts for each category
            summary_df <- as.data.frame(table(df_no_na$WHOMOV71))
            colnames(summary_df) <- c("WHOMOV71", "Count")
            
            # Convert WHOMOV71 to numeric
            summary_df$WHOMOV71 <- as.numeric(as.character(summary_df$WHOMOV71))
            
            # Create a vector of labels corresponding to WHOMOV71 values
            labels <- c(
              "Respondent is head",        # 1
              "Spouse",                    # 2
              "Son/daughter",              # 3
              "Son/daughter in law",       # 4
              "Grandchild",                # 5
              "Parent",                    # 6
              "Brother/sister",            # 7
              "Nephew/niece",              # 8
              "Cousin",                    # 9
              "Aunt/uncle",                # 10
              "Great grandchild",          # 11
              "Other",                     # 12
              "Friend",                    # 13
              "Boarder/roomer",            # 14
              "Paid employee",             # 15
              "All others",                # 16
              "Sister/brother in law"      # 17
            )
            
            # Add the labels to the summary dataframe
            summary_df$Label <- labels[summary_df$WHOMOV71]
            
            # Convert WHOMOV71 to a factor with levels ordered by count in descending order
            summary_df$WHOMOV71 <- factor(summary_df$WHOMOV71, levels = summary_df$WHOMOV71)
            
            
            
            # Create the bar chart and manually add labels
            ggplot(summary_df, aes(x = WHOMOV71, y = Count)) +
              geom_bar(stat = "identity") +
              xlab("WHOMOV71 Categories") +
              ylab("Count") +
              ggtitle("Who Moved in with the Care Recipient?") +
              geom_text(aes(label = Count), vjust = -0.5) +  # Manually add count labels
              scale_x_discrete(labels = summary_df$Label)  # Add custom labels for x-axis
            
            
            
            
        ## 3b.) TAKECARE7 - Person moved in because I needed help
            
            # Remove rows with NA in TAKECARE7
            df_no_na <- recipient[!is.na(recipient$TAKECARE7), ]
            
            ggplot(recipient, aes(x = reorder(TAKECARE7, -table(TAKECARE7)[TAKECARE7]))) +
              geom_bar() +
              xlab("TAKECARE7 Categories") +
              ylab("Count") +
              ggtitle("Person Moved In Because the Recipient Needs Help") + 
              geom_text(stat='count', aes(label=..count..), vjust=-0.5)  # Add labels
            
            
            
            
        ## 3c.) PLASTAY7 - Person moved in becuase they neede a place to stay
            
            # 2	== needed place to stay
            
            # Remove rows with NA in PLASTAY7
            df_no_na <- recipient[!is.na(recipient$TAKECARE7), ]
            
            ggplot(recipient, aes(x = reorder(PLASTAY7, -table(PLASTAY7)[PLASTAY7]))) +
              geom_bar() +
              xlab("PLASTAY7 Categories") +
              ylab("Count") +
              ggtitle("Person Moved In with Recipient Because They Need a Place to Stay") + 
              geom_text(stat='count', aes(label=..count..), vjust=-0.5)  # Add labels
          

          who_move_plus_play_to_stay <- recipient %>%
            tab(WHOMOV71,PLASTAY7)
                        
                        
                        
                        
                        
                        
            
            
            
            
                        
  #### Table 2. Adult children as caregivers demographics ---------------------------			
          
          
          
          
    #### 1.) ADULT CHILD CAREGIVER ---------------------------
      ## HHREL7i == 3
          
          # See relationships       
          relate <- caregiver_informant %>%
            tab(HHREL7I)
          
          
      # Age ---------------------------
          mean_age <- caregiver_informant %>%
            filter(HHREL7I ==3) %>%
            summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 54.90909
          
          # Alternative code 
          mean_agex <- mean(caregiver$AGE7I[caregiver$HHREL7I %in% c(1, 2)], na.rm = TRUE)
          
          
          
          
          
      # % Female --------------------------- 
              # 1	= Male
              # 2	= Female
          
          # Subset the df to only include rows where HOH or spouse of HOH
          child <- caregiver_informant %>%
            filter(HHREL7I ==3)
          
          # Calculate the % of women
          female_percent <- mean(child$SEX7I == 2, na.rm = TRUE) * 100
          # 77.2727
          
          
          
          
          
      # HH income --------------------------- 
          inc <- caregiver_informant %>%
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
          child <- caregiver_informant %>%
            filter(HHREL7I ==3)
          
          # Calculate the mean income
          mean_income <- mean(income_categories[child$HHREL7I], na.rm = TRUE)
          # 12,500
          
          # OR just mean of the category 
          # Filter the data frame based on the conditions
          filtered_df <- caregiver_informant %>%
            filter((HHREL7I == 1 | HHREL7I == 2) & !(LL3A7I %in% c(98, 99)))
          
          # Calculate the mean income category
          mean_income_category <- filtered_df %>%
            summarise(mean_income_category = mean(LL3A7I, na.rm = TRUE)) # 4.831919
          
          
          
          
          
          
          
      # HH size --------------------------- 
          mean_hh_size <- caregiver_informant %>%
            filter(HHREL7I == 3) %>%
            summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 4.772727
          
          
          # Married --------------------------- 
            child_caregiver_marst <- caregiver_informant %>%
              filter(HHREL7I == 3) %>%
              tab(CMARSTAT7I_1) # 36% of child caregivers are married 
  
            
          # n of child caregivers --------------------------- 
          child_caregivers <- caregiver_informant %>%
            filter(HHREL7I == 3) #= 22 
          
          
          
          
          
          
          
          
          
          
          
          
    #### 2.) ADULT CHILD CAREGIVER WHO MOVED IN WITH THE RECIPIENT ---------------------------
      ## OREL71:OREL76 == 3 & WHOMOV71 == 3  
          
          
          
     # Age ---------------------------
          mean_age <- caregiver_informant %>%
            filter(HHREL7I ==3 & WHOMOV71 == 3) %>%
            summarise(mean_age = mean(AGE7I, na.rm = TRUE)) # 54.90909
          
          # Alternative code 
          mean_agex <- mean(caregiver$AGE7I[caregiver$HHREL7I %in% c(1, 2)], na.rm = TRUE)
          
          
          
          
          
      # % Female --------------------------- 
              # 1	= Male
              # 2	= Female
          
          # Subset the df to only include rows where HOH or spouse of HOH
          child <- caregiver_informant %>%
            filter(HHREL7I ==3)
          
          # Calculate the % of women
          female_percent <- mean(child$SEX7I == 2, na.rm = TRUE) * 100
          # 77.2727
          
          
          
          
          
      # HH income --------------------------- 
          inc <- caregiver_informant %>%
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
          child <- caregiver_informant %>%
            filter(HHREL7I ==3)
          
          # Calculate the mean income
          mean_income <- mean(income_categories[child$HHREL7I], na.rm = TRUE)
          # 12,500
          
          # OR just mean of the category 
          # Filter the data frame based on the conditions
          filtered_df <- caregiver_informant %>%
            filter((HHREL7I == 1 | HHREL7I == 2) & !(LL3A7I %in% c(98, 99)))
          
          # Calculate the mean income category
          mean_income_category <- filtered_df %>%
            summarise(mean_income_category = mean(LL3A7I, na.rm = TRUE)) # 4.831919
          
          
          
          
          
          
          
      # HH size --------------------------- 
          mean_hh_size <- caregiver_informant %>%
            filter(HHREL7I == 3) %>%
            summarise(mean_hh_size = mean(NHOUSE7I, na.rm = TRUE)) # 4.772727
          
          
          # Married --------------------------- 
            child_caregiver_marst <- caregiver_informant %>%
              filter(HHREL7I == 3) %>%
              tab(CMARSTAT7I_1) # 36% of child caregivers are married 
  
            
          # n of child caregivers --------------------------- 
          child_caregivers <- caregiver_informant %>%
            filter(HHREL7I == 3) #= 22 
