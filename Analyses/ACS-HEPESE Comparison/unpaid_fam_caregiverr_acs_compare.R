  

      ### IPUMS note from Ivan
        # I highly recommend reviewing the detailed documentation pages for MOMLOC/POPLOC to determine if you would like to leverage this additional information. 
        # Note that these parental links are used for our attach characteristics tool (available in the last screen before submit your extracts). 
        # Using this tool, you can use the variables in your data cart to add data about a person’s mother, father, household head, or spouse as an additional column in your data extract. 
        # For example, you can append mother’s and father’s age to the person’s record for analysis.



## 1d.) Check sample size if I add a filter for unpaid family worker (i.e., proxy for family caregiver) ---------------------------
  # CLASSWKRD == 29 = unpaid family worker --> https://usa.ipums.org/usa-action/variables/CLASSWKR#codes_section
  acs_ma_fam_worker <- mex_hh %>%
    group_by(hh_id) %>%      # Ensure in same HH 
    filter(GQ %in% 1:2 | GQ==5) %>%  # Exclude group quarters 
    mutate(                                                                    # Get parent ages for each person
      mother_age = if_else(MOMLOC > 0, AGE[match(MOMLOC, PERNUM)], NA_real_),  # If MOMLOC > 0 (mother lives in same HH), then find her age; If MOMLOC = 0 (no mother in household), assigns NA
      father_age = if_else(POPLOC > 0, AGE[match(POPLOC, PERNUM)], NA_real_)) %>%
    filter(any(AGE >= 18 & CLASSWKRD == 29 & (mother_age >= 80 | father_age >= 80))) %>% # Keep HHs where at least 1 person is an adult unpaid family caregiver (18+) AND has at least 1 parent (80+) 
    ungroup()                                                                          # Could further restrict by TRANSPORT to work (==home) to get closer to identifying caregiver for parent
  
  # Unweighted n
  length(unique(acs_ma_fam_worker$person_id))
    # n = 36 (parents 80+)

  
  # Weighted n 
  acs_ma_fam_worker_n <- sum(acs_ma_fam_worker$PERWT) # n = 2,883
                                                      
  
  
  #====================#
  #=== ACS CLEANING ===#
  #====================#
  
  #### Collapse EDUC into HS/No HS categories ---------------------------
  acs_ma_fam_worker <- acs_ma_fam_worker %>%
    mutate(
      hs_binary = case_when(
        EDUC %in% 0:5 ~ "Less than High School",
        EDUC %in% 6:11 ~ "High School or More",
        TRUE ~ NA_character_)) # Handle missing values
  
  # Weighted proportions for HS/No HS
  acs_ma_fam_worker_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_ma_fam_worker)
  hs_distribution <- svymean(~factor(hs_binary), acs_ma_fam_worker_svy, na.rm = TRUE)
  print(hs_distribution)
  
  
  
  
  
  #### Collapse HH income ($20k binary) ---------------------------   
  summary(acs_ma_fam_worker$HHINCOME)
  
  # Create 3-category income variable
  acs_ma_fam_worker <- acs_ma_fam_worker %>%
    mutate(
      inc_3cat = case_when(
        HHINCOME < 20000 ~ "< $20,000",
        HHINCOME >= 20000 ~ "$20,000+",
        is.na(HHINCOME) ~ "Missing",
        TRUE ~ NA_character_))
  
  # Weighted proportions for HS/No HS
  acs_ma_fam_worker_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_ma_fam_worker)
  income_cat_pct <- svymean(~factor(inc_3cat), acs_ma_fam_worker_svy, na.rm = TRUE)
  print(income_cat_pct)
  
  
  
  
  
  #### Collapse married ---------------------------   
  acs_ma_fam_worker <- acs_ma_fam_worker %>%
    mutate(
      married_dummy = case_when(
        MARST %in% c(1, 2) ~ 1,  # Married (spouse present or absent)
        MARST %in% c(3, 4, 5, 6) ~ 0,  # Not married (separated, divorced, widowed, never married)
        TRUE ~ NA_real_))  # Handle missing values
  
  # Weighted proportions for HS/No HS
  acs_ma_fam_worker_svy <- svydesign(ids = ~1, weights = ~PERWT, data = acs_ma_fam_worker)
  married_pct <- svymean(~factor(married_dummy), acs_ma_fam_worker_svy, na.rm = TRUE)
  print(married_pct)
  
  # Or, can calculate weighted percentages this way too
  married_pct <- svymean(~married_dummy, acs_ma_fam_worker_svy, na.rm = TRUE) * 100    
  
  
  
  
  #### Visually inspect sample ---------------------------  
  samp_acs <- acs_ma_fam_worker %>%
    select(YEAR, SERIAL, PERNUM, STATEFIP, SEX, AGE, RELATE, MOMLOC, POPLOC, NCHILD, 
           MARST, married_dummy, RACE, HISPAN, EDUC, hs_binary, HHINCOME, inc_3cat, NUMPREC, FAMSIZE, GQ, PERWT)
  
  
  
  
  #### SAVE 
  write.csv(acs_ma_fam_worker, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/acs_ma_fam_worker.csv")
  write.csv(samp_acs, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/samp_acs.csv")
  
  
  
  
  
  #============================#
  #=== COMPARE HEPESE & ACS ===#
  #============================#
  
  #### 1. HEPESE sample ---------------------------
  ## ACC df generated in the 01_acc_clean script (RELSUBJ7I==3 | RELSUBJ7I==4)
  
  
  ## Gen co-residential sample 
  ACC_cg_coreside <- ACC %>%
    filter(HOUSEK_1==1) # n = 191
  
  
  ## Set compact theme for gtsummary
  theme_gtsummary_compact()
  
  
  ## Gen HEPESE table
  ACC_cg_coreside %>%  # MUST BE CO-RESIDENTIAL SAMPLE TO MATCH THE ACS
    select(AGE7I, SEX7I, cg_educ_binary, cg_married, cg_inc_3cat, # demographics 
           NHOUSE7I, # hh size
           OO79LANGI) %>% # language used during interview
    mutate(
      # Ensure continuous variables are numeric
      AGE7I = as.numeric(AGE7I),
      NHOUSE7I = as.numeric(NHOUSE7I)
    ) %>%
    tbl_summary(
      type = list(
        "AGE7I" ~ "continuous",
        "NHOUSE7I" ~ "continuous"
      ),
      statistic = list(
        all_continuous() ~ "{mean}",
        all_categorical() ~ "{n} ({p}%)"
      ),
      label = list(
        AGE7I ~ "Age",
        SEX7I ~ "Female",
        cg_educ_binary ~ "Education",
        cg_married ~ "Married",
        cg_inc_3cat ~ "Household income (annual)",
        NHOUSE7I ~ "Household size", 
        OO79LANGI ~ "Language"
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
    modify_table_styling(
      columns = starts_with("stat_")
    ) %>%
    modify_caption("**HEPESE vs ACS Caregiver Demographics: Adult Children (18+) Living with Aging Parents (80+) in AZ, CA, CO, NM, TX (2010–2011)**") 
  
  
  
  #### SAVE 
  write.csv(ACC_cg_coreside, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/ACC_cg_coreside.csv")
  
  
  
  
  
  #### 2. ACS sample ---------------------------
  
  ## Get just the adult children (not their parents or other HH members) ---------------------------   
  # Otherwise the descriptives will reflect pooled data for children + parents (e.g., age will be skewed)  
  adult_children_acs <- acs_ma_fam_worker %>%
    filter(AGE >= 18 & (mother_age >= 80 | father_age >= 80)) # Keep only adults who have at least one elderly parent in the household
  
  unique(length(adult_children_acs$person_id)) # n = 10
  
  # Visually inspect sample ---------------------------  
  samp_kids <- adult_children_acs %>%
    select(YEAR, SERIAL, PERNUM, STATEFIP, RELATE, RELATED, MOMLOC, POPLOC, NCHILD, 
           SEX, AGE, mother_age, father_age, MARST, married_dummy, RACE, HISPAN, EDUC, hs_binary, HHINCOME, inc_3cat, NUMPREC, FAMSIZE, PERWT)
  
  
  
  #### ACS TABLE ---------------------------
  adult_children_acs %>% # ALREADY CO-RESIDENTIAL, SINCE IT'S A HOUSEHOLD-BASED SURVEY
    mutate(
      SEX = haven::as_factor(SEX),   # convert haven_labelled columns to factors (otherwise get warning)
      LANGUAGE = haven::as_factor(LANGUAGE)
    ) %>%
    as_survey_design(weight = PERWT) %>% # weights
    select(
      AGE, SEX, hs_binary, married_dummy, inc_3cat, # demographics
      NUMPREC, # hh size
      LANGUAGE # language used during interview
    ) %>%
    mutate(
      # Ensure continuous variables are numeric
      AGE = as.numeric(AGE),
      NUMPREC = as.numeric(NUMPREC)
    ) %>%
    tbl_svysummary(
      type = list(
        "AGE" ~ "continuous",
        "NUMPREC" ~ "continuous"
      ),
      statistic = list(
        all_continuous() ~ "{mean}",
        all_categorical() ~ "{n} ({p}%)"
      ),
      label = list(
        AGE ~ "Age",
        SEX ~ "Female",
        hs_binary ~ "Education",
        married_dummy ~ "Married",
        inc_3cat ~ "Household income (annual)",
        NUMPREC ~ "Household size", 
        LANGUAGE ~ "Language"
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
    modify_table_styling(
      columns = starts_with("stat_")
    ) %>%
    modify_caption("**HEPESE vs ACS Caregiver Demographics: Adult Children (18+) Living with Aging Parents (80+) in AZ, CA, CO, NM, TX (2010–2011)**")
  
  
  
  #### SAVE 
  write.csv(adult_children_acs, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/adult_children_acs.csv")
  
  
  
  
  
  ## 1. Basic age statistics
  # ACS
  hist(adult_children_acs$AGE,
       main = "ACS: adult children age distribution") 
  
  # HEPESE
  hist(ACC$AGE7I,
       main = "HEPESE: adult children age distribution")  
  
  adult_children_acs %>%
    as_survey_design(weight = PERWT) %>%
    summarise(
      mean_age = survey_mean(AGE),
      min_age = min(AGE),
      max_age = max(AGE)
    )
  
  ## 2. Age groups distribution
  adult_children_acs %>%
    mutate(
      age_group = case_when(
        AGE >= 18 & AGE < 25 ~ "18-24",
        AGE >= 25 & AGE < 35 ~ "25-34",
        AGE >= 35 & AGE < 45 ~ "35-44",
        AGE >= 45 & AGE < 55 ~ "45-54",
        AGE >= 55 & AGE <= 70 ~ "55-70"
      )
    ) %>%
    as_survey_design(weight = PERWT) %>%
    group_by(age_group) %>%
    summarise(
      n_unweighted = n(),
      n_weighted = sum(PERWT),
      pct = n_weighted/sum(n_weighted)*100
    ) %>%
    arrange(age_group)
  
  # A tibble: 5 × 4
  # age_group n_unweighted n_weighted   pct
  # <chr>            <int>      <dbl> <dbl>
  #  1 18-24              350      40875   100
  #  2 25-34              188      22353   100
  #  3 35-44               43       5035   100
  #  4 45-54               13       2525   100
  #  5 55-70                2        188   100
  
  
  ## 3. Check the Distribution of MARST
  adult_children_acs %>%
    as_survey_design(weights = PERWT) %>%
    group_by(MARST) %>%
    summarise(
      count = survey_total(),
      percent = survey_mean()
    )
  
  ## 4. Check the married_dummy calculation
  table(acs_ma_fam_worker$MARST)
  table(acs_ma_fam_worker$married_dummy)
  
  adult_children_acs %>%
    as_survey_design(weights = PERWT) %>%
    summarise(
      married_pct = survey_mean(married_dummy, na.rm = TRUE) * 100
    )
  
  
