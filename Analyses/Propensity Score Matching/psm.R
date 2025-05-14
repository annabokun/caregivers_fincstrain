    # Install and load required packages
      install.packages(c("MatchIt", "cobalt", "sandwich", "lmtest"))
      library(MatchIt)
      library(cobalt)  # For assessing balance
      library(sandwich)  # For robust standard errors
      library(lmtest)  # For coefficient testing
      
    
      
    # Check variable types
      str(ACC[, c("cg_coreside", "cg_age_cat", "cg_female", "cg_marital_stat", 
                  "cg_educ_cat", "cg_inc_cat", "cg_medicaid_dum", "NHOUSE7I",
                  "cr_health", "AGE7", "ANYADL7I", "ANYIADL7I", "cr_dementia_binary",
                  "cr_own_home", "cr_married")])
      
      
    # Convert categorical variables to factors if they aren't already
      categorical_vars <- c("cg_coreside", "cg_age_cat", "cg_female", "cg_marital_stat", 
                            "cg_educ_cat", "cg_inc_cat", "cg_medicaid_dum",
                            "cr_health", "ANYADL7I", "ANYIADL7I", "cr_dementia_binary",
                            "cr_own_home", "cr_married")
      
    
      ACC <- ACC %>%
        mutate(across(all_of(categorical_vars), as.factor))
      
      # Verify conversion
      str(ACC[, categorical_vars])
      
      # If you need to check for specific levels in your factors:
      lapply(ACC[, categorical_vars], levels)
      
      
      
      
      
    # Step 1: Estimate the propensity score model
    # The treatment is co-residence (1 = co-residing, 0 = not co-residing)
    
    # Assuming your data frame is called 'acc_data'
      ps_model <- matchit(cg_coreside ~ cg_age_cat + cg_female + cg_marital_stat + cg_educ_cat + # Caregiver characteristics
                            cg_inc_cat + cg_medicaid_dum + NHOUSE7I + 
                            cr_health + AGE7 + ANYADL7I + ANYIADL7I + cr_dementia_binary + # Parent characteristics
                            cr_own_home + cr_married,
                          data = ACC,
                          method = "nearest",  # Nearest neighbor matching
                          caliper = 0.2,
                          ratio = 1)           # 1:1 matching
    
    # Step 2: Examine the matching results
      summary(ps_model)
      

      
      #Error in `matchit()`:
       # ! Missing values are not allowed in the treatment.
      
      
      # Check how many missing values are in the treatment variable
      sum(is.na(ACC$cg_coreside))
      
      ACC_clean <- ACC %>%
        dplyr::filter(
          !is.na(cg_coreside) &
            !is.na(cg_age_cat) &
            !is.na(cg_female) &
            !is.na(cg_marital_stat) &
            !is.na(cg_educ_cat) &
            !is.na(cg_inc_cat) &
            !is.na(cg_medicaid_dum) &
            !is.na(NHOUSE7I) &
            !is.na(cr_health) &
            !is.na(AGE7) &
            !is.na(ANYADL7I) &
            !is.na(ANYIADL7I) &
            !is.na(cr_dementia_binary) &
            !is.na(cr_own_home) &
            !is.na(cr_married)
        )
      
      
      colSums(is.na(ACC[, c("cg_coreside", "cg_age_cat", "cg_female", "cg_marital_stat",
                            "cg_educ_cat", "cg_inc_cat", "cg_medicaid_dum", "NHOUSE7I",
                            "cr_health", "AGE7", "ANYADL7I", "ANYIADL7I", "cr_dementia_binary",
                            "cr_own_home", "cr_married")]))
      
      table(is.na(ACC$HOUSEK_1))  # This will likely show 12 TRUE values
      

      
      ## IMPUTATION (better option)
        install.packages("mice")
        library(mice)
        
        
        
        
        
        
      # Select variables for imputation
        vars_for_impute <- c("cg_coreside", "cg_age_cat", "cg_female", "cg_marital_stat", 
                             "cg_educ_cat", "cg_inc_cat", "cg_medicaid_dum", "NHOUSE7I",
                             "cr_health", "AGE7", "ANYADL7I", "ANYIADL7I", "cr_dementia_binary",
                             "cr_own_home", "cr_married")
        
        impute_data <- ACC[vars_for_impute]
        
        
      # Inspect patterns of missingness
        md.pattern(impute_data)
        
        
        
      # Run the imputation (m = 5 is default)
        imp <- mice(impute_data, m = 5, method = "pmm", seed = 2025)
        
      # Extract the first imputed dataset
        complete_data <- complete(imp, 1)
        
      

        
        
        # Re-run your matching:
        ps_model <- matchit(
          cg_coreside ~ cg_age_cat + cg_female + cg_marital_stat + cg_educ_cat +
            cg_inc_cat + cg_medicaid_dum + NHOUSE7I +
            cr_health + AGE7 + ANYADL7I + ANYIADL7I + cr_dementia_binary +
            cr_own_home + cr_married,
          data = complete_data,
          method = "nearest",
          caliper = 0.2,
          ratio = 1)
        
        # Balance diagnostics
        summary(ps_model)
        love.plot(ps_model, threshold = 0.1, abs = TRUE, var.order = "unadjusted", stars = "raw")
        bal.tab(ps_model)
        
        # Inspect the exact SMDs being calculated.
        bal.tab(ps_model, un = TRUE)
        
        
        
        # Load required packages
        library(ggplot2)
        library(cobalt)
        
        # Create a named vector for variable renaming
        var_names <- c(
          "distance" = "Propensity score",
          "cg_female" = "Caregiver is female",
          "cg_age_cat_Early Adulthood (18-44)" = "Caregiver age: 18-44",
          "cg_age_cat_Middle Age (45-54)" = "Caregiver age: 45-54",
          "cg_age_cat_Late Middle Age (55-64)" = "Caregiver age: 55-64",
          "cg_age_cat_Early Retirement (65-75)" = "Caregiver age: 65-75",
          "cg_marital_stat" = "Caregiver marital status",
          "cg_educ_cat_Less than HS" = "Caregiver education: Less than HS",
          "cg_educ_cat_HS Graduate" = "Caregiver education: HS graduate",
          "cg_educ_cat_Some College" = "Caregiver education: Some college",
          "cg_educ_cat_Bachelor's+" = "Caregiver education: Bachelor's+",
          "cg_inc_cat_<$10,000" = "Caregiver income: <$10,000",
          "cg_inc_cat_$10,000-$19,999" = "Caregiver income: $10K-$20K",
          "cg_inc_cat_$20,000-$29,999" = "Caregiver income: $20K-$30K",
          "cg_inc_cat_$30,000-$49,999" = "Caregiver income: $30K-$50K",
          "cg_inc_cat_$50,000+" = "Caregiver income: $50,000+",
          "NHOUSE7I" = "Caregiver household size",
          "cg_medicaid_dum_1" = "Caregiver on Medicaid",
          "cg_medicaid_dum_0" = "Caregiver not on Medicaid",
          "cg_medicaid_dum_Do not know/refused" = "Caregiver Medicaid status unknown",
          "AGE7" = "Parent age",
          "cr_married" = "Parent marital status",
          "cr_health_fair/poor" = "Parent health: Fair/poor",
          "cr_dementia_binary" = "Parent has dementia",
          "ANYADL7I" = "Parent has ADL limitations",
          "ANYIADL7I" = "Parent has IADL limitations",
          "cr_own_home" = "Parent owns home"
        )
        
        
        
        # Create the love plot with custom variable names
        love.plot(ps_model,
                  var.order = "unadjusted",
                  abs = TRUE,
                  binary = "std",
                  thresholds = c(0.1),
                  colors = c("red", "blue"),
                  var.names = var_names,
                  stars = "raw", 
                  sample.names = c("Unmatched", "Matched"),
                  title = "Covariate Balance Before and After Matching",
                  subtitle = "Standardized Mean Differences (SMD < 0.1 indicates good balance)") + 
          annotate("text", 
                   x =1, y = 1.5,  # adjust these based on space
                   label = "Note: Covariates ordered by pre-match imbalance. SMD < 0.1 = good balance.", 
                   size = 3.5, 
                   hjust = 1, 
                   color = "gray30")
        
        
        # B&W version
        final_plot <- love.plot(
          ps_model,
          threshold = 0.1,
          abs = TRUE,
          binary = "std",
          var.order = "unadjusted",
          var.names = var_names,
          stars = "raw",
          sample.names = c("Unmatched", "Matched"),
          title = "Covariate Balance Before and After Matching",
          subtitle = "Standardized Mean Differences (SMD < 0.1 indicates good balance)",
          limits = c(0, 0.9),
          colors = c("black", "black"),
          shapes = c(16, 17),
          size = 3,
          line = FALSE
        ) +
          annotate("text", 
                   x = 0.88, y = 1.5,   # ✅ move within visible space
                   label = "Note: Covariates ordered by pre-match imbalance. SMD < 0.1 = excellent balance.", 
                   size = 3.5,
                   hjust = 1,
                   color = "gray30")
        final_plot
        
        # Check y range of plot 
        ggplot_build(final_plot)$layout$panel_params[[1]]$y.range
        
        # Save 
        ggsave("love_plot_bw.png", plot = final_plot, width = 8, height = 6, dpi = 600)
        
        
        

        
        
        
        
        
        
        
        

        
        
        
        
        
        
        
        
        