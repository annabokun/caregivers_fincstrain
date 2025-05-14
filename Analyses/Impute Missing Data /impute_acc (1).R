
  ## 📦 Load Required Packages
    library(mice)
    library(tidyverse)
    
    
  ## 1. Subset to Variables You Actually Need
    # Alternatively, impute only variables with >5% missingness 
    vars_needed <- c("Q_NO",
      "cg_finc_strain", "cg_coreside", "cg_age_cat", "cg_female", "hh_size_mean", 
      "cg_marital_stat", "cg_marital_cat", "cg_educ_cat", "cg_educ_binary", "cg_inc_cat", "cg_inc_3cat",
      "AGE7I", "SEX7I", "cg_married", "cg_HOH", "cg_intensity", "NHOUSE7I", 
      "cg_health", "cg_chronic_any", "cg_arth", "cg_heart_probs", "cg_diab", "cg_cancer", 
      "cg_sad", "cg_sad_binary", "cg_health_util", "cg_hours_adl", "cg_hours_iadl", "cg_yrs_care", "cr_dementia2", 
      "cg_diff_help_kids", "cg_diff_house", "cg_diff_travel", "cg_diff_other_unspec",  
      "cg_receive_help", "cg_medicaid_dum",  
      "AGE7", "SEX7", "cr_married", "NHOUSE7", "cr_native",  
      "ANYADL7I", "ANYIADL7", "cr_dementia",
      "cr_health", "cr_sad", "cr_chronic_any", "cr_arth", "cr_heart_problems", "cr_diab", "cr_cancer", "QQ1B7",
      "cr_health_util", "cr_medicare", "cr_medicaid_dum", "cr_duals", "cr_private_dum", "cr_other_dum",   
      "cr_own_home", "cr_inc_adequate", "cr_inc_ss", "cr_inc_ssi", "cr_inc_pension", "cr_inc_property", "cr_inc_cg", "cr_inc_fam"
    )
    
    ACC_small <- ACC[, vars_needed]

    
    
# SAVE 
write.csv(ACC_small, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/ACC_small.csv")
    
    
    

    
  ## Smart Method Assignment for mice()
    
    
    # Detect and assign "logreg" to binary variables (0/1 or two-level factors),
    # Detect and assign "polyreg" to unordered factor variables with 3+ levels,
    # Default all others to "pmm" (good for continuous and many ordinal variables).
    
    
    # Step 1: Initialize method vector based on default guesses
    method <- make.method(ACC_small)
    
    # Step 2: Auto-assign based on variable types
    for (var in names(ACC_small)) {
      var_data <- ACC_small[[var]]
      
      if (is.factor(var_data)) {
        n_levels <- length(levels(var_data))
        if (n_levels == 2) {
          method[var] <- "logreg"  # binary factor
        } else {
          method[var] <- "polyreg"  # unordered factor with 3+ levels
        }
      } else if (is.numeric(var_data)) {
        n_unique <- length(unique(na.omit(var_data)))
        if (n_unique == 2) {
          method[var] <- "logreg"  # binary numeric (0/1)
        } else {
          method[var] <- "pmm"  # continuous variable
        }
      } else {
        method[var] <- ""  # exclude unsupported types
      }
    }
    
    # View assignment summary
    table(method) # logreg: 38 
                  # pmm: 13 
                  # 
    
    names(method[method == ""])
    
    
    
    
    # ✅ Fix: Convert to Proper Factor or Numeric Format

    # Convert character or labelled categorical variables to proper factor
    ACC_small$cg_age_cat <- as.factor(ACC_small$cg_age_cat)
    ACC_small$cg_marital_cat <- as.factor(ACC_small$cg_marital_cat)
    ACC_small$cg_educ_cat <- as.factor(ACC_small$cg_educ_cat)
    
    # Convert ordinal categories if needed (e.g., 1 = poor to 5 = excellent)
    ACC_small$cg_health <- as.numeric(ACC_small$cg_health)
    
    # Make sure binary vars like cg_medicaid_dum are 0/1
    ACC_small$cg_medicaid_dum <- as.numeric(ACC_small$cg_medicaid_dum)
    
    
          # Then re-run the method assignment block
              # logreg: 39     
              # pmm: 14
              # polyreg: 3  
              # method: 10
    
    names(method[method == ""])
    
    
    

    

    
    
    
    
    
    
    
    
    
    
    
    
    
  ## 2. Use quickpred() to Reduce Predictors
    pred_matrix <- quickpred(ACC_small, mincor = 0.1) # Only use predictors with r > 0.1
                                                      # Only include variables as predictors in the imputation model 
                                                      # if their correlation with the target variable is greater than 0.1 (in absolute value).
    
    
  ## 3.) Impute missing vars
    imp_ACC <- mice(ACC_small, m = 5, predictorMatrix = pred_matrix, seed = 123)
    
    
    imp_ACC <- mice(ACC_small, m = 5, method = method, predictorMatrix = pred_matrix, seed = 123, printFlag = FALSE)
    
    

    
    
    
    
  ## 🔁 Step 3: Run MICE Imputation
    # Run imputation
    imp_ACC <- mice(ACC, m = 5, method = "pmm", seed = 123)
    
    # Check a summary
    summary(imp_ACC)
    
    
    
  ## 💾 Step 4: Extract Completed Data (for Descriptives)
    ACC_imputed <- complete(imp_ACC, 1)
    
    
  ## CHECK IMPUTATION 
    
    # 🔍 a.) Trace plots (to check convergence across imputations)
      plot(imp_ACC)
      
      
    # 📊 b) Compare observed vs. imputed for one variable
      densityplot(imp_ACC, ~HOUSEHOLD_INCOME_VAR_NAME)  # Replace with your variable
      
    
    
    

