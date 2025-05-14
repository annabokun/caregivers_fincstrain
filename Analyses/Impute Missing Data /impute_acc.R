
  ## 📦 Load Required Packages
    library(mice)
    library(tidyverse)
    
    
  ## 1. Subset to relevant variables 
    vars_needed <- c("Q_NO", "cg_age_cat", "cg_female",  
      "cg_marital_stat", "cg_marital_cat", "cg_educ_cat", "cg_educ_binary", "cg_inc_cat", "cg_inc_binary",
      "AGE7I", "SEX7I", "cg_married", "cg_HOH", "cg_intensity", "NHOUSE7I", 
      "cg_health", "cg_chronic_any", "cg_arth", "cg_heart_probs", "cg_diab", "cg_cancer", 
      "cg_sad", "cg_sad_binary", "cg_health_util", "cg_hours_adl", "cg_hours_iadl", "cg_yrs_care",
      "cg_diff_help_kids", "cg_diff_house", "cg_diff_travel", "cg_diff_other_unspec",  
      "cg_receive_help", "cg_medicaid_dum",  
      "AGE7", "SEX7", "cr_married", "NHOUSE7", "cr_native",  
      "ANYADL7I", "ANYIADL7", "cr_dementia2",
      "cr_health", "cr_sad", "cr_chronic_any2", "cr_arth", "cr_heart_problems2", "cr_diab", "cr_cancer", "QQ1B7",
      "cr_health_util", "cr_medicare", "cr_medicaid_dum", "cr_duals", "cr_private_dum", "cr_other_dum",   
      "cr_own_home", "cr_inc_adequate", "cr_inc_ss", "cr_inc_ssi", "cr_inc_pension", "cr_inc_property", "cr_inc_cg", "cr_inc_fam"
    )
    
    ACC_small <- ACC[, vars_needed]

    
    
# SAVE 
write.csv(ACC, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/ACC.csv")
write.csv(ACC_small, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/ACC_small.csv")
    

    

  ## 2. See missingness
    missing_summary <- function(data) {
      missing_pct <- sapply(data, function(x) mean(is.na(x)) * 100)
      summary_df <- data.frame(
        variable = names(missing_pct),
        percent_missing = round(missing_pct, 1)
      )
      summary_df <- summary_df[order(-summary_df$percent_missing), ]
      rownames(summary_df) <- NULL
      return(summary_df)
    }

    missing_summary(ACC_small)
    

    

    
  ## 3.) Impute vars that are ~10% missing or less (exception = cr_sad at 11.2%)
    vars_to_impute <- c(
      "cr_sad", "cg_hours_iadl", "cg_inc_cat", "cg_inc_binary", "cr_own_home", "cg_yrs_care",
      "cr_inc_ssi", "cr_other_dum", "cr_inc_pension", "cr_private_dum", "cr_medicaid_dum", 
      "cr_inc_ss", "cg_medicaid_dum", "cg_educ_binary", "cg_educ_cat", "cr_duals", 
      "cr_medicare", "cr_inc_adequate", "cg_arth", "NHOUSE7", "cr_arth", "cg_cancer", 
      "cg_diab", "cg_married", "cg_marital_cat", "cg_marital_stat", "cg_health", 
      "cg_receive_help", "cr_health", "cr_cancer", "cr_married", "cg_health_util", 
      "cg_sad_binary", "cg_sad", "cr_dementia2", "cg_HOH", "AGE7I", "cg_age_cat"
    )
    
    ACC_subset <- ACC_small[, vars_to_impute]
    
    
    
    
    
  ## 4.) Smart Method Assignment

    # Create method vector
    method <- make.method(ACC_subset)
    
    # Smart method assignment
    for (var in names(ACC_subset)) {
      x <- ACC_subset[[var]]
      x_clean <- na.omit(x)
      n_unique <- length(unique(x_clean))
      
      # Recode character to factor
      if (is.character(x)) {
        ACC_subset[[var]] <- as.factor(x)
        x <- ACC_subset[[var]]
      }
      
      # Assign method
      if (n_unique == 1) {
        method[var] <- ""  # Skip constant
      } else if (is.factor(x)) {
        if (n_unique == 2) {
          method[var] <- "logreg"
        } else {
          method[var] <- "polyreg"
        }
      } else if (is.numeric(x)) {
        if (n_unique == 2) {
          ACC_subset[[var]] <- as.factor(x)  # Recode binary numeric as factor
          method[var] <- "logreg"
        } else {
          method[var] <- "pmm"
        }
      } else {
        method[var] <- ""  # catch-all fallback
      }
    }

    table(method)
        # logreg: 28 
        # pmm: 3
        # polyreg: 7 

    
    # Save a CSV of method assignments for transparency
      write.csv(data.frame(variable = names(method), method = method),
                "mice_method_assignment.csv", row.names = FALSE)
      


      
      

  # OPTIONAL: Convert Binary Variables to Factor (Before Imputation)
      
      # Step 1: Identify all variables assigned to "logreg"
      logreg_vars <- names(method[method == "logreg"])
      
      # Step 2: Convert those to factors
      ACC_subset[logreg_vars] <- lapply(ACC_subset[logreg_vars], as.factor)
      
      
      
      

      
  #### IMPUTATION  
        
    # Generate a predictor matrix (lightweight & relevant)
      pred_matrix <- quickpred(ACC_subset, mincor = 0.1)
                                      # Only use predictors with r > 0.1
                                      # Only include variables as predictors in the imputation model 
                                      # if their correlation with the target variable is greater than 0.1 (in absolute value)
      
    # Run the imputation
      imp_ACC <- mice(
        data = ACC_subset,
        m = 5,
        method = method,
        predictorMatrix = quickpred(ACC_subset, mincor = 0.1),
        seed = 123,
        printFlag = TRUE  # Set to FALSE if you prefer no output
      )
      

    # Check a summary
    summary(imp_ACC)
    
    
    
  ## 💾 Extract Completed Data
    ACC_imputed <- complete(imp_ACC, 1)
    
    
    
  ## CHECK IMPUTATION
    
    # Basic convergence plot for all variables
      plot(imp_ACC)
    
    # Density plots 
      library(lattice)
    
    # Check high-missing vars 
      densityplot(imp_ACC, ~cg_inc_cat)
      densityplot(imp_ACC, ~cg_inc_binary)
      densityplot(imp_ACC, ~cg_hours_iadl)
      densityplot(imp_ACC, ~cr_own_home)  
      densityplot(imp_ACC, ~cg_educ_cat) 
      
    

      

# SAVE 
write.csv(ACC_imputed, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/ACC_imputed.csv")

    
    






#### MERGE NEWLY IMPUTED VARIABLES TO MAIN SAMPLE 

  
  # Get names of the imputed variables only
  imputed_vars <- names(imp_ACC$imp)
  
  
  # Replace only the imputed columns
  ACC_full_imputed <- ACC
  ACC_full_imputed[, imputed_vars] <- complete(imp_ACC, 1)[, imputed_vars]
  
  

  

# SAVE 
write.csv(ACC_full_imputed, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/ACC_full_imputed.csv")
  