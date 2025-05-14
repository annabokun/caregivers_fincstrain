
    # Since no missing values, add outcome to imputed/matched dataset
      complete_data$cg_finc_strain <- ACC$cg_finc_strain
     
    # Recreate matched dataset with outcome included
      matched_data <- match.data(ps_model)
      

      
      
      #### Set reference categories ---------------------------
      
      ## 1.) Age groups     
      matched_data$cg_age_cat <- factor(matched_data$cg_age_cat, levels = c("Early Retirement (65-75)", # ref 
                                                                            "Early Adulthood (18-44)", 
                                                                            "Middle Age (45-54)",
                                                                            "Late Middle Age (55-64)"))
      ## 2.) Education 
      matched_data$cg_educ_cat <- factor(matched_data$cg_educ_cat, levels = c("Less than HS", # ref 
                                                                              "HS Graduate", 
                                                                              "Some College",
                                                                              "Bachelor's+"))
      
      ## 3.) HH income
      matched_data$cg_inc_cat <- factor(matched_data$cg_inc_cat, levels = c("$10,000-$19,999", # ref
                                                                            "<$10,000",   
                                                                            "$20,000-$29,999",
                                                                            "$30,000-$49,999", 
                                                                            "$50,000+"
      ))
      
      ## 4.) Gender 
      matched_data$cg_female <- factor(matched_data$cg_female, levels = c(0, 1)) # ref 0 (MEN)
      
      
      ## 5.) Marital stat 
      matched_data$cg_marital_stat <- factor(matched_data$cg_marital_stat, levels = c(0, 1)) # ref 0 (not married)
      
      
          
      ## 7.) CR health
      matched_data$cr_health <- factor(matched_data$cr_health, levels = c("excellent/good", # ref 
                                                                          "fair/poor")) 
      
      
      
      ## Check reference levels ---------------------------
      levels(matched_data$cg_age_cat) 
      levels(matched_data$cg_educ_cat) 
      levels(matched_data$cg_inc_cat) 
      levels(matched_data$cr_health)
      
      
      
      ## Set theme for tables 
      theme_gtsummary_compact()
      
      
    # Run the Baseline Logit Model (Treatment Effect Only)
      logit_model <- glm(
        cg_finc_strain ~ cg_coreside,
        data = matched_data,
        family = binomial(link = "logit")
      )
      
      
    # Exponentiated ORs
      library(gtsummary)
      
      logit_table <- tbl_regression(
        logit_model,
        exponentiate = TRUE,
        label = list(
          cg_coreside ~ "Co-residence with parent"
        )
      ) %>%
        modify_caption("**Logistic Regression (Matched Sample): Effect of Co-residence on Financial Strain**")
      
      logit_table
      
      
      
      

      
      
    # 2. Estimate the Logit Model (Post-Matching) 
      logit_dr <- glm(cg_finc_strain ~ cg_coreside + cr_health + 
                 cg_age_cat + 
                 cg_female + 
                 cg_marital_stat + 
                 cg_educ_cat + 
                 cg_inc_cat, 
               family = binomial(link = "logit"), 
               data = matched_data)
     
      logit_dr_table <- tbl_regression(
       logit_dr, 
       exponentiate = TRUE,
       label = list(
         cg_coreside ~ "Co-residence with parent",
         cg_age_cat ~ "Age group",
         cg_female ~ "Female",
         cg_marital_stat ~ "Marital status",
         cg_educ_cat ~ "Education",
         cg_inc_cat ~ "Household income (annual)"
       )
     ) %>%
       modify_caption("**M1. Logistic Regression of Co-residence on Financial Strain**")
     
      logit_dr_table 
      
      
      
      
      #### Merge them into a single document --------------------------- 
      merged_tables <- tbl_merge(
        tbls = list(logit_table, logit_dr_table),
        tab_spanner = c("Treatment only", "With covariates")
      )
      
      print(merged_tables)
      
      
      
      
      # Convert to gt and format p-values
      gt_table <- merged_tables %>%
        as_gt() %>%
        gt::fmt_number(
          columns = dplyr::starts_with("p.value"), # or simply "p.value" if it's named that way
          decimals = 3
        )
      
      print(gt_table)
      
      
      
      # Save to Word
      gt_table %>%
        as_flex_table() %>%
        flextable::save_as_docx(path = "acc_regs_psm.docx")