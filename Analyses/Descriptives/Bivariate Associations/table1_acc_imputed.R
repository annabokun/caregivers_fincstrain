  
  #### Load packages
  library(tidyverse)
  library(gtsummary)
  library(flextable)
  library(smd)
  

# TABLE 1 
  # Run without table1_acc <- if want to see in the right hand side of the console immediately, otherwise it will save to Word
 theme_gtsummary_compact()  
 
 table1_acc <- ACC_full_imputed %>% 
    select(cg_finc_strain, AGE7I, SEX7I, cg_educ_binary, cg_educ_cat, 
           cg_marital_stat, cg_inc_cat, cg_inc_binary, # demographics
           cg_coreside, cg_HOH, NHOUSE7I, # living arrangements
           cg_health, cg_chronic_any, cg_arth, cg_heart_problems, cg_diab, cg_cancer, # physical health 
           cg_sad, cg_health_util, # mental health + util
           cg_hours_adl, cg_hours_iadl, cg_yrs_care, cr_dementia2) %>% # caregiving intensity
    mutate(
      # Ensure continuous variables are numeric
      AGE7I = as.numeric(AGE7I),
      NHOUSE7I = as.numeric(NHOUSE7I),
      cg_hours_adl = as.numeric(cg_hours_adl),
      cg_hours_iadl = as.numeric(cg_hours_iadl),
      # Create factor with specific level order for financial strain
      cg_finc_strain = factor(cg_finc_strain, 
                              levels = c(1, 0),
                              labels = c("Financial strain", "No financial strain"))
    ) %>%
    tbl_summary(
      by = cg_finc_strain,
      type = list(
        "AGE7I" ~ "continuous",
        "NHOUSE7I" ~ "continuous",
        "cg_hours_adl" ~ "continuous",
        "cg_hours_iadl" ~ "continuous"
      ),
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      label = list(
        AGE7I ~ "Age",
        SEX7I ~ "Female",
        cg_educ_binary ~ "Education (binary)",
        cg_educ_cat ~ "Education", 
        cg_marital_stat ~ "Married",
        cg_inc_cat ~ "Household income (annual)", 
        cg_inc_binary ~ "Household income (annual) (binary)",
        cg_coreside ~ "Co-reside with parent",
        cg_HOH ~ "Caregiver - head of household",
        NHOUSE7I ~ "Household size", 
        cg_health ~ "Self-rated health", 
        cg_chronic_any ~ "Any chronic condition",
        cg_arth ~ "Arthritis",
        cg_heart_problems ~ "Cardiovascular disease",
        cg_diab ~ "Diabetes",
        cg_cancer ~ "Cancer",
        cg_sad ~ "Felt sad",
        cg_health_util ~ "Healthcare utilization",
        cg_hours_adl ~ "ADL hours/day",
        cg_hours_iadl ~ "IADL hours/day",
        cg_yrs_care ~ "Years caregiving",
        cr_dementia2 ~ "Care recipient has dementia"
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
    add_overall(col_label = "**Total Sample**, N = 659") %>%
    modify_table_styling(
      columns = starts_with("stat_")
    ) %>%
    add_p(
      test = list(
        all_continuous() ~ "aov",     # ANOVA for continuous variables
        all_categorical() ~ "chisq.test"  # Chi-square for categorical variables
      ),
      pvalue_fun = ~ style_pvalue(.x, digits = 3)
    ) %>%
    modify_caption("**Table 1. Adult Children Caregivers: Demographic, Health, and Caregiving Characteristics**") %>%
   as_flex_table() %>%
   save_as_docx(path = "table1_acc.docx")
 

