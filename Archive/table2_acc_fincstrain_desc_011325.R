#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

  # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025
  
  # TABLE 2: ACC Financial Strain by Co-residence Status

# Script created in RStudio ("Cranberry Hibiscus")
# updated 12/16/2024
#=======================================================================================================#


  #==============#
  #=== SET-UP ===#
  #==============#
  

  #### Load packages
    library(tidyverse)
    library(gtsummary)
    library(flextable)
    library(smd)
 
 
  #### Set working directory 
   setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files")
  
  
  
  #### Load df (see acc_table1 for data source & cleaning)
    # ACC
  
  
  #===============#
  #=== TABLE 2 ===#
  #===============#
  
  theme_gtsummary_compact()
  
  ### TABLE 2 ---------------------------
    ACC %>% 
    select(cg_finc_strain, cg_diff_help_kids, cg_diff_house, cg_diff_travel, 
           cg_diff_other_unspec, cg_receive_help, cg_inc_cat, cg_medicaid_dum, cg_coreside) %>% 
    mutate(
      cg_coreside = as.factor(cg_coreside),
      cg_coreside = fct_recode(cg_coreside, 
                               "Co-resident" = "1",
                               "Non-coresident" = "0"),
      cg_finc_strain = case_when(cg_finc_strain == 1 ~ "Yes", TRUE ~ "No"),
      cg_diff_help_kids = case_when(cg_diff_help_kids == 1 ~ "Yes", TRUE ~ "No"),
      cg_diff_house = case_when(cg_diff_house == 1 ~ "Yes", TRUE ~ "No"),
      cg_diff_travel = case_when(cg_diff_travel == 1 ~ "Yes", TRUE ~ "No"),
      cg_diff_other_unspec = case_when(cg_diff_other_unspec == 1 ~ "Yes", TRUE ~ "No"),
      cg_receive_help = case_when(cg_receive_help == 1 ~ "Yes", TRUE ~ "No"),
      cg_medicaid_dum = case_when(cg_medicaid_dum == 1 ~ "Yes", TRUE ~ "No")
    ) %>%
    tbl_summary(
      by = cg_coreside,
      type = list(everything() ~ "categorical"),
      statistic = list(all_categorical() ~ "{n} ({p}%)"),
      label = list(
        cg_finc_strain ~ "Financial strain",
        cg_diff_help_kids ~ "Supporting children",
        cg_diff_house ~ "Housing",
        cg_diff_travel ~ "Travel/Vacation",
        cg_diff_other_unspec ~ "Other/Unspecified",
        cg_receive_help ~ "Received financial help from parents",
        cg_inc_cat ~ "Household income (annual)",
        cg_medicaid_dum ~ "Medicaid"
      ),
      missing = "no",
      digits = list(all_categorical() ~ c(0, 1))
    ) %>%
    modify_header(
      label = "**Characteristic**"
    ) %>%
    add_overall(col_label = "**Overall**, N = 659") %>%
    modify_table_styling(
      columns = starts_with("stat_")
    ) %>%
    add_p(
      test = list(all_categorical() ~ "chisq.test"),
      pvalue_fun = ~ style_pvalue(.x, digits = 3)
    ) %>%
    modify_caption("**Table 2. Adult Child Caregiver Financial Strain by Co-residence**") 

    #%>% 
    #as_flex_table() %>%
    #save_as_docx(path = "acc_table2.docx")

    
    
    
    
    
    
    
    
