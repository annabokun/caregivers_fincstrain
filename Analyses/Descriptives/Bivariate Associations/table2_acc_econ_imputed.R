### TABLE 2 ---------------------------
ACC_full_imputed %>% 
  select(cg_finc_strain2, cg_diff_help_kids, cg_diff_house, cg_diff_travel, 
         cg_diff_other_unspec, cg_receive_help, cg_inc_cat, cg_medicaid_dum, cg_coreside) %>% 
    tbl_summary(
    by = cg_coreside,
    type = list(everything() ~ "categorical"),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    label = list(
      cg_finc_strain2 ~ "Financial strain",
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

print(table2)

#%>% 
#as_flex_table() %>%
#save_as_docx(path = "acc_table2.docx")