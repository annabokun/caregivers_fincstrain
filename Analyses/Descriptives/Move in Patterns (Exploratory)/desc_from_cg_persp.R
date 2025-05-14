

# Create a function for formatting tables
format_table <- function(df, caption, col_names = NULL) {
  if(is.null(col_names)) {
    col_names <- names(df)
  }
  
  kable(df, caption = caption, col.names = col_names, align = c("l", rep("c", ncol(df)-1))) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    row_spec(0, bold = TRUE)
}

#################################################################
# PART 2: CAREGIVER/INFORMANT PERSPECTIVE
#################################################################

# 1. Co-residence analysis
cg_coresidence_analysis <- ACC %>%
  summarize(
    total_dyads = n(),
    co_residing = sum(cg_coreside == 1, na.rm = TRUE),
    elder_owns_home = sum(LL76 == 1, na.rm = TRUE),
    elder_owns_and_cg_coresides = sum(LL76 == 1 & cg_coreside == 1, na.rm = TRUE),
    caregiver_is_hoh = sum(cg_HOH == 1, na.rm = TRUE),
    cg_hoh_and_cg_coresides = sum(cg_HOH == 1 & cg_coreside == 1, na.rm = TRUE)
  ) %>%
  mutate(
    pct_coresiding = round(100 * co_residing / total_dyads, 1),
    pct_elder_owns = round(100 * elder_owns_home / total_dyads, 1),
    pct_elder_owns_cg_coresides = round(100 * elder_owns_and_cg_coresides / total_dyads, 1),
    pct_cg_hoh = round(100 * caregiver_is_hoh / total_dyads, 1),
    pct_cg_hoh_cg_coresides = round(100 * cg_hoh_and_cg_coresides / total_dyads, 1)
  )

cg_coresidence_table <- data.frame(
  Pattern = c(
    "Care recipient and caregiver co-reside",
    "Care recipient owns their home",
    "Care recipient owns home AND co-resides with caregiver",
    "Caregiver is head of household",
    "Caregiver is head of household AND co-resides with the care recipient"
  ),
  Count = c(
    cg_coresidence_analysis$co_residing,
    cg_coresidence_analysis$elder_owns_home,
    cg_coresidence_analysis$elder_owns_and_cg_coresides,
    cg_coresidence_analysis$caregiver_is_hoh,
    cg_coresidence_analysis$cg_hoh_and_cg_coresides
  ),
  Percentage = paste0(
    c(
      cg_coresidence_analysis$pct_coresiding,
      cg_coresidence_analysis$pct_elder_owns,
      cg_coresidence_analysis$pct_elder_owns_cg_coresides,
      cg_coresidence_analysis$pct_cg_hoh,
      cg_coresidence_analysis$pct_cg_hoh_cg_coresides
    ),
    "%"
  )
)

format_table(
  cg_coresidence_table,
  caption = paste0("Co-residence Patterns (N=", cg_coresidence_analysis$total_dyads, ")"),
  col_names = c("Living Arrangement Pattern", "Count", "% of All Caregiver-Care Recipient Dyads")
)





# 2. Create move-in typology based on household structure
# This helps infer who moved in with whom
data <- ACC %>%
  mutate(
    move_in_typology = case_when(
      # Elder likely moved in with caregiver
      cg_coreside == 1 & cg_HOH == 1 & LL76 == 2 ~ "Care recipient/parent likely moved in with caregiver", # ZERO OBSERVATIONS
      
      # Caregiver likely moved in with elder
      cg_coreside == 1 & cg_HOH == 0 & LL76 == 1 ~ "Caregiver likely moved in with care recipient/parent",
      
      # Caregiver and elder co-reside, but elder still owns the home, CG is HOH
      cg_coreside == 1 & cg_HOH == 1 & LL76 == 1 ~ "Care recipient/parent owns home but caregiver is HOH",
      
      # Co-residing but structure unclear
      cg_coreside == 1 ~ "Other co-residence arrangement",
      
      # Not co-residing
      TRUE ~ "Not co-residing"
    )
  )

typology_summary <- data %>%
  filter(!is.na(move_in_typology)) %>%
  group_by(move_in_typology) %>%
  summarize(
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = round(100 * count / sum(count), 1)
  ) %>%
  arrange(desc(count))

format_table(
  typology_summary,
  caption = paste0("Inferred Move-In Typology (N=", sum(typology_summary$count), ")"),
  col_names = c("Move-In Arrangement", "Count", "Percentage")
)




# 3. Cross-tabulation of household headship and co-residence
hoh_cg_coresidence <- with(ACC, table(
  "Caregiver is Head of Household" = factor(cg_HOH, levels = c(0, 1), 
                                            labels = c("No", "Yes")),
  "Co-residence" = factor(cg_coreside, levels = c(FALSE, TRUE), 
                          labels = c("No", "Yes")),
  useNA = "ifany"
))

# Display cross-tabulation as a formatted table
format_table(
  as.data.frame.matrix(hoh_cg_coresidence), 
  caption = "Cross-tabulation of Household Headship and Co-residence") 




# 4. Financial strain by household structure and move-in typology
financial_strain <- data %>%
  filter(!is.na(move_in_typology), !is.na(cg_finc_strain)) %>%
  group_by(move_in_typology) %>%
  summarize(
    n = n(),
    financial_strain_count = sum(cg_finc_strain == 1, na.rm = TRUE),
    financial_strain_pct = round(100 * mean(cg_finc_strain == 1, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(financial_strain_pct))

format_table(
  financial_strain,
  caption = "Financial Strain by Move-In Arrangement",
  col_names = c("Move-In Arrangement", "N", "# with Financial Strain", "% with Financial Strain")
)



# 5. Create a visualization of financial strain by living arrangement
# Focus only on co-residing arrangements for clarity
strain_plot_data <- financial_strain %>%
  filter(move_in_typology != "Not co-residing")

ggplot(strain_plot_data, 
       aes(x = reorder(move_in_typology, financial_strain_pct), 
           y = financial_strain_pct)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(financial_strain_pct, "%")), 
            vjust = -0.5) +
  labs(title = "Financial Strain by Move-In Arrangement",
       subtitle = "Percentage of caregivers reporting financial strain",
       x = NULL,
       y = "Percentage Reporting Financial Strain") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 6. Analyze caregiver characteristics by living arrangement type
caregiver_by_type <- data %>%
  filter(!is.na(move_in_typology)) %>%
  group_by(move_in_typology) %>%
  summarize(
    n = n(),
    mean_age = mean(AGE7I, na.rm = TRUE),
    pct_female = mean(cg_female == 1, na.rm = TRUE) * 100,
    pct_married = mean(cg_married == 1, na.rm = TRUE) * 100,
    pct_medicaid = mean(cg_medicaid_dum == 1, na.rm = TRUE) * 100,
    pct_financial_strain = mean(cg_finc_strain == 1, na.rm = TRUE) * 100,
    mean_ADL_hours = mean(cg_hours_adl, na.rm = TRUE),
    mean_IADL_hours = mean(cg_hours_iadl, na.rm = TRUE)
  ) %>%
  mutate(
    across(where(is.numeric) & !n, ~round(., 1))
  )

format_table(
  caregiver_by_type, 
  caption = "Caregiver Characteristics by Living Arrangement Type",
  col_names = c("Living Arrangement Type", "N", "Mean Age", "% Female", "% Married", 
                "% on Medicaid", "% with Financial Strain", "Mean ADL Hours", "Mean IADL Hours")
)




# 7. Analyze specific types of financial difficulty by living arrangement
financial_difficulties <- data %>%
  filter(!is.na(move_in_typology)) %>%
  group_by(move_in_typology) %>%
  summarize(
    n = n(),
    # Types of financial difficulty
    diff_kids = mean(help_kids_diff == 1, na.rm = TRUE) * 100,
    diff_housing = mean(housing_diff == 1, na.rm = TRUE) * 100,
    diff_travel = mean(travel_diff == 1, na.rm = TRUE) * 100,
    diff_essentials = mean(other_essential_diff == 1, na.rm = TRUE) * 100,
    any_diff = mean(total_diff > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric) & !n, ~round(., 1))
  )

format_table(
  financial_difficulties, 
  caption = "Types of Financial Difficulty by Living Arrangement",
  col_names = c("Living Arrangement Type", "N", "% Difficulty Helping Kids", 
                "% Housing Difficulties", "% Travel/Vacation Difficulties",
                "% Essential Expenses Difficulties", "% Any Financial Difficulty")
)




# 8. Living arrangements specifically for adult child caregivers
adult_child_arrangements <- data %>%
  filter(RELSUBJ7I %in% c(3, 4)) %>%  # 3=Son/Daughter, 4=Son-In-Law/Daughter-In-Law
  group_by(move_in_typology) %>%
  summarize(
    n = n(),
    pct_of_children = round(100 * n() / sum(!is.na(move_in_typology)), 1),
    financial_strain_pct = round(100 * mean(cg_finc_strain == 1, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

format_table(
  adult_child_arrangements,
  caption = "Living Arrangements of Adult Child Caregivers",
  col_names = c("Living Arrangement Type", "Count", "% of Adult Children", "% with Financial Strain")
)




# 9. Compare the two key configurations: adult child moved in vs. parent moved in
key_arrangements <- data %>%
  filter(
    RELSUBJ7I %in% c(3, 4), # Adult children only
    move_in_typology %in% c("Care recipient/parent likely moved in with caregiver", "Caregiver likely moved in with care recipient/parent")
  ) %>%
  group_by(move_in_typology) %>%
  summarize(
    n = n(),
    mean_age = mean(AGE7I, na.rm = TRUE),
    pct_female = mean(cg_female == 1, na.rm = TRUE) * 100,
    pct_married = mean(cg_married == 1, na.rm = TRUE) * 100,
    financial_strain_pct = mean(cg_finc_strain == 1, na.rm = TRUE) * 100,
    mean_ADL_hours = mean(cg_hours_adl, na.rm = TRUE),
    mean_IADL_hours = mean(cg_hours_iadl, na.rm = TRUE),
    cr_dementia = mean(cr_dementia_binary == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric) & !n, ~round(., 1))
  )

format_table(
  key_arrangements,
  caption = "Comparison: Parent Moved In vs. Adult Child Moved In",
  col_names = c("Living Arrangement", "N", "Mean Age", "% Female", "% Married", 
                "% with Financial Strain", "Mean ADL Hours", "Mean IADL Hours", "% CR with Dementia")
)


