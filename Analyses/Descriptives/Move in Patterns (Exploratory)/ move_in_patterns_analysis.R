ACC %>%
filter(RELSUBJ7I %in% c(3, 4)) %>%  # 3=Son/Daughter, 4=Son-In-Law/Daughter-In-Law
  group_by(move_in_type_label) %>%
  summarize(
    n = n(),
    pct_of_children = round(100 * n() / sum(!is.na(move_in_type_label)), 1),
    pct_strain = round(100 * mean(cg_finc_strain == 1, na.rm = TRUE), 1),
    pct_female = round(100 * mean(cg_female == 1, na.rm = TRUE), 1),
    mean_age = round(mean(AGE7I, na.rm = TRUE), 1),
    pct_married = round(100 * mean(cg_married == 1, na.rm = TRUE), 1),
    mean_ADL_hours = round(mean(cg_hours_adl, na.rm = TRUE), 1),
    mean_IADL_hours = round(mean(cg_hours_iadl, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_strain))

format_table(
  adult_child_basic,
  caption = "Move-In Types for Adult Child Caregivers Only",
  col_names = c("Move-In Type", "Count", "% of Adult Children", "% with Financial Strain", 
                "% Female", "Mean Age", "% Married", "Mean ADL Hours", "Mean IADL Hours")
)
```

## 3.3. Detailed Living Arrangements of Adult Child Caregivers

```{r adult-child-enriched}
# Adult Child Analysis - Enriched Typology
adult_child_enriched <- data %>%
  filter(RELSUBJ7I %in% c(3, 4)) %>%  # 3=Son/Daughter, 4=Son-In-Law/Daughter-In-Law
  group_by(enriched_typology) %>%
  summarize(
    n = n(),
    pct_of_children = round(100 * n() / sum(!is.na(enriched_typology)), 1),
    pct_strain = round(100 * mean(cg_finc_strain == 1, na.rm = TRUE), 1),
    pct_female = round(100 * mean(cg_female == 1, na.rm = TRUE), 1),
    mean_age = round(mean(AGE7I, na.rm = TRUE), 1),
    pct_married = round(100 * mean(cg_married == 1, na.rm = TRUE), 1),
    mean_ADL_hours = round(mean(cg_hours_adl, na.rm = TRUE), 1),
    mean_IADL_hours = round(mean(cg_hours_iadl, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_strain))

format_table(
  adult_child_enriched,
  caption = "Detailed Living Arrangements of Adult Child Caregivers",
  col_names = c("Living Arrangement", "Count", "% of Adult Children", "% with Financial Strain", 
                "% Female", "Mean Age", "% Married", "Mean ADL Hours", "Mean IADL Hours")
)
```

## 3.4. Financial Strain by Move-In Type

```{r strain-plot, fig.width=8, fig.height=5}
# Visualize financial strain by move-in type for adult children only
ggplot(adult_child_basic, aes(x = reorder(move_in_type_label, pct_strain), y = pct_strain)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(pct_strain, "%")), vjust = -0.5) +
  labs(title = "Financial Strain by Move-In Type",
       subtitle = "Adult Child Caregivers Only",
       x = NULL,
       y = "Percentage with Financial Strain") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## 3.5. Types of Financial Difficulty by Move-In Type

```{r financial-difficulty-types}
# Types of financial difficulties by move-in type for adult children
adult_child_difficulties <- data %>%
  filter(RELSUBJ7I %in% c(3, 4)) %>%  # Adult children only
  group_by(move_in_type_label) %>%
  summarize(
    n = n(),
    # Types of financial difficulty
    diff_kids = round(100 * mean(help_kids_diff == 1, na.rm = TRUE), 1),
    diff_housing = round(100 * mean(housing_diff == 1, na.rm = TRUE), 1),
    diff_travel = round(100 * mean(travel_diff == 1, na.rm = TRUE), 1),
    diff_essentials = round(100 * mean(other_essential_diff == 1, na.rm = TRUE), 1),
    any_diff = round(100 * mean(total_diff > 0, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(any_diff))

format_table(
  adult_child_difficulties,
  caption = "Types of Financial Difficulty by Move-In Type (Adult Children Only)",
  col_names = c("Move-In Type", "N", "% Difficulty Helping Kids", 
                "% Housing Difficulties", "% Travel/Vacation Difficulties",
                "% Essential Expenses Difficulties", "% Any Financial Difficulty")
)
```

## 3.6. Dementia Status by Move-In Type

```{r dementia-status}
# Dementia status by move-in type for adult children
adult_child_dementia <- data %>%
  filter(RELSUBJ7I %in% c(3, 4)) %>%  # Adult children only
  group_by(move_in_type_label) %>%
  summarize(
    n = n(),
    n_dementia = sum(cr_dementia == 1, na.rm = TRUE),
    n_no_dementia = sum(cr_dementia == 0, na.rm = TRUE),
    n_dementia_valid = n_dementia + n_no_dementia,
    pct_dementia = ifelse(n_dementia_valid > 0, 
                          round(100 * n_dementia / n_dementia_valid, 1), 
                          NA),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_dementia))

format_table(
  adult_child_dementia,
  caption = "Dementia Status by Move-In Type (Adult Children Only)",
  col_names = c("Move-In Type", "N", "# with Dementia", "# without Dementia", 
                "Valid Cases", "% with Dementia")
)
```

# Key Findings and Conclusions

Based on the analysis presented in this document, several key patterns emerge regarding move-in scenarios among Latino caregivers and care recipients:
  
  1. **Who Moves In**: The analysis shows that adult children (including in-laws) constitute the largest group of individuals who move in with elderly care recipients, followed by spouses and grandchildren.

2. **Reasons for Moving In**: We identified multiple motivations for move-ins:
  - Providing care for the elderly parent
- The adult child needing housing
- The parent moving to be closer to their children

3. **Financial Strain Patterns**: Financial strain appears to vary significantly based on the move-in arrangement:
  - Adult children who move in for housing reasons report higher rates of financial strain
- Caregivers living with parents who own their homes report different strain levels than those where the caregiver is the homeowner

4. **Household Structure Impact**: The combination of co-residence, household headship, and home ownership provides insights into the economic dynamics of these intergenerational households.

5. **Dementia and Care Needs**: Care recipient dementia status appears to be associated with certain living arrangements, suggesting dementia may be a driver of household restructuring.

These findings have important implications for understanding the financial burden faced by Latino caregivers and for designing supportive interventions and policies that address their specific needs.

```{r final-figure, fig.width=8, fig.height=4.5}
# Create a summary visualization comparing co-residence and financial strain
summary_data <- data %>%
  filter(RELSUBJ7I %in% c(3, 4)) %>%  # Adult children only
  group_by(move_in_type = move_in_type_label) %>%
  summarize(
    count = n(),
    pct_coresident = 100 * mean(coreside == 1, na.rm = TRUE),
    pct_strain = 100 * mean(cg_finc_strain == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(count >= 10) %>%  # Only include categories with at least 10 observations
  arrange(desc(pct_strain))

# Reshape for grouped bar chart
summary_long <- summary_data %>%
  select(move_in_type, pct_coresident, pct_strain) %>%
  pivot_longer(
    cols = c(pct_coresident, pct_strain),
    names_to = "measure",
    values_to = "percentage"
  ) %>%
  mutate(
    measure = factor(measure, 
                     levels = c("pct_coresident", "pct_strain"),
                     labels = c("% Co-resident", "% with Financial Strain"))
  )

# Create the visualization
ggplot(summary_long, aes(x = reorder(move_in_type, -percentage), y = percentage, fill = measure)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Co-residence and Financial Strain by Move-In Type",
       subtitle = "Adult Child Caregivers Only",
       x = NULL,
       y = "Percentage",
       fill = "Measure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  scale_fill_brewer(palette = "Set1")
```