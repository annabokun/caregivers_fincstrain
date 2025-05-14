# Load necessary libraries
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)

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
# UPDATED MOVE-IN TYPOLOGY: ADULT CHILD CAREGIVERS ONLY
#################################################################

# 1. Create the move-in typology variable based on the refined categories
data <- ACC %>%
  mutate(
    # Create the move-in typology variable
    move_in_type = case_when(
      # Category 1: Parent likely moved in with adult child for care
      MOVED7 == 1 &                                # CR moved since last contact
        (WHYMOVE7 == 2 | TAKECARE7 == 1) ~ 1,      # Moved to be near children OR needed help
      
      # Category 2: Adult child moved in for caregiving reasons
      MOVEIN7 == 1 &                               # Anyone moved in with you since last contact?
        (WHOMOV71 == 3 | WHOMOV71 == 4) &          # Children or children-in-law moved in
        TAKECARE7 == 1 ~ 2,                        # Person moved in because I needed help
      
      # Category 3: Adult child moved in for housing
      MOVEIN7 == 1 &                               # Anyone moved in with you since last contact?
        (WHOMOV71 == 3 | WHOMOV71 == 4) &          # Children or children-in-law moved in
        PLASTAY7 == 2 ~ 3,                         # 2 = "Needed place to stay"
      
      # Category 4: Adult child moved in for other reasons
      MOVEIN7 == 1 &                               # Anyone moved in with you since last contact?
        (WHOMOV71 == 3 | WHOMOV71 == 4) &          # Children or children-in-law moved in
        YOUOTH7 == 3 ~ 4,                          # 3 = "other" reasons
      
      # Category 5: General move-in
      MOVEIN7 == 1 | !is.na(WHOMOV71) ~ 5,
      
      # Category 6: No movement (default)
      TRUE ~ 6
    ),
    
    # Create a labeled version for better readability
    move_in_type_label = case_when(
      move_in_type == 1 ~ "Parent moved in with adult child for care",
      move_in_type == 2 ~ "Adult child moved in for caregiving reasons",
      move_in_type == 3 ~ "Adult child moved in for housing",
      move_in_type == 4 ~ "Adult child moved in for other reasons",
      move_in_type == 5 ~ "General move-in (unspecified reason)",
      move_in_type == 6 ~ "No movement"
    ),
    
    # Create enriched typology that combines move-in type with household characteristics
    enriched_typology = case_when(
      # Parent moved in for care + caregiver is head of household
      move_in_type == 1 & cg_HOH == 1 ~ "Parent moved in for care (caregiver is HOH)",
      
      # Parent moved in for care + caregiver is not head of household
      move_in_type == 1 & cg_HOH == 0 ~ "Parent moved in for care (caregiver not HOH)",
      
      # Adult child moved in for caregiving + parent owns home
      move_in_type == 2 & LL76 == 1 ~ "Adult child moved in for caregiving (parent owns home)",
      
      # Adult child moved in for caregiving + parent doesn't own home
      move_in_type == 2 & LL76 == 2 ~ "Adult child moved in for caregiving (parent doesn't own home)",
      
      # Adult child moved in for housing + parent owns home
      move_in_type == 3 & LL76 == 1 ~ "Adult child moved in for housing (parent owns home)",
      
      # Adult child moved in for housing + parent doesn't own home
      move_in_type == 3 & LL76 == 2 ~ "Adult child moved in for housing (parent doesn't own home)",
      
      # Adult child moved in for other reasons + parent owns home
      move_in_type == 4 & LL76 == 1 ~ "Adult child moved in for other reasons (parent owns home)",
      
      # Adult child moved in for other reasons + parent doesn't own home
      move_in_type == 4 & LL76 == 2 ~ "Adult child moved in for other reasons (parent doesn't own home)",
      
      # Any co-residence not captured by other categories
      move_in_type == 5 & coreside == 1 ~ "Other co-residence with movement",
      
      # Co-residence without recent movement
      move_in_type == 6 & coreside == 1 ~ "Co-residence (no recent movement)",
      
      # No co-residence
      TRUE ~ "Not co-residing"
    )
  )

# 2. Analyze the distribution of the updated move-in typology
typology_distribution <- data %>%
  group_by(move_in_type, move_in_type_label) %>%
  summarize(
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = round(100 * count / sum(count), 1)
  ) %>%
  arrange(move_in_type)

format_table(
  typology_distribution,
  caption = "Move-In Typology",
  col_names = c("Type Code", "Description", "Count", "Percentage")
)



# 3. Distribution of the enriched typology
enriched_distribution <- data %>%
  group_by(enriched_typology) %>%
  summarize(
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = round(100 * count / sum(count), 1)
  ) %>%
  arrange(desc(count))

format_table(
  enriched_distribution,
  caption = "Distribution of Enriched Move-In Typology",
  col_names = c("Arrangement Description", "Count", "Percentage")
)




# 4. Adult Child Only Analysis - Basic Move-In Types
adult_child_basic <- data %>%
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
  caption = "Updated Move-In Types for Adult Child Caregivers Only",
  col_names = c("Move-In Type", "Count", "% of Adult Children", "% with Financial Strain", 
                "% Female", "Mean Age", "% Married", "Mean ADL Hours", "Mean IADL Hours")
)





# 5. Adult Child Analysis - Enriched Typology
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



# 6. Visualize financial strain by move-in type for adult children only
ggplot(adult_child_basic, aes(x = reorder(move_in_type_label, pct_strain), y = pct_strain)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(pct_strain, "%")), vjust = -0.5) +
  labs(title = "Financial Strain by Updated Move-In Type",
       subtitle = "Adult Child Caregivers Only",
       x = NULL,
       y = "Percentage with Financial Strain") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# 7. Types of financial difficulties by move-in type for adult children
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
  caption = "Types of Financial Difficulty by Updated Move-In Type (Adult Children Only)",
  col_names = c("Move-In Type", "N", "% Difficulty Helping Kids", 
                "% Housing Difficulties", "% Travel/Vacation Difficulties",
                "% Essential Expenses Difficulties", "% Any Financial Difficulty")
)




# 8. Dementia status by move-in type for adult children
adult_child_dementia <- data %>%
  filter(RELSUBJ7I %in% c(3, 4)) %>%  # Adult children only
  group_by(move_in_type_label) %>%
  summarize(
    n = n(),
    n_dementia = sum(cr_dementia == 1, na.rm = TRUE),
    n_no_dementia = sum(cr_dementia_binary == 0, na.rm = TRUE),
    n_dementia_valid = n_dementia + n_no_dementia,
    pct_dementia = ifelse(n_dementia_valid > 0, 
                          round(100 * n_dementia / n_dementia_valid, 1), 
                          NA),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_dementia))

format_table(
  adult_child_dementia,
  caption = "Dementia Status by Move-In Type",
  col_names = c("Move-In Type", "N", "# with Dementia", "# without Dementia", 
                "Valid Cases", "% with Dementia")
)



# 9. Add analysis of financial strain by move-in type and household headship
strain_by_typology_headship <- data %>%
  filter(RELSUBJ7I %in% c(3, 4)) %>%  # Adult children only
  group_by(move_in_type_label, cg_HOH) %>%
  summarize(
    n = n(),
    n_strain = sum(cg_finc_strain == 1, na.rm = TRUE),
    pct_strain = round(100 * mean(cg_finc_strain == 1, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%  # Only include groups with at least 5 observations
  mutate(
    hoh_status = ifelse(cg_HOH == 1, "Caregiver is HOH", "Caregiver is not HOH"),
    typology_hoh = paste(move_in_type_label, "-", hoh_status)
  ) %>%
  arrange(desc(pct_strain))

format_table(
  strain_by_typology_headship,
  caption = "Financial Strain by Move-In Type and Household Headship (Adult Children Only)",
  col_names = c("Move-In Type", "HOH Status", "N", "# with Strain", "% with Financial Strain", 
                "HOH Status", "Combined Category")
)



# 10. Household characteristics by move-in type
household_by_typology <- data %>%
  filter(RELSUBJ7I %in% c(3, 4)) %>%  # Adult children only
  group_by(move_in_type_label) %>%
  summarize(
    n = n(),
    pct_cg_hoh = round(100 * mean(cg_HOH == 1, na.rm = TRUE), 1),
    pct_elder_owns = round(100 * mean(LL76 == 1, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(move_in_type_label)

format_table(
  household_by_typology,
  caption = "Household Structure by Move-In Type (Adult Children Only)",
  col_names = c("Move-In Type", "N", "% CG is HOH", "% Parent Owns Home")
)