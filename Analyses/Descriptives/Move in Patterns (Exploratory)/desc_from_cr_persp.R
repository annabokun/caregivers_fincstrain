# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)

# Assuming 'data' is your HEPESE dataset
# If you need to import it first:
# data <- read.csv("ACC.csv", na.strings = c("", "NA", "NULL"))

#################################################################
# PART 1: CARE RECIPIENT PERSPECTIVE (Using recipient variables)
#################################################################

# Create a function for formatting tables
format_table <- function(df, caption, col_names = NULL) {
  if(is.null(col_names)) {
    col_names <- names(df)
  }
  
  kable(df, caption = caption, col.names = col_names, align = c("l", rep("c", ncol(df)-1))) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    row_spec(0, bold = TRUE)
}

# 1. Summary of people moving in with care recipients
recipient_moves <- ACC %>%
  summarize(
    total_recipients = n(),
    anyone_moved_in = sum(MOVEIN7 == 1, na.rm = TRUE),
    moved_in_for_care = sum(TAKECARE7 == 1, na.rm = TRUE),
    moved_in_need_place = sum(PLASTAY7 == 2, na.rm = TRUE)
  ) %>%
  mutate(
    pct_anyone_moved = round(100 * anyone_moved_in / total_recipients, 1),
    pct_moved_for_care = round(100 * moved_in_for_care / total_recipients, 1),
    pct_moved_need_place = round(100 * moved_in_need_place / total_recipients, 1)
  )

# Create an easier-to-read table
recipient_moves_table <- data.frame(
  Scenario = c(
    "Someone moved in with parent",
    "Person moved in to provide care for parent",
    "Person moved in because they needed a place to stay"
  ),
  Count = c(
    recipient_moves$anyone_moved_in,
    recipient_moves$moved_in_for_care,
    recipient_moves$moved_in_need_place
  ),
  Percentage = paste0(
    c(
      recipient_moves$pct_anyone_moved,
      recipient_moves$pct_moved_for_care,
      recipient_moves$pct_moved_need_place
    ),
    "%"
  )
)

format_table(
  recipient_moves_table,
  caption = paste0("Move-in Scenarios from the Care Recipient/Parent Perspective (N=", 
                   recipient_moves$total_recipients, ")"),
  col_names = c("Scenario", "Count", "% of All Care Recipients/Parents")
)






# 2. Who moved in with care recipients
# Define relationship lookup
rel_codes <- c(
  "1" = "Head of Household",
  "2" = "Spouse",
  "3" = "Son/Daughter",
  "4" = "Children-In-Law",
  "5" = "Grandchild",
  "6" = "Parent",
  "7" = "Brother or Sister",
  "8" = "Nephew or Niece",
  "9" = "Cousin",
  "10" = "Aunt/Uncle",
  "11" = "Great Grandchild",
  "12" = "Other Relative",
  "13" = "Friend",
  "14" = "Boarder or Roomer",
  "15" = "Paid Employee",
  "16" = "Other",
  "17" = "Sister/Brother In-Law"
)

# Function to safely convert factors to numeric
safe_as_numeric <- function(x) {
  as.numeric(as.character(x))
}

# Analyze who moved in
who_moved_in <- ACC %>%
  filter(MOVEIN7 == 1) %>%
  select(WHOMOV71, WHOMOV72, WHOMOV73, WHOMOV74) %>%
  pivot_longer(
    cols = everything(),
    names_to = "person_num",
    values_to = "rel_code",
    values_drop_na = TRUE
  ) %>%
  # Convert to numeric if not already
  mutate(rel_code = safe_as_numeric(rel_code)) %>%
  # Map codes to labels
  mutate(relationship = factor(rel_code, 
                               levels = as.numeric(names(rel_codes)),
                               labels = rel_codes)) %>%
  # Group relationships for clearer presentation
  mutate(
    rel_group = case_when(
      relationship %in% c("Son/Daughter", "Children-In-Law") ~ "Adult Children (inc. in-laws)",
      relationship == "Spouse" ~ "Spouse",
      relationship %in% c("Grandchild", "Great Grandchild") ~ "Grandchildren",
      relationship %in% c("Brother or Sister", "Sister/Brother In-Law", 
                          "Nephew or Niece", "Cousin", "Aunt/Uncle", "Other Relative") ~ "Other relatives",
      TRUE ~ "Non-relatives"
    )
  ) %>%
  group_by(rel_group) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(percentage = round(100 * count / sum(count), 1)) %>%
  arrange(desc(count))

format_table(
  who_moved_in,
  caption = paste0("Who Moved In with Care Recipients (Among ", sum(who_moved_in$count), " Move-ins)"),
  col_names = c("Relationship to Care Recipient", "Count", "Percentage")
)





# Analyze who moved in - WHOMOV71 ONLY (primary person)
primary_mover <- ACC %>%
  filter(MOVEIN7 == 1, !is.na(WHOMOV71)) %>%
  # Convert to numeric if not already
  mutate(rel_code = safe_as_numeric(WHOMOV71)) %>%
  # Map codes to labels
  mutate(relationship = factor(rel_code, 
                               levels = as.numeric(names(rel_codes)),
                               labels = rel_codes)) %>%
  # Group relationships for clearer presentation
  mutate(
    rel_group = case_when(
      relationship %in% c("Son/Daughter", "Son-In-Law/Daughter-In-Law") ~ "Adult Children (inc. in-laws)",
      relationship == "Spouse" ~ "Spouse",
      relationship %in% c("Grandchild", "Great Grandchild") ~ "Grandchildren",
      relationship %in% c("Brother or Sister", "Sister/Brother In-Law", 
                          "Nephew or Niece", "Cousin", "Aunt/Uncle", "Other Relative") ~ "Other relatives",
      TRUE ~ "Non-relatives"
    )
  ) %>%
  group_by(rel_group) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(percentage = round(100 * count / sum(count), 1)) %>%
  arrange(desc(count))



# Create a more detailed table with all individual relationship types
detailed_primary_mover <- ACC %>%
  filter(MOVEIN7 == 1, !is.na(WHOMOV71)) %>%
  # Convert to numeric if not already
  mutate(rel_code = safe_as_numeric(WHOMOV71)) %>%
  # Map codes to labels
  mutate(relationship = factor(rel_code, 
                               levels = as.numeric(names(rel_codes)),
                               labels = rel_codes)) %>%
  group_by(relationship) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(percentage = round(100 * count / sum(count), 1)) %>%
  arrange(desc(count))



# Display the primary mover table (grouped categories)
format_table(
  primary_mover,
  caption = paste0("Primary Person Who Moved In With Care Recipients (N=", sum(primary_mover$count), ")"),
  col_names = c("Relationship to Care Recipient", "Count", "Percentage")
)



# Display the detailed relationship table
format_table(
  detailed_primary_mover,
  caption = paste0("Detailed Relationship of Primary Person Who Moved In (N=", sum(detailed_primary_mover$count), ")"),
  col_names = c("Specific Relationship to Elder", "Count", "Percentage")
)



# Now analyze the relationship between who moved in and why they moved in
primary_mover_reasons <- ACC %>%
  filter(MOVEIN7 == 1, !is.na(WHOMOV71)) %>%
  # Convert to numeric if not already
  mutate(rel_code = safe_as_numeric(WHOMOV71)) %>%
  # Map codes to labels
  mutate(relationship = factor(rel_code, 
                               levels = as.numeric(names(rel_codes)),
                               labels = rel_codes)) %>%
  # Group relationships for clearer presentation
  mutate(
    rel_group = case_when(
      relationship %in% c("Son/Daughter", "Son-In-Law/Daughter-In-Law") ~ "Adult Children (inc. in-laws)",
      relationship == "Spouse" ~ "Spouse",
      relationship %in% c("Grandchild", "Great Grandchild") ~ "Grandchildren",
      relationship %in% c("Brother or Sister", "Sister/Brother In-Law", 
                          "Nephew or Niece", "Cousin", "Aunt/Uncle", "Other Relative") ~ "Other relatives",
      TRUE ~ "Non-relatives"
    )
  ) %>%
  # Group by relationship and count reason types
  group_by(rel_group) %>%
  summarize(
    total = n(),
    for_care = sum(TAKECARE7 == 1, na.rm = TRUE),
    for_care_pct = round(100 * sum(TAKECARE7 == 1, na.rm = TRUE) / n(), 1),
    needed_place = sum(PLASTAY7 == 2, na.rm = TRUE), 
    needed_place_pct = round(100 * sum(PLASTAY7 == 2, na.rm = TRUE) / n(), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(total))

# Display reasons by relationship
format_table(
  primary_mover_reasons,
  caption = "Reasons for Moving In by Relationship Type (Primary Mover Only)",
  col_names = c("Relationship to Care Recipient", "Total Count", "Moved for Care", 
                "% Moved for Care", "Needed Place", "% Needed Place")
)





# Analyze who moves in with whom when the informant is the primary mover
informant_relationships <- ACC %>%
  # Filter to cases where informant is related as a child/child-in-law and moved in
  filter(MOVEIN7 == 1, !is.na(WHOMOV71), 
         RELSUBJ7I %in% c(3, 4), # 3=Son/Daughter, 4=Son-In-Law/Daughter-In-Law
         WHOMOV71 %in% c(3, 4)) %>% # Primary mover is a child/child-in-law
  summarize(
    total_child_moveins = n(),
    cg_is_hoh = sum(cg_HOH == 1, na.rm = TRUE),
    cg_is_hoh_pct = round(100 * sum(cg_HOH == 1, na.rm = TRUE) / n(), 1),
    elder_owns_home = sum(TT17I == 1, na.rm = TRUE),
    elder_owns_home_pct = round(100 * sum(TT17I == 1, na.rm = TRUE) / n(), 1),
    moved_for_care = sum(TAKECARE7 == 1, na.rm = TRUE),
    moved_for_care_pct = round(100 * sum(TAKECARE7 == 1, na.rm = TRUE) / n(), 1),
    needed_place = sum(PLASTAY7 == 2, na.rm = TRUE), 
    needed_place_pct = round(100 * sum(PLASTAY7 == 2, na.rm = TRUE) / n(), 1),
    financial_strain = sum(cg_finc_strain == 1, na.rm = TRUE),
    financial_strain_pct = round(100 * sum(cg_finc_strain == 1, na.rm = TRUE) / n(), 1)
  )

# Create a formatted table for adult child movers
child_mover_table <- data.frame(
  Characteristic = c(
    "Total adult children who moved in",
    "Caregiver is head of household",
    "Care recipient/parent owns the home",
    "Moved in to provide care",
    "Moved in because needed place",
    "Reports financial strain"
  ),
  Count = c(
    informant_relationships$total_child_moveins,
    informant_relationships$cg_is_hoh,
    informant_relationships$elder_owns_home,
    informant_relationships$moved_for_care,
    informant_relationships$needed_place,
    informant_relationships$financial_strain
  ),
  Percentage = paste0(
    c(
      NA, # No percentage for total
      informant_relationships$cg_is_hoh_pct,
      informant_relationships$elder_owns_home_pct,
      informant_relationships$moved_for_care_pct,
      informant_relationships$needed_place_pct,
      informant_relationships$financial_strain_pct
    ),
    ifelse(is.na(
      c(
        NA,
        informant_relationships$cg_is_hoh_pct,
        informant_relationships$elder_owns_home_pct,
        informant_relationships$moved_for_care_pct,
        informant_relationships$needed_place_pct,
        informant_relationships$financial_strain_pct
      )), "", "%")
  )
)

# Display adult child mover table
format_table(
  child_mover_table,
  caption = "Characteristics of Adult Children Who Moved In With Parents",
  col_names = c("Characteristic", "Count", "Percentage")
)






# 3. Elders who moved
elder_moved <- ACC %>%
  summarize(
    total = n(),
    elder_moved = sum(MOVED7 == 1, na.rm = TRUE),
    moved_near_children = sum(WHYMOVE7 == 2, na.rm = TRUE)
  ) %>%
  mutate(
    pct_elder_moved = round(100 * elder_moved / total, 1),
    pct_moved_near_children = round(100 * moved_near_children / total, 1)
  )

elder_moved_table <- data.frame(
  Scenario = c(
    "Care Recipient/Parent moved since last contact",
    "Care Recipient/Parent moved to be near children"
  ),
  Count = c(
    elder_moved$elder_moved,
    elder_moved$moved_near_children
  ),
  Percentage = paste0(
    c(
      elder_moved$pct_elder_moved,
      elder_moved$pct_moved_near_children
    ),
    "%"
  )
)

format_table(
  elder_moved_table,
  caption = paste0("Care Recipient/Parent Mobility Patterns (N=", elder_moved$total, ")"),
  col_names = c("Scenario", "Count", "% of All Care Recipients")
)




