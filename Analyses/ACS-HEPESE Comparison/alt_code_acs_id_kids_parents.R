  
  # Create subset of households with elderly parents and adult children
    target_households2 <- mexican_hhs %>%
      group_by(SERIAL) %>%
      filter(GQ %in% 1:2) %>% # Exclude group quarters 
      mutate(
        mother_age = case_when(     # Get parent ages for each person in a vectorized way
          MOMLOC > 0 ~ AGE[match(MOMLOC, PERNUM)],
          TRUE ~ NA_real_),
        father_age = case_when(
          POPLOC > 0 ~ AGE[match(POPLOC, PERNUM)],
          TRUE ~ NA_real_)) %>%
      filter(any(AGE >= 18 & (mother_age >= 80 | father_age >= 80))) %>% # Keep households where at least one person is adult with elderly parent
      ungroup()
  
          # Unweighted n 
          # n = 9,306 (unweighted) 
          
          # Weighted n 
          target_households2_n <- sum(target_households2$PERWT) # 977,216


# Check some example households
target_households2 %>%
  select(SERIAL, PERNUM, AGE, MOMLOC, POPLOC, mother_age, father_age) %>%
  arrange(SERIAL, PERNUM) %>%
  head(10)










#====================#
#=== ACS CLEANING ===#
#====================#

#### Collapse EDUC into HS/No HS categories ---------------------------
target_households2 <- target_households2 %>%
  mutate(
    hs_binary = case_when(
      EDUC %in% 0:5 ~ "Less than High School",
      EDUC %in% 6:11 ~ "High School or More",
      TRUE ~ NA_character_)) # Handle missing values

# Weighted proportions for HS/No HS
target_households2_svy <- svydesign(ids = ~1, weights = ~PERWT, data = target_households2)
hs_distribution <- svymean(~factor(hs_binary), target_households2_svy, na.rm = TRUE)
print(hs_distribution)





#### Collapse HH income ($20k binary) ---------------------------   
summary(target_households2$HHINCOME)

# Create 3-category income variable
target_households2 <- target_households2 %>%
  mutate(
    inc_3cat = case_when(
      HHINCOME < 20000 ~ "< $20,000",
      HHINCOME >= 20000 ~ "$20,000+",
      is.na(HHINCOME) ~ "Missing",
      TRUE ~ NA_character_))

# Weighted proportions for HS/No HS
target_households2_svy <- svydesign(ids = ~1, weights = ~PERWT, data = target_households2)
income_cat_pct <- svymean(~factor(inc_3cat), target_households2_svy, na.rm = TRUE)
print(income_cat_pct)





#### Collapse married ---------------------------   
target_households2 <- target_households2 %>%
  mutate(
    married_dummy = case_when(
      MARST %in% c(1, 2) ~ 1,  # Married (spouse present or absent)
      MARST %in% c(3, 4, 5, 6) ~ 0,  # Not married (separated, divorced, widowed, never married)
      TRUE ~ NA_real_))  # Handle missing values

# Weighted proportions for HS/No HS
target_households2_svy <- svydesign(ids = ~1, weights = ~PERWT, data = target_households2)
married_pct <- svymean(~factor(married_dummy), target_households2_svy, na.rm = TRUE)
print(married_pct)

# Or, can calculate weighted percentages this way too
married_pct <- svymean(~married_dummy, target_households2_svy, na.rm = TRUE) * 100    




#### Visually inspect sample ---------------------------  
samp_acs2 <- target_households2 %>%
  select(YEAR, SERIAL, PERNUM, STATEFIP, SEX, AGE, RELATE, MOMLOC, POPLOC, NCHILD, 
         MARST, married_dummy, RACE, HISPAN, EDUC, hs_binary, HHINCOME, inc_3cat, NUMPREC, FAMSIZE, GQ, PERWT)
