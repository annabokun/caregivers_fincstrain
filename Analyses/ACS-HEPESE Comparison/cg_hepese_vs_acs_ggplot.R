#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

# "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
# PAA 2025

# Figure comparing significant associations of HEPESE vs ACS caregiver demographics 

# Script created in RStudio ("Cranberry Hibiscus")
# updated 2/26/2025
#=======================================================================================================#

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Create the dataset (from Table in the paper) - now with four groups
df <- data.frame(
  Characteristic = c("Age (mean)", "Female", "Less than high school", "Married",
                     "Household income <$20k", "Spanish Language"),
  HEPESE_MexicanAmerican = c(55.78, 64.4, 38.2, 35.3, 48.3, 65.4),
  ACS_MexicanAmerican = c(51.83, 49.6, 29.9, 29.7, 12.7, 81.6),
  ACS_NonHispanicWhite = c(55.64, 49.4, 5.9, 27.3, 7.1, 1.6),
  ACS_MexAm_UnpaidFamilyWorker = c(50.34, 50.2, 29.6, 26.0, 24.1, 90.4)
)

# Convert to long format for ggplot
df_long <- df %>%
  pivot_longer(cols = -Characteristic, names_to = "Group", values_to = "Value") %>%
  # Reorder factor levels for the groups to control legend order
  mutate(Group = factor(Group, levels = c("HEPESE_MexicanAmerican", 
                                          "ACS_MexAm_UnpaidFamilyWorker", 
                                          "ACS_MexicanAmerican", 
                                          "ACS_NonHispanicWhite")))

# Format labels appropriately based on variable type
df_long <- df_long %>%
  mutate(Is_Percentage = ifelse(Characteristic == "Age (mean)", FALSE, TRUE),
         Value_Display = ifelse(Is_Percentage, 
                                paste0(round(Value, 1), "%"), 
                                as.character(round(Value, 1))))

# Define a better color scheme with a fourth color
colors <- c("HEPESE_MexicanAmerican" = "#2166AC", 
            "ACS_MexicanAmerican" = "darkorange", 
            "ACS_NonHispanicWhite" = "#CCCCCC",
            "ACS_MexAm_UnpaidFamilyWorker" = "#66A61E")  # Added a green color for unpaid workers

# Create better labels for the legend
legend_labels <- c(
  "HEPESE Mexican American (n=191)",
  "ACS Mexican American (n=2,696)",
  "ACS Non-Hispanic White (n=6,038)",
  "ACS Mexican-American Unpaid Family Worker (n=1,401)"
)

# Create the plot with a single legend at the bottom
p <- ggplot(df_long, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_text(aes(label = Value_Display), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 5) +
  facet_wrap(~ Characteristic, scales = "fixed") +
  scale_fill_manual(values = colors,
                    labels = c("HEPESE Mexican-American (caregivers)", 
                               "ACS Mexican-American (unpaid family workers)",
                               "ACS Mexican-American", 
                               "ACS Non-Hispanic White")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Comparison of Co-residential Adult Children Demographics Across Samples",
       x = NULL,
       y = "Value (% or years)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),  # Made legend text smaller to fit all groups
        axis.text.x = element_blank(),  # Remove x-axis text completely
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        strip.background = element_rect(fill = "lightgray", color = NA),
        strip.text = element_text(face = "bold", size = 14))

print(p)

# Save 
ggsave(
  "caregiver_demographics_comparison.png",  # File name
  plot = p,                                # Your plot object
  width = 12,                              # Width in inches
  height = 7,                              # Height in inches
  dpi = 300,                               # Resolution (300 dpi is print quality)
  bg = "white"                             # White background
)