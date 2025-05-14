#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

# "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
# PAA 2025

# Figure comparing different move-in scenarios with caregiver financial strain

# Script created in RStudio ("Cranberry Hibiscus")
# updated 2/26/2025
#=======================================================================================================#

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# Create dataset based on Table 6 from your paper
financial_strain_data <- data.frame(
  living_arrangement = c(
    "Adult child moved in for caregiving reasons",
    "Parent moved in with adult child for care",
    "Adult child moved in for housing",
    "Adult child moved in (general)",
    "Co-residence (no data on specific reasons)",
    "Not co-residing"
  ),
  financial_strain_pct = c(22.2, 19.4, 14.3, 16.7, 15.2, 10.5),
  n = c(18, 31, 21, 24, 178, 387)
)

# Sort by financial strain percentage
financial_strain_data <- financial_strain_data %>%
  mutate(living_arrangement = fct_reorder(living_arrangement, financial_strain_pct))

# Create Cleveland dot plot
p <- ggplot(financial_strain_data, aes(x = financial_strain_pct, y = living_arrangement)) +
  # Add horizontal lines
  geom_segment(aes(x = 0, xend = financial_strain_pct, 
                   y = living_arrangement, yend = living_arrangement),
               color = "gray70") +
  # Add circles for values
  geom_point(aes(size = n), color = "darkgreen", alpha = 0.8) +
  # Add text labels
  geom_text(aes(label = paste0(financial_strain_pct, "%")), 
            hjust = -0.3, vjust = 0.3, size = 6) +
  # Add sample size as labels
  geom_text(aes(label = paste0("n=", n)), x = 1, hjust = -0.2, 
            vjust = -1.2, size = 3, color = "gray40") +
  # Scales and labels
  scale_x_continuous(limits = c(0, 30), 
                     breaks = seq(0, 30, by = 5),
                     labels = function(x) paste0(x, "%")) +
  scale_size_continuous(range = c(3, 10), guide = "none") +
  labs(title = "Financial Strain by Living Arrangement Among Adult Child Caregivers",
       subtitle = "Percentage reporting financial strain, by move-in scenario",
       x = "Percentage with Financial Strain",
       y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(color = "gray40", size = 15)
  )

print(p)

# Save 
ggsave(
  "finc_strain_living_arrang.png",  # File name
  plot = p,                                # Your plot object
  width = 14,                              # Width in inches
  height = 7,                              # Height in inches
  dpi = 300,                               # Resolution (300 dpi is print quality)
  bg = "white"                             # White background
)