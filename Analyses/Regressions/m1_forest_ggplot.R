library(ggplot2)
library(dplyr)

# Create a dataset with the exact values from your updated table
# Only including significant predictors (p < 0.1)
forest_data <- data.frame(
  term = c(
    "Co-residence (vs. no co-residence)",
    "Age group: 18-44 (vs. 65-75)",
    "Age group: 55-64 (vs. 65-75)",
    "Married (vs. not married)",
    "HH Income $50k+ (vs. $10-20k)"
  ),
  estimate = c(2.17, 6.82, 5.04, 1.85, 0.42),  # Updated ORs
  conf.low = c(1.22, 1.67, 1.44, 1.04, 0.15),  # Updated lower CI bounds
  conf.high = c(3.82, 46.3, 32.0, 3.39, 1.06), # Updated upper CI bounds
  p.value = c(0.007, 0.017, 0.032, 0.040, 0.079), # Updated p-values
  significance = c("p < 0.01", "p < 0.05", "p < 0.05", "p < 0.05", "p < 0.1"),
  label_text = c(
    "2.17",
    "6.82",
    "5.04",
    "1.85",
    "0.42"
  )
)

# Reverse the order of the terms
forest_data$term <- factor(forest_data$term, 
                           levels = rev(c(
                             "Co-residence (vs. no co-residence)",
                             "Age group: 18-44 (vs. 65-75)",
                             "Age group: 55-64 (vs. 65-75)",
                             "Married (vs. not married)",
                             "HH Income $50k+ (vs. $10-20k)"
                           )))

# Create the forest plot
m1_forest <- ggplot(forest_data, aes(y = term)) +
  # Reference line
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 1) +
  # Points and confidence intervals with thicker lines
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, x = estimate, color = significance),
    height = 0.3, linewidth = 1.5
  ) +
  geom_point(
    aes(x = estimate, color = significance),
    size = 5
  ) +
  # OR labels at end of CI bars
  geom_text(
    aes(
      x = ifelse(estimate > 1, conf.high * 1.1, conf.low * 0.9),
      label = label_text,
      hjust = ifelse(estimate > 1, 0, 1)
    ),
    size = 6
  ) +
  # Colors for significance levels
  scale_color_manual(values = c(
    "p < 0.01" = "#d55e00", 
    "p < 0.05" = "#005824", 
    "p < 0.1" = "#0072b2"
  ),
  name = "Significance") +
  # Scale with expanded limits to fit labels
  scale_x_continuous(
    trans = "log2",
    breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64),
    labels = c("0.125", "0.25", "0.5", "1", "2", "4", "8", "16", "32", "64"),
    limits = c(0.05, 64) # Expanded limits to fit all values
  ) +
  # Labels
  labs(
    title = "Significant Predictors of Financial Strain",
    subtitle = "Odds Ratios with 95% Confidence Intervals",
    x = "Odds Ratio (log scale)",
    y = ""
  ) +
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 15, hjust = 0),
    axis.text.y = element_text(size = 18), 
    axis.text.x = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    legend.text = element_text(size = 14), 
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

print(m1_forest)

# Save the plot
ggsave(
  "significant_predictors_forest_plot.png",
  plot = m1_forest,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)