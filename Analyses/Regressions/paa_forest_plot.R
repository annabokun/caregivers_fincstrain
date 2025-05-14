# Load library
library(ggplot2)

# Create data
forest_data <- data.frame(
  Variable = c("Co-residence with parent (ref = no co-residence)",
               "Poor care recipient health (ref = good/excellent)",
               "Age 18–44 (ref = 65-75)",
               "Age 55–64 (ref = 65-75)",
               "Married (ref = not married)",
               "Income: $50k+ (ref = $10-20k)"),
  OR = c(2.23, 2.52, 6.63, 5.06, 1.74, 0.42),
  Lower = c(1.25, 1.28, 1.62, 1.44, 0.97, 0.15),
  Upper = c(3.95, 5.46, 45.1, 32.2, 3.21, 1.06),
  pval = c(0.006, 0.012, 0.019, 0.031, 0.069, 0.077)
)

# Assign significance level
forest_data$sig_level <- cut(forest_data$pval,
                             breaks = c(-Inf, 0.01, 0.05, 0.1),
                             labels = c("p < 0.01", "p < 0.05", "p < 0.1"))

# Set color palette
sig_colors <- c("p < 0.01" = "darkred",
                "p < 0.05" = "darkorange",
                "p < 0.1"  = "darkgreen")

# Order for plotting
forest_data$Variable <- factor(forest_data$Variable, levels = rev(forest_data$Variable))

# Plot with labels above points
ggplot(forest_data, aes(x = OR, y = Variable, color = sig_level)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = round(OR, 2)), 
            vjust = -1,  # positions label above the point
            size = 5, 
            color = "black") +
  scale_x_log10(limits = c(0.1, 50)) +
  scale_color_manual(values = sig_colors, name = "Significance Level") +
  labs(
    title = "Significant Predictors of Financial Strain",
    subtitle = "Odds Ratios with 95% Confidence Intervals",
    x = "Odds Ratio (log scale)",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
