#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

# "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
# PAA 2025

  # Bivariate descriptives plot

# Script created in RStudio ("Cranberry Hibiscus")
# updated 2/24/2025
#=======================================================================================================#


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# Create health data
health_data <- data.frame(
  Category = factor(c("Poor self-rated health", "Feeling sad (most days)", "Caring for parent with dementia"),
                    levels = c("Caring for parent with dementia", "Poor self-rated health", "Feeling sad (most days)")),
  With_Financial_Strain = c(51, 12, 63),
  Without_Financial_Strain = c(39, 3, 39),
  With_SE = c(5.7, 3.8, 5.5),
  Without_SE = c(2.0, 0.7, 2.0)
)

# Create hours data
hours_data <- data.frame(
  Category = c("ADL", "IADL"),
  With_Financial_Strain = c(6.8, 6.7),
  Without_Financial_Strain = c(5.0, 4.7),
  With_SE = c(0.5, 0.6),
  Without_SE = c(0.3, 0.4)
)

# Create health plot
health_plot <- ggplot() +
  # Add bars for With Financial Strain
  geom_bar(data = health_data, aes(x = Category, y = With_Financial_Strain), 
           stat = "identity", fill = "#E41A1C", width = 0.4, position = position_nudge(x = -0.225)) +
  # Add bars for Without Financial Strain
  geom_bar(data = health_data, aes(x = Category, y = Without_Financial_Strain), 
           stat = "identity", fill = "#377EB8", width = 0.4, position = position_nudge(x = 0.225)) +
  # Add error bars for With Financial Strain
  geom_errorbar(data = health_data, 
                aes(x = Category, ymin = With_Financial_Strain - With_SE, ymax = With_Financial_Strain + With_SE), 
                width = 0.2, position = position_nudge(x = -0.225)) +
  # Add error bars for Without Financial Strain
  geom_errorbar(data = health_data, 
                aes(x = Category, ymin = Without_Financial_Strain - Without_SE, ymax = Without_Financial_Strain + Without_SE), 
                width = 0.2, position = position_nudge(x = 0.225)) +
  # Add text labels for With Financial Strain
  geom_text(data = health_data, 
            aes(x = Category, y = With_Financial_Strain + With_SE + 3, label = paste0(With_Financial_Strain, "%")),
            hjust = 0.5, size = 3.5, position = position_nudge(x = -0.225)) +
  # Add text labels for Without Financial Strain
  geom_text(data = health_data, 
            aes(x = Category, y = Without_Financial_Strain + Without_SE + 3, label = paste0(Without_Financial_Strain, "%")),
            hjust = 0.5, size = 3.5, position = position_nudge(x = 0.225)) +
  # Set up scales and formatting
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75)) +
  coord_flip() +
  labs(title = "Caregiver Health", 
       subtitle = "Percentage of caregivers reporting each outcome",
       x = "", 
       y = "Percentage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "gray80"),
    panel.grid.major.y = element_blank()
  )

# Create hours plot
hours_plot <- ggplot() +
  # Add bars for With Financial Strain
  geom_bar(data = hours_data, aes(x = Category, y = With_Financial_Strain), 
           stat = "identity", fill = "#E41A1C", width = 0.4, position = position_nudge(x = -0.225)) +
  # Add bars for Without Financial Strain
  geom_bar(data = hours_data, aes(x = Category, y = Without_Financial_Strain), 
           stat = "identity", fill = "#377EB8", width = 0.4, position = position_nudge(x = 0.225)) +
  # Add error bars for With Financial Strain
  geom_errorbar(data = hours_data, 
                aes(x = Category, ymin = With_Financial_Strain - With_SE, ymax = With_Financial_Strain + With_SE), 
                width = 0.2, position = position_nudge(x = -0.225)) +
  # Add error bars for Without Financial Strain
  geom_errorbar(data = hours_data, 
                aes(x = Category, ymin = Without_Financial_Strain - Without_SE, ymax = Without_Financial_Strain + Without_SE), 
                width = 0.2, position = position_nudge(x = 0.225)) +
  # Add text labels for With Financial Strain
  geom_text(data = hours_data, 
            aes(x = Category, y = With_Financial_Strain + With_SE + 0.4, label = sprintf("%.1f", With_Financial_Strain)),
            hjust = 0.5, size = 3.5, position = position_nudge(x = -0.225)) +
  # Add text labels for Without Financial Strain
  geom_text(data = hours_data, 
            aes(x = Category, y = Without_Financial_Strain + Without_SE + 0.4, label = sprintf("%.1f", Without_Financial_Strain)),
            hjust = 0.5, size = 3.5, position = position_nudge(x = 0.225)) +
  # Set up scales and formatting
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8.5)) +
  labs(title = "Daily Caregiving Hours", 
       x = "", 
       y = "Hours per day") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray80")
  )

# Create a manual legend
legend <- ggplot() +
  geom_rect(aes(xmin = 1, xmax = 2, ymin = 0, ymax = 1), fill = "#E41A1C") +
  geom_rect(aes(xmin = 3, xmax = 4, ymin = 0, ymax = 1), fill = "#377EB8") +
  geom_text(aes(x = 1.5, y = 0.5, label = "With Financial Strain"), hjust = 0.5) +
  geom_text(aes(x = 3.5, y = 0.5, label = "Without Financial Strain"), hjust = 0.5) +
  scale_x_continuous(limits = c(0, 5)) +
  theme_void()

# Create a manual legend using grid grobs
legend_grob <- grobTree(
  rectGrob(x = 0.25, y = 0.5, width = 0.04, height = 0.6, gp = gpar(fill = "#E41A1C")),
  textGrob("With Financial Strain", x = 0.32, y = 0.5, just = "left", gp = gpar(fontsize = 10)),
  rectGrob(x = 0.6, y = 0.5, width = 0.04, height = 0.6, gp = gpar(fill = "#377EB8")),
  textGrob("Without Financial Strain", x = 0.67, y = 0.5, just = "left", gp = gpar(fontsize = 10))
)

# Combine the plots
final_plot <- grid.arrange(
  legend_grob,
  health_plot,
  hours_plot,
  ncol = 1,
  heights = c(0.1, 1.5, 1),
  top = textGrob("Financial Strain Associations Among Adult Child Caregivers", 
                 x = 0.02, just = "left", 
                 gp = gpar(fontsize = 16, fontface = "bold"))
)


print(final_plot)


# Save the combined plot
ggsave("financial_strain_associations.pdf", final_plot, width = 10, height = 8, dpi = 300)