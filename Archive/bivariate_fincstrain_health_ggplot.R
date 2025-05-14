library(ggplot2)
library(dplyr)

# Create health data
health_data <- data.frame(
  Category = factor(c("Poor self-rated health", "Feeling sad (most days)", "Caring for parent with dementia"),
                    levels = c("Caring for parent with dementia", "Poor self-rated health", "Feeling sad (most days)")),
  With_Financial_Strain = c(51, 12, 63),
  Without_Financial_Strain = c(39, 3, 39),
  With_SE = c(5.7, 3.8, 5.5),
  Without_SE = c(2.0, 0.7, 2.0)
)

# Reshape data to long format for better ggplot2 handling
health_data_long <- health_data %>%
  tidyr::pivot_longer(
    cols = c(With_Financial_Strain, Without_Financial_Strain),
    names_to = "Strain_Group",
    values_to = "Percentage"
  ) %>%
  mutate(
    SE = ifelse(Strain_Group == "With_Financial_Strain", With_SE, Without_SE),
    Strain_Group = factor(Strain_Group, 
                          levels = c("With_Financial_Strain", "Without_Financial_Strain"),
                          labels = c("With Financial Strain", "Without Financial Strain"))
  )

# Create health plot with proper legend
health_plot <- ggplot(health_data_long, 
                      aes(x = Category, y = Percentage, fill = Strain_Group, group = Strain_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Percentage - SE, ymax = Percentage + SE),
                position = position_dodge(width = 0.7), width = 0.2) +
  geom_text(aes(label = paste0(Percentage, "%"), y = Percentage + SE + 3),
            position = position_dodge(width = 0.7), hjust = 0.5, size = 6) +
  scale_fill_manual(values = c("With Financial Strain" = "darkorange", 
                               "Without Financial Strain" = "#377EB8")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75)) +
  coord_flip() +
  labs(title = "Significant Health Differences of Adult Children Caregivers, by Financial Strain", 
       subtitle = "Data: HEPESE (2010-2011)",
       x = "", 
       y = "Percentage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 15, hjust = 0),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "gray80"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1, "cm")  # Make legend keys bigger
  ) 

print(health_plot)

# Save the plot
ggsave(
  "bivariate_health_diff.png",
  plot = health_plot,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)