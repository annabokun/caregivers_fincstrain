library(ggplot2)
library(dplyr)

# Create hours data
hours_data <- data.frame(
  Category = c("ADL", "IADL"),
  With_Financial_Strain = c(6.8, 6.7),
  Without_Financial_Strain = c(5.0, 4.7),
  With_SE = c(0.5, 0.6),
  Without_SE = c(0.3, 0.4)
)

# Convert to long format for easier plotting with standard legend
hours_data_long <- hours_data %>%
  tidyr::pivot_longer(
    cols = c(With_Financial_Strain, Without_Financial_Strain),
    names_to = "Strain_Status",
    values_to = "Hours"
  ) %>%
  mutate(
    SE = ifelse(Strain_Status == "With_Financial_Strain", 
                hours_data$With_SE[match(Category, hours_data$Category)],
                hours_data$Without_SE[match(Category, hours_data$Category)]),
    # Calculate the position for labels at the end of error bars
    Label_Position = Hours + SE
  )

# Create hours plot with proper legend
hours_plot <- ggplot(hours_data_long, aes(x = Category, y = Hours, fill = Strain_Status)) +
  # Add bars
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # Add error bars
  geom_errorbar(aes(ymin = Hours - SE, ymax = Hours + SE),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  # Add text labels at the end of error bars
  geom_text(aes(y = Label_Position, label = sprintf("%.1f", Hours)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 6) +
  # Set up scales and formatting
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9)) +  # Increased limit to make room for labels
  scale_fill_manual(
    values = c("With_Financial_Strain" = "#FF8C00", "Without_Financial_Strain" = "#377EB8"),
    labels = c("With Financial Strain", "Without Financial Strain")
  ) +
  labs(title = "Significant Caregiving Hours Differences of Adult Children Caregivers, by Financial Strain", 
       subtitle = "Data: HEPESE (2010-2011)",
       x = "", 
       y = "Hours per day",
       fill = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray80"),
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    legend.margin = margin(6, 0, 6, 0),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  )

print(hours_plot)


# Save the plot
ggsave(
  "bivariate_hours_diff.png",  # File name
  plot = hours_plot,                                # Your plot object
  width = 12,                              # Width in inches
  height = 7,                              # Height in inches
  dpi = 300,                               # Resolution (300 dpi is print quality)
  bg = "white"                             # White background
)


