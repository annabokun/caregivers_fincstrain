library(ggplot2)
library(dplyr)
library(tidyr)

# Create dataset with only financial strain and Medicaid variables
significant_data <- data.frame(
  Measure = c("Financial strain", "Medicaid"),
  Category = c("Financial Strain", "Health Insurance"),
  Co_resident = c(15.7, 16.2),
  Non_coresident = c(10.1, 8.8),
  p_value = c(0.059, 0.009)
)

# Sample sizes
n_co_resident <- 191
n_non_coresident <- 456

# Calculate standard errors
significant_data <- significant_data %>%
  mutate(
    # Standard errors
    Co_resident_SE = sqrt((Co_resident/100 * (1 - Co_resident/100)) / n_co_resident) * 100,
    Non_coresident_SE = sqrt((Non_coresident/100 * (1 - Non_coresident/100)) / n_non_coresident) * 100
  )

# Convert to long format
significant_long <- significant_data %>%
  pivot_longer(
    cols = c(Co_resident, Non_coresident),
    names_to = "Residence", 
    values_to = "Percentage"
  ) %>%
  mutate(
    SE = case_when(
      Residence == "Co_resident" ~ significant_data$Co_resident_SE[match(Measure, significant_data$Measure)],
      Residence == "Non_coresident" ~ significant_data$Non_coresident_SE[match(Measure, significant_data$Measure)]
    ),
    # Calculate position for text labels at the end of error bars
    Label_Position = Percentage + SE
  )

# Create significance markers
significant_long <- significant_long %>%
  mutate(
    significance = case_when(
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.1 ~ "†",
      TRUE ~ ""
    ),
    # Create measure with significance markers
    Measure_Label = paste0(Measure, 
                           ifelse(Measure == "Financial strain", "†", "**")),
    # Order variables
    Measure_Label = factor(Measure_Label, 
                           levels = c("Medicaid**", "Financial strain†"))
  )

# Create the plot
p <- ggplot(significant_long, aes(x = Percentage, y = Measure_Label, fill = Residence)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # Add error bars
  geom_errorbar(aes(xmin = Percentage - SE, xmax = Percentage + SE),
                position = position_dodge(width = 0.8),
                width = 0.25, color = "black", linewidth = 0.5) +
  # Place text labels at the end of error bars
  geom_text(aes(x = Label_Position, label = paste0(Percentage, "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.2,
            size = 6) +
  scale_fill_manual(values = c("Co_resident" = "darkorange", "Non_coresident" = "#377EB8"),
                    labels = c("Co-resident (n=191)", "Non-co-resident (n=456)")) +
  labs(title = "Significant Economic Differences Between Co-resident and Non-co-resident Adult Children Caregivers",
       subtitle = "Data: HEPESE (2010-2011)",
       x = "Percentage (%)",
       y = NULL,
       fill = NULL) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +  # Adjusted x-axis limit
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 18), 
        plot.subtitle = element_text(size = 15), 
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 18)) +
  facet_grid(Category ~ ., scales = "free_y", space = "free_y") +
  labs(caption = "Statistical significance based on chi-square tests: † p<0.1; ** p<0.01")


print(p)

# Save 
ggsave(
  "bivariate_econ_diff.png",  # File name
  plot = p,                                # Your plot object
  width = 13,                              # Width in inches
  height = 7,                              # Height in inches
  dpi = 300,                               # Resolution (300 dpi is print quality)
  bg = "white"                             # White background
)