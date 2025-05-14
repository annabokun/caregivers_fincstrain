#### Bar charts --------------------------- 
# Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# Create the dataset for financial stressors with fixed values and explicit ordering
stressors_data <- data.frame(
  Category = rep(c("Financial strain", "Housing", "Travel/Vacation", 
                   "Supporting children", "Other/Unspecified"), each = 2),
  Residence = rep(c("Co-resident", "Non-coresident"), 5),
  Percentage = c(
    15.7, 10.1,  # Financial strain
    9.4, 7.0,    # Housing
    9.4, 6.6,    # Travel/Vacation
    5.8, 4.8,    # Supporting children
    4.2, 1.3     # Other/Unspecified
  ),
  SE = c(
    2.6, 1.4,    # Financial strain
    2.1, 1.2,    # Housing
    2.1, 1.2,    # Travel/Vacation
    1.7, 1.0,    # Supporting children
    1.5, 0.5     # Other/Unspecified
  )
)

# Create the dataset for financial resources with fixed values
resources_data <- data.frame(
  Category = rep(c("Medicaid", "Financial help from parent"), each = 2),
  Residence = rep(c("Co-resident", "Non-coresident"), 2),
  Percentage = c(
    16.2, 8.8,   # Medicaid
    15.2, 12.9   # Received financial help
  ),
  SE = c(
    2.7, 1.3,    # Medicaid
    2.6, 1.6     # Received financial help
  )
)

# Define custom order for stressors
stressor_order <- c("Financial strain", "Housing", "Travel/Vacation", 
                    "Supporting children", "Other/Unspecified")
stressors_data$Category <- factor(stressors_data$Category, levels = rev(stressor_order))

# Create stressors plot with fixed ordering
stressors_plot <- ggplot() +
  # Add bars for Co-resident
  geom_bar(data = stressors_data[stressors_data$Residence == "Co-resident",], 
           aes(x = Category, y = Percentage), 
           stat = "identity", fill = "#E41A1C", width = 0.4, 
           position = position_nudge(x = -0.225)) +
  # Add bars for Non-coresident
  geom_bar(data = stressors_data[stressors_data$Residence == "Non-coresident",], 
           aes(x = Category, y = Percentage), 
           stat = "identity", fill = "#377EB8", width = 0.4, 
           position = position_nudge(x = 0.225)) +
  # Add error bars for Co-resident
  geom_errorbar(data = stressors_data[stressors_data$Residence == "Co-resident",], 
                aes(x = Category, 
                    ymin = Percentage - SE, ymax = Percentage + SE), 
                width = 0.2, position = position_nudge(x = -0.225)) +
  # Add error bars for Non-coresident
  geom_errorbar(data = stressors_data[stressors_data$Residence == "Non-coresident",], 
                aes(x = Category, 
                    ymin = Percentage - SE, ymax = Percentage + SE), 
                width = 0.2, position = position_nudge(x = 0.225)) +
  # Add text labels for Co-resident
  geom_text(data = stressors_data[stressors_data$Residence == "Co-resident",], 
            aes(x = Category, 
                y = Percentage + SE + 1, 
                label = paste0(round(Percentage, 1), "%")),
            hjust = 0.5, size = 3.5, position = position_nudge(x = -0.225)) +
  # Add text labels for Non-coresident
  geom_text(data = stressors_data[stressors_data$Residence == "Non-coresident",], 
            aes(x = Category, 
                y = Percentage + SE + 1, 
                label = paste0(round(Percentage, 1), "%")),
            hjust = 0.5, size = 3.5, position = position_nudge(x = 0.225)) +
  # Set up scales and formatting
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) +
  coord_flip() +
  labs(title = "Financial Stressors by Co-residence Status", 
       subtitle = "Percentage of caregivers reporting each stressor",
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

# Define custom order for resources
resource_order <- c("Medicaid", "Financial help from parent")
resources_data$Category <- factor(resources_data$Category, levels = resource_order)

# Create resources plot
resources_plot <- ggplot() +
  # Add bars for Co-resident
  geom_bar(data = resources_data[resources_data$Residence == "Co-resident",], 
           aes(x = Category, y = Percentage), 
           stat = "identity", fill = "#E41A1C", width = 0.4, 
           position = position_nudge(x = -0.225)) +
  # Add bars for Non-coresident
  geom_bar(data = resources_data[resources_data$Residence == "Non-coresident",], 
           aes(x = Category, y = Percentage), 
           stat = "identity", fill = "#377EB8", width = 0.4, 
           position = position_nudge(x = 0.225)) +
  # Add error bars for Co-resident
  geom_errorbar(data = resources_data[resources_data$Residence == "Co-resident",], 
                aes(x = Category, 
                    ymin = Percentage - SE, ymax = Percentage + SE), 
                width = 0.2, position = position_nudge(x = -0.225)) +
  # Add error bars for Non-coresident
  geom_errorbar(data = resources_data[resources_data$Residence == "Non-coresident",], 
                aes(x = Category, 
                    ymin = Percentage - SE, ymax = Percentage + SE), 
                width = 0.2, position = position_nudge(x = 0.225)) +
  # Add text labels for Co-resident
  geom_text(data = resources_data[resources_data$Residence == "Co-resident",], 
            aes(x = Category, 
                y = Percentage + SE + 1, 
                label = paste0(round(Percentage, 1), "%")),
            hjust = 0.5, size = 3.5, position = position_nudge(x = -0.225)) +
  # Add text labels for Non-coresident
  geom_text(data = resources_data[resources_data$Residence == "Non-coresident",], 
            aes(x = Category, 
                y = Percentage + SE + 1, 
                label = paste0(round(Percentage, 1), "%")),
            hjust = 0.5, size = 3.5, position = position_nudge(x = 0.225)) +
  # Set up scales and formatting
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) +
  coord_flip() +
  labs(title = "Financial Resources by Co-residence Status", 
       x = "", 
       y = "Percentage (%)") +
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
legend_grob <- grobTree(
  rectGrob(x = 0.25, y = 0.5, width = 0.04, height = 0.6, gp = gpar(fill = "#E41A1C")),
  textGrob("Co-resident", x = 0.32, y = 0.5, just = "left", gp = gpar(fontsize = 10)),
  rectGrob(x = 0.6, y = 0.5, width = 0.04, height = 0.6, gp = gpar(fill = "#377EB8")),
  textGrob("Non-coresident", x = 0.67, y = 0.5, just = "left", gp = gpar(fontsize = 10))
)

# Combine the plots
final_plot <- grid.arrange(
  legend_grob,
  stressors_plot,
  resources_plot,
  ncol = 1,
  heights = c(0.1, 1.5, 1),
  top = textGrob("Economic Characteristics of Adult Children Caregivers by Co-residence Status", 
                 x = 0.02, just = "left", 
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

print(final_plot)