library(ggplot2)
library(dplyr)
library(tidyr)
# Import data downloaded from FanGraphs
data <- read.csv("~/Downloads/kyletucker1.csv") 

# Select only the desired columns
selected_columns <- c("Team", "RBI", "BB.", "K.","AVG", "wOBA", "wRC.", "OPS", "PA")

# Create a new data frame with the selected columns
subset_analysisDf <- data[, selected_columns]

# Turn the data frame vertically
subset_analysisDf_long <- tidyr::gather(subset_analysisDf, metric, value, -Team, na.rm = TRUE)

# Rearrange the vertical data frame to keep each metric together
subset_analysisDf_long <- subset_analysisDf_long %>%
  arrange(metric, Team, desc(Team == "After"), desc(Team == "Before"))

#print(subset_analysisDf_long) 

#position_nudge_vals <- c("Before" = -0.2, "After" = 0.2)

# Relabel the statistics
custom_labels <- c(
  "Team" = "Time",
  "PA" = "Plate Appearances",
  "RBI" = "RBIs",
  "BB." = "Walk Rate",
  "K." = "Strikeout Rate",
  "AVG" = "Batting Average",
  "wOBA" = "Weighted On-base Average",
  "wRC." = "Weighted Runs Created Plus",
  "OPS" = "OPS"
)

# Define the order
facet_order <- c("PA", "AVG", "RBI", "OPS", "BB." , "K.", "wOBA", "wRC.")
subset_analysisDf_long$metric <- factor(subset_analysisDf_long$metric, levels = facet_order)

# Reorder levels of team factor to have "Before" on the left and "After" on the right
subset_analysisDf_long$Team <- factor(subset_analysisDf_long$Team, levels = c("Before", "After"))

# Add color to the plot
team_colors <- c("Before" = "black", "After" = "darkorange")


# Graph
ggplot(subset_analysisDf_long, aes(x = Team, y = value, fill = Team)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = 1.5, color = "white") +
  facet_wrap(~ metric, scales = "free_y", ncol = 2, labeller = as_labeller(custom_labels)) +
  labs(y = "Value", fill = "Metric", title = "Kyle Tucker Since Wearing Batting Gloves") +
  scale_fill_manual(values = team_colors) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_blank())  # Remove y-axis title)



