library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

data <- read_csv("~/Downloads/Average number of placement settings by race_ethnicity - Sheet1.csv")

names(data) <- gsub("/", "_", names(data))

# reshape data
data_long <- data %>%
  pivot_longer(cols=c(White, Total,	Hispanic_Latinx, Black, Asian,	Multiracial,	Unknown),
               names_to = "race_ethnicity", 
               values_to = "average_placement_settings")

# Calculate the differences between consecutive years
data_long <- data_long %>%
  group_by(Year) %>%
  arrange(Year) %>%
  mutate(change = average_placement_settings - lag(average_placement_settings))

# Reorder the 'race_ethnicity' factor to move 'Total' to the first position
data_long$race_ethnicity <- factor(data_long$race_ethnicity, levels = c("Total", "White", "Hispanic_Latinx", "Black", "Asian", "Multiracial", "Unknown"))

filter(data_long, race_ethnicity == "White")


# Create the heatmap of average placement settings
ggplot(data_long, aes(x = as.factor(Year), y = race_ethnicity, fill = average_placement_settings)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(data_long$average_placement_settings, na.rm = TRUE)) +  # Diverging color scale
  #geom_text(aes(label = round(average_placement_settings, 2))) +  # Add text labels with average placement values
  labs(title = "Placement Stability Breakdown by Race: 2000-2021",
       x = "Year", 
       y = "Race/Ethnicity", 
       fill="Average Placement Settings") +
  scale_y_discrete(labels = c("White" = "White", "Hispanic_Latinx" = "Hispanic/Latinx", 
                                  "Black" = "Black", "Asian" = "Asian", 
                                  "Multiracial" = "Multiracial", "Unknown" = "Unknown",
                                  "Total" = "Average")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title.x = element_text(size = 12, vjust=-5, hjust = 0.5),
        axis.title.y = element_text(size = 12, vjust = 8, hjust = 0.5),
        plot.margin = margin(30, 30, 40, 40))  # Adjust margins for better v