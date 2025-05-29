library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# Load the updated CSV data
data <- read_csv("~/Downloads/Average number of placement settings by race_ethnicity - Sheet1.csv")

names(data) <- gsub("/", "_", names(data))

# reshape data
long_data <- data %>%
  pivot_longer(cols=c(White, Total,	Hispanic_Latinx, Black, Asian,	Multiracial,	Unknown),
               names_to = "race_ethnicity", 
               values_to = "average_placement_settings")

# Plot the data
ggplot(long_data, aes(x = Year, y = average_placement_settings, color = race_ethnicity, group = race_ethnicity)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Number of Placement Settings by Race/Ethnicity: 2000-2021",
       x = "Year", 
       y = "Average Placement Settings") +
  scale_color_discrete(labels = c("White" = "White", "Hispanic_Latinx" = "Hispanic/Latinx", 
                                  "Black" = "Black", "Asian" = "Asian", 
                                  "Multiracial" = "Multiracial", "Unknown" = "Unknown",
                                  "Total" = "Average")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "top",
        plot.title = element_text(hjust=0.5))



# Plot the data with the "Total" line highlighted
ggplot(long_data, aes(x = Year, y = average_placement_settings, color = race_ethnicity, group = race_ethnicity)) +
  geom_line(data = subset(long_data, race_ethnicity == "Total"), 
            aes(color = race_ethnicity), size = 3) +  # Highlight Total with thicker line
  geom_point(data = subset(long_data, race_ethnicity == "Total"), 
             aes(color = race_ethnicity), size = 4) +  # Highlight Total with larger points
  geom_line(data = subset(long_data, race_ethnicity != "Total"), 
            aes(color = race_ethnicity)) +  # Other race/ethnicity lines
  geom_point(data = subset(long_data, race_ethnicity != "Total"), 
             aes(color = race_ethnicity)) +
  labs(title = "Average Number of Placement Settings by Race/Ethnicity: 2000-2021",
       x = "Year", 
       y = "Average Placement Settings") +
  
  scale_color_manual(values = c("Total" = "gray",  # Color the Total line (e.g., red)
                                "White" = "blue", 
                                "Hispanic_Latinx" = "green", 
                                "Black" = "purple", 
                                "Asian" = "orange", 
                                "Multiracial" = "brown", 
                                "Unknown" = "red")) + 
  scale_color_discrete(labels = c("White" = "White", "Hispanic_Latinx" = "Hispanic/Latinx", 
                                  "Black" = "Black", "Asian" = "Asian", 
                                  "Multiracial" = "Multiracial", "Unknown" = "Unknown",
                                  "Total" = "Average")) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = "top", 
        plot.title = element_text(hjust = 0.5),
        legend.spacing.x = unit(0.5, "cm"),  # Adjust spacing between items
        legend.key.size = unit(0.8, "cm")) +  # Adjust the size of the legend keys
        guides(color = guide_legend(nrow = 1),
               axis.title.x = element_text(margin = margin(t = 20)),  # Increase space between x-axis title and values
               axis.title.y = element_text(margin = margin(r = 20)),  # Increase space between y-axis title and values
               axis.text.x = element_text(margin = margin(t = 20)),  # Adjust space between x-axis values and plot area
               axis.text.y = element_text(margin = margin(r = 20)))  # Center the title

