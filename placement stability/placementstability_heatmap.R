# HEATMAP
data <- data.frame(
  x = c(2014, 2018, 2019, 2021.5),
  National = c(4.12, 4.44, 4.44, 4.48),
  Massachusetts = c(6.23, 9.02, 7.1, 6.14)
)

# Reshape the data to long format
data_long <- melt(data, id.vars = "x")

# Calculate the differences between consecutive years
data_long <- data_long %>%
  group_by(variable) %>%
  arrange(x) %>%
  mutate(diff = value - lag(value)) %>%
  na.omit()  # Remove NA values resulting from the lag function

# customizing x-axis labels
data_long$x <- as.factor(data_long$x)
levels(data_long$x)[levels(data_long$x) == "2021.5"] <- "2021-2022"

# Create a heatmap of the differences
# Heatmap with diverging color scale (orange for improvement, blue for lagging)
ggplot(data_long, aes(x = as.factor(x), y = variable, fill = diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 0) +  # Diverging color scale
  geom_text(aes(label = paste0(round(diff, 2), "\nMoves Per \n2.7 Years"))) +  # Add difference labels
  labs(title = "Change in Placement Stability",
       x = "Year", 
       y = "Moves Per 1,000 Days (2.7 Years) in Foster Care", 
       fill="Key") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title.x = element_text(size = 12, vjust=-5, hjust = 0.5),
        axis.title.y = element_text(size = 12, vjust = 8, hjust = 0.5),
        plot.margin = margin(30, 30, 40, 40))  # Rotate x-axis labels for clarity
