library(ggplot2)
library(dplyr)
library(readr)

years <- 2000:2021

life_los_data <- data.frame(Year = integer(), Total_LifeLOS = numeric())

for (year in years) {
  file_path <- paste0("~/Downloads/", year, "_ps_race.csv")

  if (file.exists(file_path)) {
    df <- read_csv(file_path)
    
    # aggregate LifeLOS for year
    year_total_life_los <- df %>%
      summarise(Total_LifeLOS = sum(LifeLOS, na.rm = TRUE)) %>%
      mutate(Year = year)
    
    # add results to aggregated data
    life_los_data <- bind_rows(life_los_data, year_total_life_los)
  }
}

# line plot for Total LifeLOS over years
ggplot(life_los_data, aes(x = Year, y = Total_LifeLOS)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  geom_text(aes(label = Total_LifeLOS), vjust = -0.5, size = 2.5, angle = 45, hjust = 0.5) +
  labs(
    title = "Total Length of Time in Foster Care: 2000-2021",
    x = "Year",
    y = "Total Length of Time (Days)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12)
  )