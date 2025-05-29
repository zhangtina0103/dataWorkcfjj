library(ggplot2)
library(dplyr)
library(readr)

race_labels <- c(
  "1" = "White", "2" = "Black", "3" = "Native American", "4" = "Asian",
  "5" = "Pacific Islander", "6" = "Mixed", "7" = "Hispanic", "99" = "Unknown"
)

years <- 2000:2021

life_los_race_data <- data.frame(Year = integer(), RaceEthn = character(), Total_LifeLOS = numeric())
average_los_race_data <- data.frame(Year = integer(), RaceEthn = character(), Average_LifeLOS = numeric())

for (year in years) {
  file_path <- paste0("~/Downloads/", year, "_ps_race.csv")
  
  if (file.exists(file_path)) {
    df <- read_csv(file_path)
    
    # filter for non-NA RaceEthn and calculate Total LifeLOS by RaceEthn and Year
    year_race_life_los <- df %>%
      filter(!is.na(RaceEthn)) %>%
      group_by(RaceEthn, Year = year) %>%
      summarise(Total_LifeLOS = sum(LifeLOS, na.rm = TRUE), .groups = "drop") %>%
      mutate(RaceEthn = factor(as.character(RaceEthn), levels = names(race_labels), labels = race_labels))
    
    # add results to aggregated data
    life_los_race_data <- bind_rows(life_los_race_data, year_race_life_los)
    
    # calculate average LifeLOS
    # divide by count of people per year and race
    year_race_average_los <- df %>%
      filter(!is.na(RaceEthn)) %>%
      group_by(RaceEthn, Year = year) %>%
      summarise(
        Total_LifeLOS = sum(LifeLOS, na.rm = TRUE),
        Count = n(),
        .groups = "drop"
      ) %>%
      mutate(Average_LifeLOS = Total_LifeLOS / Count) %>%
      mutate(RaceEthn = factor(as.character(RaceEthn), levels = names(race_labels), labels = race_labels))
    
    # add to average data
    average_los_race_data <- bind_rows(average_los_race_data, year_race_average_los)
  }
}

# line plot for Average LifeLOS by Race/Ethnicity over years
ggplot(average_los_race_data, aes(x = Year, y = Average_LifeLOS, color = RaceEthn, group = RaceEthn)) +
  geom_line(size = 1) +
  geom_point(size = 1) + 
  labs(
    title = "Average Length of Stay in Foster Care by Race/Ethnicity: 2000-2021",
    x = "Year",
    y = "Average Length of Stay (Days)",
    color = NULL
  ) +
  scale_color_manual(values = c(
    "White" = "#1f77b4", "Black" = "#ff7f0e", "Native American" = "#2ca02c", 
    "Asian" = "#d62728", "Pacific Islander" = "#9467bd", "Mixed" = "#8c564b", 
    "Hispanic" = "#e377c2", "Unknown" = "#7f7f7f"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )