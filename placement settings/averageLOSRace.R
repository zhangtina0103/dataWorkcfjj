library(ggplot2)
library(dplyr)
library(readr)

file_path <- "~/Downloads/2021_ps_race.csv"
df <- read_csv(file_path)

# race/ethnicity mapping
race_labels <- c(
  "1" = "White",
  "2" = "Black",
  "3" = "Native American",
  "4" = "Asian",
  "5" = "Pacific Islander",
  "6" = "Mixed",
  "7" = "Hispanic",
  "99" = "Unknown"
)

# filter for MA (State == 25), group by RaceEthn
df_filtered <- df %>%
  filter(State == 25) %>%
  group_by(RaceEthn) %>%
  summarise(
    Total_LifeLOS = sum(LifeLOS, na.rm = TRUE),
    Total_People = n()  # total number of people for each race/ethnicity
  ) %>%
  mutate(
    # calculate average LifeLOS per person
    Avg_LifeLOS = Total_LifeLOS / Total_People,  
    RaceEthn = factor(RaceEthn, levels = names(race_labels), labels = race_labels)
  )

# bar graph for RaceEthn vs. Avg_LifeLOS
ggplot(df_filtered, aes(x = RaceEthn, y = Avg_LifeLOS, fill = RaceEthn)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c(
    "White" = "blue", "Black" = "red", "Native American" = "green", "Asian" = "purple",
    "Pacific Islander" = "orange", "Mixed" = "brown", "Hispanic" = "pink", "Unknown" = "gray"
  )) +
  geom_text(aes(label = round(Avg_LifeLOS, 1)), vjust = -0.5, size = 3) +  # Add data labels with rounded average
  labs(
    title = "Average Length of Stay in Out-of-Home Placement by Race/Ethnicity",
    x = "Race/Ethnicity",
    y = "Average Length of Stay (Days)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 8)  # Smaller x-axis labels
  )