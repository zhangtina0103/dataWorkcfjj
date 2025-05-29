library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

file_path <- "~/Downloads/2021_ps_race.csv"
df <- read_csv(file_path)

# Convert DOB to age (2021-year)
df <- df %>%
  mutate(Age = 2021 - year(DOB))

# filter for MA and emotionally disturbed only
df_filtered <- df %>%
  filter(State == 25, EmotDist == 1)

race_labels <- c(
  "1" = "White", "2" = "Black", "3" = "Native American", "4" = "Asian",
  "5" = "Pacific Islander", "6" = "Mixed", "7" = "Hispanic", "99" = "Unknown"
)

# summarize by age and race
age_race_summary <- df_filtered %>%
  group_by(Age, RaceEthn) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(RaceEthn = factor(as.character(RaceEthn), levels = names(race_labels), labels = race_labels))

ggplot(age_race_summary, aes(x = factor(Age), y = Count, fill = RaceEthn, label = Count)) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(
    position = position_stack(vjust = 0.5),
    size = 2.5,
    color = "black"
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Emotionally Disturbed Children by Age and Race/Ethnicity",
    x = "Age",
    y = "Number of Emotionally Disturbed Children",
    fill = "Race/Ethnicity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
  )