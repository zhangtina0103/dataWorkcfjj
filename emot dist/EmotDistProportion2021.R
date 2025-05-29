library(dplyr)
library(ggplot2)
library(readr)

df_2021 <- read_csv("~/Downloads/2021_ps_race.csv")

# proportion of Emotionally Disturbed Children by Race/Ethnicity
df_race_proportion <- df_2021 %>%
  group_by(RaceEthn) %>%
  summarise(
    EmotDistCount = sum(EmotDist == 1, na.rm = TRUE),
    TotalCount = n(),
    .groups = "drop"
  ) %>%
  # calculate proportion
  mutate(Proportion_EmotDist = EmotDistCount / TotalCount)

df_race_proportion$RaceEthn <- factor(df_race_proportion$RaceEthn)

# mapping number to race
race_labels <- c(
  "1" = "White", 
  "2" = "Black", 
  "3" = "Native American", 
  "4" = "Asian", 
  "5" = "Pacific Islander", 
  "6" = "Multiracial", 
  "7" = "Hispanic",   # Add Hispanic category
  "99" = "Unknown"
)

# bar plot for proportion of Emotionally Disturbed Children by Race/Ethnicity
ggplot(df_race_proportion, aes(x = RaceEthn, y = Proportion_EmotDist, fill = RaceEthn)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Proportion_EmotDist)), vjust = -0.3, size = 3) +
  labs(
    title = "Proportion of Emotionally Disturbed Children by Race/Ethnicity (2021)",
    x = "Race/Ethnicity",
    y = "Proportion of Emotionally Disturbed Children",
    fill = "Race/Ethnicity"
  ) +
  scale_fill_manual(values = c(
    "White" = "#1f77b4", 
    "Black" = "#ff7f0e", 
    "Native American" = "#2ca02c", 
    "Asian" = "#d62728", 
    "Pacific Islander" = "#9467bd", 
    "Multiracial" = "#8c564b", 
    "Hispanic" = "#e377c2", 
    "Unknown" = "#7f7f7f"
  )) +
  scale_x_discrete(labels = race_labels) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )
