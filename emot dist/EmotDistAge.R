library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

file_path <- "~/Downloads/2021_ps_race.csv"
df <- read_csv(file_path)

# convert DOB to age (2021-year)
df <- df %>%
  mutate(Age = 2021 - year(DOB))

# filter for MA (State == 25)
df_filtered <- df %>%
  filter(State == 25)

age_summary <- df_filtered %>%
  group_by(Age) %>%
  summarise(
    Total_Count = n(),
    EmotDist_Count = sum(EmotDist, na.rm = TRUE)
  ) %>%
  mutate(EmotDist_Percentage = (EmotDist_Count / Total_Count) * 100)

# bar graph for EmotDist by age
ggplot(age_summary, aes(x = factor(Age))) +
  geom_bar(aes(y = Total_Count, fill = "Total"), stat = "identity", color = "black") +
  geom_bar(aes(y = EmotDist_Count, fill = "Emotionally Disturbed"), stat = "identity") +
  scale_fill_manual(values = c("Total" = "gray", "Emotionally Disturbed" = "red")) +
  geom_text(aes(y = Total_Count, label = Total_Count), vjust = -0.5, size = 3) +
  geom_text(aes(y = EmotDist_Count, label = paste0(round(EmotDist_Percentage, 1), "%")), vjust = -0.5, size = 3) +  # Label Emotionally Disturbed Percentage
  labs(
    title = "Breakdown of Emotionally Disturbed Children by Age",
    x = "Age",
    y = "Number of Children"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 8)
  ) +
  guides(fill = guide_legend(title = NULL))
