library(ggplot2)
library(dplyr)
library(readr)

file_path <- "~/Downloads/2021_ps_race.csv"
df <- read_csv(file_path)

# filter for MA (State == 25), group by AgeAtLatRem
df_filtered <- df %>%
  filter(State == 25) %>%
  group_by(AgeAtLatRem) %>%
  summarise(Total_LifeLOS = sum(LifeLOS, na.rm = TRUE))  # Aggregate LifeLOS by AgeAtLatRem

# bar graph for AgeAtLatRem vs. Total_LifeLOS with data labels
ggplot(df_filtered, aes(x = factor(AgeAtLatRem), y = Total_LifeLOS, fill = as.factor(AgeAtLatRem))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis_d() +
  geom_text(aes(label = Total_LifeLOS), vjust = -0.5, size = 3) + 
  labs(
    title = "Total Length of Stay in Out-of-Home Placement by Age at Latest Removal",
    x = "Age at Latest Removal",
    y = "Total Length of Stay (Days)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 8)
  )