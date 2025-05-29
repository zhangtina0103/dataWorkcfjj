library(tidyverse)

file_path <- "~/Downloads/Filtered_MA_data.csv"
data <- read_csv(file_path)

curplset_mapping <- c(
  "1" = "Pre-adoptive home",
  "2" = "Foster family home, relative",
  "3" = "Foster family home, non-relative",
  "4" = "Group home",
  "5" = "Institution",
  "6" = "Supervised independent living",
  "7" = "Runaway",
  "8" = "Trial home visit",
  "99" = "Unknown"
)

data$CurPlSet <- curplset_mapping[as.character(data$CurPlSet)]

data_summary <- data %>%
  group_by(FY, CurPlSet) %>%
  summarise(TotalNumPlep = sum(NumPlep, na.rm = TRUE)) %>%
  ungroup()

# line graph
ggplot(data_summary, aes(x = FY, y = TotalNumPlep, color = CurPlSet, group = CurPlSet)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Placement Types Trend Over Time",
       x = "Year",
       y = "Total Number of Placements",
       color = "Placement Type") +
  theme_minimal() +
  theme(legend.position = "right")

