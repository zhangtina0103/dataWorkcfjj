library(tidyverse)

# can filter for any race (currently white)
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

raceethn_mapping <- c(
  "1" = "Non-Hispanic, White",
  "2" = "Non-Hispanic, Black or African American",
  "3" = "Non-Hispanic, American Indian or Native Alaskan",
  "4" = "Non-Hispanic, Asian",
  "5" = "Non-Hispanic, Native Hawaiian or Other Pacific Islander",
  "6" = "Non-Hispanic, More than One Race",
  "7" = "Hispanic or Latino",
  "99" = "Missing or Unknown"
)

data$CurPlSet <- curplset_mapping[as.character(data$CurPlSet)]
data$RaceEthn <- raceethn_mapping[as.character(data$RaceEthn)]

# filter for just white
filtered_data <- data %>%
  filter(RaceEthn == "Non-Hispanic, Black or African American")

data_summary <- filtered_data %>%
  group_by(FY, CurPlSet) %>%
  summarise(TotalNumPlep = sum(NumPlep, na.rm = TRUE)) %>%
  ungroup()

# line graph
ggplot(data_summary, aes(x = FY, y = TotalNumPlep, color = CurPlSet, group = CurPlSet)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +
  labs(title = "Trends for All Placement Types for White Children",
       x = "Year",
       y = "Total Number of Placements",
       color = "Placement Type") +
  theme_minimal() +
  theme(legend.position = "right")
