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

raceethn_mapping <- c(
  "1" = "White",
  "2" = "African American",
  "4" = "Asian",
  "6" = "Multiracial",
  "7" = "Hispanic",
  "99" = "Unknown"
)

data$CurPlSet <- curplset_mapping[as.character(data$CurPlSet)]
data$RaceEthn <- raceethn_mapping[as.character(data$RaceEthn)]

filtered_data_group_home <- data %>%
  filter(CurPlSet == "Group home") %>%
  filter(RaceEthn != "Unknown")

data_summary_group_home <- filtered_data_group_home %>%
  group_by(FY, RaceEthn) %>%
  summarise(TotalNumPlep = sum(NumPlep, na.rm = TRUE)) %>%
  ungroup()

total_people_per_race_per_year <- data %>%
  group_by(FY, RaceEthn) %>%
  summarise(TotalPeople = sum(NumPlep, na.rm = TRUE)) %>%
  ungroup()

# merge placements data and total people data
merged_data <- data_summary_group_home %>%
  left_join(total_people_per_race_per_year, by = c("FY", "RaceEthn"))

# calculate the proportion using TotalNumPlep / TotalPeople
merged_data <- merged_data %>%
  mutate(Proportion = TotalNumPlep / TotalPeople)

# plot proportion of placements for each race over years
ggplot(merged_data, aes(x = FY, y = Proportion, color = RaceEthn, group = RaceEthn)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +
  labs(title = "Proportion of Group Home Placements for Races Over the Years",
       x = "Year",
       y = "Proportion of Placements",
       color = "Race/Ethnicity") +
  theme_minimal() +
  theme(legend.position = "right")

