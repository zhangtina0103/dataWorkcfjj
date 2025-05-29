library(tidyverse)

file_path <- "~/Downloads/Filtered_MA_data.csv"
data <- read_csv(file_path)

# map numbers to descriptions for RaceEthn
raceethn_mapping <- c(
  "1" = "White",
  "2" = "African American",
  "4" = "Asian",
  "6" = "Multiracial",
  "7" = "Hispanic",
  "99" = "Unknown"
)

data$RaceEthn <- raceethn_mapping[as.character(data$RaceEthn)]
data <- data %>%
  filter(RaceEthn != "Unknown")

# filter for neglect-only removals
neglect_only_data <- data %>%
  filter(
    Neglect == 1,
    PhyAbuse == 0,
    SexAbuse == 0,
    AAParent == 0,
    DAParent == 0,
    AAChild == 0,
    DAChild == 0,
    ChilDis == 0,
    ChBehPrb == 0,
    PrtsDied == 0,
    PrtsJail == 0,
    NoCope == 0,
    Abandmnt == 0,
    Relinqsh == 0,
    Housing == 0
  )

# group by total neglect-only removals per year and race
neglect_removals_summary <- neglect_only_data %>%
  group_by(FY, RaceEthn) %>%
  summarise(TotalNeglectRemovals = n()) %>%
  ungroup()

# get total kids per year and race 
total_kids_summary <- data %>%
  group_by(FY, RaceEthn) %>%
  summarise(TotalKids = sum(NumPlep, na.rm = TRUE)) %>%
  ungroup()

merged_summary <- neglect_removals_summary %>%
  left_join(total_kids_summary, by = c("FY", "RaceEthn"))

# get proportion
merged_summary <- merged_summary %>%
  mutate(Proportion = TotalNeglectRemovals / TotalKids)

# plot proportion over time
ggplot(merged_summary, aes(x = FY, y = Proportion, color = RaceEthn, group = RaceEthn)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +
  labs(
    title = "Proportion of Neglect-Only Removals Over Time by Race/Ethnicity",
    x = "Year",
    y = "Proportion of Removals",
    color = "Race/Ethnicity"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

