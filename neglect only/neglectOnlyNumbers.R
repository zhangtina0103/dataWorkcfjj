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
    Housing == 0,
    RaceEthn != "Unknown"
  )

# group by year (FY) and RaceEthn
# then summarize total number of removals
neglect_by_race_year <- neglect_only_data %>%
  group_by(FY, RaceEthn) %>%
  summarise(TotalRemovals = n()) %>%
  ungroup()

# plot trend over years
ggplot(neglect_by_race_year, aes(x = FY, y = TotalRemovals, color = RaceEthn, group = RaceEthn)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +
  labs(
    title = "Neglect-Only Removals Over Time by Race/Ethnicity",
    x = "Year",
    y = "Total Number of Removals",
    color = "Race/Ethnicity"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

