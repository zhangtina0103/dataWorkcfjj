library(tidyverse)

# can do same to analyze other races (just need to change the RaceEthn number)

data <- read_csv("~/Downloads/Filtered_MA_data.csv")

# mapping for numbers to placement setting types
curplset_map <- c(
  "1" = "Pre-adoptive home",
  "2" = "Foster home, relative",
  "3" = "Foster home, non-relative",
  "4" = "Group home",
  "5" = "Institution",
  "6" = "Supervised independent living",
  "7" = "Runaway",
  "8" = "Trial home visit"
)

# filter for Asians
filtered_data <- data %>%
  filter(RaceEthn == 4) %>%
  filter(CurPlSet != 99) %>%
  mutate(CurPlSet = as.character(CurPlSet),
         CurPlSet = recode(CurPlSet, !!!curplset_map)) %>%
  group_by(FY, CurPlSet) %>%
  summarise(Count = n(), .groups = 'drop')


ggplot(filtered_data, aes(x = FY, y = Count, color = CurPlSet)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Group Home Placements: Asians",
    x = "Year",
    y = "Number of Placements",
    color = "Placement Type"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("grey", "black", "darkblue", "darkred", "darkgreen", "darkorange", "brown", "purple")) + 
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )