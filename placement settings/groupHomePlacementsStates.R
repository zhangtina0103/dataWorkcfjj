library(tidyverse)
library(ggrepel)

group_home_trends <- data.frame()

for (year in 2011:2021) {
  filepath <- paste0("~/Downloads/", year, "_ps_race.csv")
  
  if (file.exists(filepath)) {
    data <- read_csv(filepath, show_col_types = FALSE)
    
    group_home <- data %>%
      filter(CurPlSet == 4) %>%
      group_by(St) %>%
      summarise(GroupHomePlacements = n()) %>%
      mutate(Year = year)
    
    group_home_trends <- bind_rows(group_home_trends, group_home)
  }
}

ma_2021_value <- group_home_trends %>%
  filter(St == "MA", Year == 2021) %>%
  pull(GroupHomePlacements)

# cind states with 2021 values > MA
highlight_states <- group_home_trends %>%
  filter(Year == 2021, GroupHomePlacements > ma_2021_value) %>%
  pull(St)

label_data <- group_home_trends %>%
  filter(St %in% highlight_states, Year == 2021)

ma_data <- group_home_trends %>% filter(St == "MA")


ggplot(group_home_trends, aes(x = Year, y = GroupHomePlacements, group = St)) +
  geom_line(color = "grey80", alpha = 0.6) +
  
  # red line for MA
  geom_line(data = ma_data, color = "red", size = 1.2) +
  
  geom_point(data = label_data, aes(color = St), size = 2) +
  
  geom_text_repel(data = label_data,
                  aes(label = St, color = St),
                  nudge_x = 0.6,
                  hjust = 0,
                  direction = "y",
                  segment.color = "grey50",
                  show.legend = FALSE) +
  
  scale_x_continuous(limits = c(2011, 2022), breaks = 2011:2021) +
  labs(
    title = "Group Home Placements by State (2011–2021)",
    x = "Year",
    y = "Number of Group Home Placements"
  ) +
  theme_minimal() +
  theme(legend.position = "none")