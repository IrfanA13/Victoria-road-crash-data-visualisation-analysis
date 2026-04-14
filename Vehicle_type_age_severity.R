if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scales)

vehicle <- read_csv("vehicle.csv", show_col_types = FALSE)
person  <- read_csv("person.csv", show_col_types = FALSE)
crash   <- read_csv("vic_road_crash_data.csv", show_col_types = FALSE)

df <- vehicle %>%
  inner_join(person, by = c("ACCIDENT_NO","VEHICLE_ID")) %>%
  inner_join(crash,  by = "ACCIDENT_NO") %>%
  distinct(ACCIDENT_NO, VEHICLE_ID, .keep_all = TRUE) %>%
  group_by(ACCIDENT_NO) %>%
  mutate(n_veh_in_crash = n()) %>%         
  ungroup() %>%
  mutate(
    VEHICLE_YEAR_MANUF = as.numeric(VEHICLE_YEAR_MANUF),
    YEAR_GROUP = cut(
      VEHICLE_YEAR_MANUF,
      breaks  = c(1980, 1990, 2001, 2012, 2024),
      labels  = c("1980–1990","1991–2001","2002–2012","2013–2024"),
      right = TRUE, include.lowest = TRUE
    )
  ) %>%
  filter(!is.na(YEAR_GROUP), VEHICLE_YEAR_MANUF >= 1980, VEHICLE_YEAR_MANUF <= 2024) %>%
  pivot_longer(
    c(FATALITY, SERIOUSINJURY, OTHERINJURY, NONINJURED),
    names_to  = "CRASH_SEVERITY",
    values_to = "sev_count"
  ) %>%
  mutate(weight = sev_count / pmax(n_veh_in_crash, 1))

top9_types <- df %>%
  count(VEHICLE_TYPE_DESC, sort = TRUE) %>%
  slice_head(n = 9) %>%
  pull(VEHICLE_TYPE_DESC)

plot_df <- df %>%
  filter(VEHICLE_TYPE_DESC %in% top9_types) %>%
  mutate(
    CRASH_SEVERITY = factor(
      CRASH_SEVERITY,
      levels = c("FATALITY","SERIOUSINJURY","OTHERINJURY","NONINJURED"),
      labels = c("Fatal","Serious Injury","Other Injury","Not Injured")
    ),
    VEHICLE_TYPE_DESC = factor(VEHICLE_TYPE_DESC, levels = top9_types)
  ) %>%
  group_by(VEHICLE_TYPE_DESC, YEAR_GROUP, CRASH_SEVERITY) %>%
  summarise(w = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(VEHICLE_TYPE_DESC, YEAR_GROUP) %>%
  mutate(
    pct = w / sum(w),
    label_txt = if_else(pct < 0.04, "", paste0(scales::comma(round(w)), "\n(", scales::percent(pct, 1), ")"))
  ) %>%
  ungroup()

# pastel palette
pastels <- c(
  "Fatal"          = "#F6B8B8",  # soft rose
  "Serious Injury" = "#BFD6F6",  # baby blue
  "Other Injury"   = "#FBE8A6",  # pale yellow
  "Not Injured"    = "#BFE3BF"   # mint
)

ggplot(plot_df, aes(YEAR_GROUP, pct, fill = CRASH_SEVERITY)) +
  geom_col(color = "white", linewidth = 0.3) +
  geom_text(aes(label = label_txt),
            position = position_stack(vjust = 0.5),
            size = 3, lineheight = 0.9) +
  facet_wrap(~ VEHICLE_TYPE_DESC, ncol = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = pastels, name = "Crash Severity") +
  labs(
    title    = expression(bold("Crash severity by Vehicle Type and Manufacture Period 1980 - 2024")),
    subtitle = "Vehicle Type with most frequent accidents displayed (Top 9)",
    x = "Year of Vehicle Manufacture",
    y = "Proportion of Crashes"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text  = element_text(face = "bold", size = 10)
  )




