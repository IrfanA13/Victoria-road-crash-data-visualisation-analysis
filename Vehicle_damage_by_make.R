if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scales)

vehicle <- read_csv("vehicle.csv", show_col_types = FALSE)

df <- vehicle %>%
  filter(!is.na(VEHICLE_MAKE), !is.na(LEVEL_OF_DAMAGE)) %>%
  mutate(
    DAMAGE_LEVEL = recode(as.character(LEVEL_OF_DAMAGE),
                          "6" = "Nil damage",
                          "1" = "Minor",
                          "2" = "Moderate (driveable vehicle)",
                          "3" = "Moderate (unit towed away)",
                          "4" = "Major (unit towed away)",
                          "5" = "Extensive (unrepairable)",
                          "9" = "Not known",
                          .default = NA_character_
    ),
    DAMAGE_LEVEL = factor(
      DAMAGE_LEVEL,
      levels = c("Nil damage","Minor","Moderate (driveable vehicle)",
                 "Moderate (unit towed away)","Major (unit towed away)",
                 "Extensive (unrepairable)")
    )
  ) %>%
  filter(!is.na(DAMAGE_LEVEL), DAMAGE_LEVEL != "Not known")

top_makes <- df %>%
  count(VEHICLE_MAKE, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(VEHICLE_MAKE)

df_top <- df %>%
  filter(VEHICLE_MAKE %in% top_makes) %>%
  group_by(VEHICLE_MAKE, DAMAGE_LEVEL) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(VEHICLE_MAKE) %>%
  mutate(pct = n / sum(n))

ggplot(df_top, aes(x = reorder(VEHICLE_MAKE, -pct), y = pct, fill = DAMAGE_LEVEL)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = ifelse(pct >= 0.04, scales::percent(pct, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    name = "Damage Level",
    values = c(
      "Nil damage"                     = "#d2fbd4",
      "Minor"                          = "#b6e3b5",
      "Moderate (driveable vehicle)"   = "#fedc97",
      "Moderate (unit towed away)"     = "#fdbf6f",
      "Major (unit towed away)"        = "#fb9a99",
      "Extensive (unrepairable)"       = "#e31a1c"
    )
  ) +
  labs(
    title = "Distribution of Vehicle Damage by Make (Top 10)",
    x = "Vehicle Make",
    y = "Proportion of Damage Level",
  
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )







