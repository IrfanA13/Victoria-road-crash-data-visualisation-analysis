if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scales)

setwd("~/Documents/Macquarie_term_4/Dataviz_8090/SecondAssignment")

# Load dataset
crash <- read_csv("vic_road_crash_data.csv", show_col_types = FALSE)

# Clean and transform
df <- crash %>%
  select(DEG_URBAN_NAME, FATALITY, SERIOUSINJURY, OTHERINJURY, NONINJURED) %>%
  filter(!is.na(DEG_URBAN_NAME)) %>%                                 # Drop NA values
  mutate(
    DEG_URBAN_NAME = recode(DEG_URBAN_NAME, "MELB_URBAN" = "MELBOURNE_URBAN"),  # Capitalise
    across(c(FATALITY, SERIOUSINJURY, OTHERINJURY, NONINJURED), as.numeric)
  ) %>%
  pivot_longer(
    cols = c(FATALITY, SERIOUSINJURY, OTHERINJURY, NONINJURED),
    names_to = "CRASH_SEVERITY",
    values_to = "Count"
  ) %>%
  group_by(DEG_URBAN_NAME, CRASH_SEVERITY) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(DEG_URBAN_NAME) %>%
  mutate(Percent = Total / sum(Total)) %>%
  ungroup()

# Optional: order categories logically (urban → rural)
df$DEG_URBAN_NAME <- factor(df$DEG_URBAN_NAME,
                            levels = c("MELBOURNE_CBD", "MELBOURNE_URBAN", "LARGE_PROVINCIAL_CITIES",
                                       "SMALL_CITIES", "TOWNS", "SMALL_TOWNS", "RURAL_VICTORIA")
)

# Plot
ggplot(df, aes(x = DEG_URBAN_NAME, y = Percent, fill = CRASH_SEVERITY)) +
  geom_col(color = "white") +
  geom_text(
    data = subset(df, Percent >= 0.06),
    aes(label = paste0(Total, "\n(", percent(Percent, accuracy = 1), ")")),
    position = position_stack(vjust = 0.5),
    size = 3.2
  ) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_fill_manual(
    values = c(
      "FATALITY" = "#F28E8E",       # soft coral
      "SERIOUSINJURY" = "#FFD580",  # light peach
      "OTHERINJURY" = "#FFF2B2",    # pale yellow
      "NONINJURED" = "#A6D8FF"      # baby blue
    ),
    name = "Crash Severity",
    labels = c("Fatality", "Serious Injury", "Other Injury", "Not Injured")
  ) +
  labs(
    title = "Crash Severity Distribution by Urban–Rural Category in Victoria",
    x = "Urban–Rural Category",
    y = "Percentage of Total Crashes (%)",

  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(face = "bold")
  )

