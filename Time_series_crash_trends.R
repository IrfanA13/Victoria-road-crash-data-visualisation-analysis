if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, scales)

crash <- readr::read_csv("vic_road_crash_data.csv", show_col_types = FALSE) %>%
  mutate(
    ACCIDENT_DATE = coalesce(dmy(ACCIDENT_DATE, quiet = TRUE),
                             ymd(ACCIDENT_DATE, quiet = TRUE))
  ) %>%
  filter(!is.na(ACCIDENT_DATE))

end_date <- floor_date(max(crash$ACCIDENT_DATE, na.rm = TRUE), "month")
start_date <- end_date %m-% years(3)

crash3y <- crash %>%
  filter(ACCIDENT_DATE >= start_date, ACCIDENT_DATE < end_date %m+% months(1)) %>%
  mutate(month = floor_date(ACCIDENT_DATE, "month"))

crash3y <- crash3y %>%
  rename(SEVERITY = SEVERITY) %>%
  mutate(
    SEVERITY = case_when(
      str_detect(SEVERITY, regex("fatal", ignore_case = TRUE)) ~ "Fatal accident",
      str_detect(SEVERITY, regex("serious", ignore_case = TRUE)) ~ "Serious injury accident",
      str_detect(SEVERITY, regex("other", ignore_case = TRUE)) ~ "Other injury accident",
      str_detect(SEVERITY, regex("non", ignore_case = TRUE)) ~ "Non injury accident",
      TRUE ~ SEVERITY
    ),
    SEVERITY = factor(
      SEVERITY,
      levels = c("Fatal accident", "Serious injury accident",
                 "Other injury accident", "Non injury accident")
    )
  )

by_sev <- crash3y %>%
  count(month, SEVERITY, name = "n")

totals <- by_sev %>%
  group_by(month) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(SEVERITY = "Total (All Severities)")

plot_df <- bind_rows(by_sev, totals) %>%
  mutate(
    is_total = SEVERITY == "Total (All Severities)",
    SEVERITY = fct_relevel(SEVERITY,
                           "Fatal accident", "Serious injury accident",
                           "Other injury accident", "Non injury accident",
                           "Total (All Severities)"
    )
  )

season_of <- function(d){
  m <- month(d)
  case_when(
    m %in% c(12,1,2) ~ "Summer",
    m %in% c(3,4,5)  ~ "Autumn",
    m %in% c(6,7,8)  ~ "Winter",
    TRUE             ~ "Spring"
  )
}

blocks <- tibble(
  start = seq(floor_date(start_date, "month"), end_date, by = "3 months")
) %>%
  mutate(
    end   = pmin(start %m+% months(3), end_date %m+% months(1)),
    mid   = start %m+% months(1) %m+% days(15),
    season = season_of(start)
  )

y_top <- max(plot_df$n, na.rm = TRUE)

years_label <- paste0("(", year(start_date), " - ",
                      year(start_date %m+% years(2)), ")")

quarter_breaks <- seq(floor_date(start_date, "quarter"),
                      end_date, by = "3 months")

ggplot() +
  geom_rect(
    data = blocks,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = season),
    alpha = 0.22, color = NA
  ) +
  geom_line(
    data = plot_df,
    aes(x = month, y = n, color = SEVERITY, linetype = SEVERITY),
    linewidth = 1
  ) +
  geom_point(
    data = filter(plot_df, SEVERITY != "Total (All Severities)"),
    aes(x = month, y = n, color = SEVERITY),
    size = 1.8
  ) +
  geom_text(
    data = blocks,
    aes(x = mid, y = y_top * 0.97, label = season),
    size = 3.6, fontface = "bold", color = "#222222"
  ) +
  labs(
    title = "Seasonal Trends in Road Crash Severity Across Victoria (2022–2024)",
    x = "Month (Quarterly Labels)",
    y = "Number of Crashes",
    color = "Severity + Total",
    linetype = "Severity + Total",
    fill = "Season"
  ) +
  scale_x_date(
    breaks = quarter_breaks,
    labels = function(x) paste0(year(x), " Q", quarter(x))
  ) +
  scale_linetype_manual(values = c(
    "Fatal accident" = "solid",
    "Serious injury accident" = "solid",
    "Other injury accident" = "solid",
    "Non injury accident" = "solid",
    "Total (All Severities)" = "dashed"
  )) +
  scale_color_manual(values = c(
    "Fatal accident" = "#1f77b4",
    "Serious injury accident" = "#d62728",
    "Other injury accident" = "#2ca02c",
    "Non injury accident" = "#ff7f0e",
    "Total (All Severities)" = "black"
  )) +
  scale_fill_manual(values = c(
    "Summer" = "#FFE08A",
    "Autumn" = "#FFC38A",
    "Winter" = "#B7D5FF",
    "Spring" = "#BDE7A1"
  )) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 11),
    legend.position = "right",
    plot.title = element_text(face = "bold"),   # <-- makes title bold
    plot.margin = margin(12, 16, 12, 12)
  )








