if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggmap, scales)

register_google(key = "AIzaSyDfAtP9-mYReHdJz3BmOH4ljjKJnTxq4VY")

crash <- readr::read_csv("vic_road_crash_data.csv", show_col_types = FALSE)

df <- crash %>%
  rename(lon = LONGITUDE, lat = LATITUDE) %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  filter(lon >= 140, lon <= 150, lat >= -39.5, lat <= -33.5)

basemap <- get_map(location = c(lon = 145.0, lat = -37.8), zoom = 9, maptype = "terrain")

dens <- ggplot_build(
  ggplot(df, aes(lon, lat)) + stat_density_2d(geom = "polygon")
)$data[[1]]

qs <- unique(quantile(dens$level, probs = seq(0, 1, length.out = 6), na.rm = TRUE))
if (length(qs) < 6) {
  qs <- seq(min(dens$level, na.rm = TRUE), max(dens$level, na.rm = TRUE), length.out = 6)
}

dens <- dens %>%
  mutate(DensityLevel = cut(level, breaks = qs, include.lowest = TRUE,
                            labels = c("Very Low", "Low", "Medium", "High", "Very High")))

# Points to mark on the map (edit/add as you like)
spots <- tibble::tribble(
  ~name,            ~lon,      ~lat,
  "Melbourne CBD",   144.9631, -37.8136,
  "Dandenong",       145.2150, -37.9880,
  "Ringwood",        145.2270, -37.8160,
  "Werribee",        144.6610, -37.8960,
  "Frankston",       145.1220, -38.1446,
  "Sunshine",        144.8320, -37.7800
)

p_cat <- ggmap(basemap) +
  geom_polygon(data = dens,
               aes(x = x, y = y, group = group, fill = DensityLevel),
               alpha = 0.55) +
  scale_fill_manual(
    values = c(
      "Very High" = "#2c0078",
      "High"      = "#6600a7",
      "Medium"    = "#b24fb5",
      "Low"       = "#f9a56c",
      "Very Low"  = "#fdf0a6"
    ),
    name = "Crash Density"
  ) +
  # markers + labels
  geom_point(data = spots, aes(x = lon, y = lat),
             size = 2.8, shape = 21, fill = "white", color = "black", stroke = 0.6) +
  geom_text(data = spots, aes(x = lon, y = lat, label = name),
            nudge_y = 0.06, size = 3.2, fontface = "bold") +
  labs(
    title = "Crash Density Heatmap Victoria",
    subtitle = "Focused on Melbourne and surrounding areas",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 15),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "right")

print(p_cat)






