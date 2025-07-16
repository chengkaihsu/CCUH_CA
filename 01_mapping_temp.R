library(data.table)
library(ggplot2)
library(maps)

# Step 1: Ensure data.table format
setDT(daily_RTI_PRISM_main)

# Step 2: Calculate annual mean temperature per city
annual_temp <- daily_RTI_PRISM_main[, .(
  annual_mean_temp = mean(tmean_popwt, na.rm = TRUE),
  city_lat = first(city_lat),
  city_long = first(city_long)
), by = CITY]

# Step 3: Get California state and county maps
ca_state_map <- map_data("state", region = "california")
ca_county_map <- map_data("county", region = "california")

# Step 4: Plot
ggplot() +
  # State boundary
  geom_polygon(data = ca_state_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "black") +
  # County boundaries
  geom_path(data = ca_county_map, aes(x = long, y = lat, group = group),
            color = "gray60", linewidth = 0.3) +
  # Points for cities
  geom_point(
    data = annual_temp,
    aes(x = city_long, y = city_lat, fill = annual_mean_temp),
    shape = 21,
    color = "black",
    size = 2.5,
    stroke = 0.4
  ) +
  scale_fill_viridis_c(name = "Annual Mean Temp (°C)", option = "plasma") + 

  coord_fixed(1.3) +
  labs(
    title = "Annual Mean Temperature by City in California",
    subtitle = "Using PRISM-derived daily temperature (point centroids)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.75),           # Bottom-right corner
    legend.background = element_rect(fill = "white", color = "gray80")
  )
