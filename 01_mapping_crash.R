library(data.table)
library(ggplot2)
library(maps)

# Step 1: Ensure data.table format
setDT(daily_RTI_PRISM_main)

city_year_stats <- daily_RTI_PRISM_main[, .(
  total_crashes = sum(CRASH_COUNT, na.rm = TRUE),
  population = first(SE_A00001_),
  city_lat = first(city_lat),
  city_long = first(city_long)
), by = .(CITY, year = year(COLLISION_DATE))]

# Compute annual crash rate per 100,000
city_year_stats[, crash_rate := (total_crashes / population) * 100000]

city_avg_rate <- city_year_stats[, .(
  mean_crash_rate = mean(crash_rate, na.rm = TRUE),
  city_lat = first(city_lat),
  city_long = first(city_long)
), by = CITY]

ggplot() +
  geom_polygon(data = ca_state_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "black") +
  geom_path(data = ca_county_map, aes(x = long, y = lat, group = group),
            color = "gray60", linewidth = 0.3) +
  geom_point(
    data = city_avg_rate,
    aes(x = city_long, y = city_lat, fill = mean_crash_rate),
    shape = 21,
    color = "black",
    size = 2.5,
    stroke = 0.4
  ) +
  scale_fill_viridis_c(name = "Avg Annual Crash Rate\n(per 100,000)", option = "plasma") +
  coord_fixed(1.3) +
  labs(
    title = "Average Annual Road Traffic Crash Rate by City in California",
    subtitle = "Crash rate = total crashes per 100,000 residents, averaged across years",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.75),
    legend.background = element_rect(fill = "white", color = "gray80")
  )
