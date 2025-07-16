crashes_filtered <- crashes_enriched # specify stratum
crashes_filtered <- crashes_enriched[crashes_enriched$COLLISION_SEVERITY %in% c(3,4)] # specify stratum
crashes_filtered <- crashes_enriched[crashes_enriched$TYPE_OF_COLLISION %in% c("F")] # specify stratum #A - Head-On B - Sideswipe C - Rear End D - Broadside E - Hit Object F - Overturned G - Vehicle/Pedestrian H - Other
crashes_filtered <- crashes_enriched[crashes_enriched$MVIW_CATEGORY %in% c("others")] # specify stratum
crashes_filtered <- crashes_enriched[!crashes_enriched$INTERSECTION %in% c("Y","N")] # specify stratum
crashes_filtered <- crashes_enriched[crashes_enriched[['TIME_OF_DAY']] %in% c("Nighttime")]

crashes_filtered <- crashes_enriched[crashes_enriched[['PCF_VIOL_CATEGORY']] %in% c("01","02","03","04","05","06","07","08","09","10",
                                                                                    "12","13","14","15","16","17","21","22","24")]
crashes_filtered <- crashes_enriched[crashes_enriched[['PCF_VIOL_CATEGORY']] %in% c("11")]
crashes_filtered <- crashes_enriched[crashes_enriched[['PCF_VIOL_CATEGORY']] %in% c("18")]
crashes_filtered <- crashes_enriched[!crashes_enriched$PCF_VIOL_CATEGORY	 %in% c("01","02","03","04","05","06","07","08","09","10",
                                                                                 "12","13","14","15","16","17","21","22","24",
                                                                                 "11",
                                                                                 "18")] # specify stratum

crashes_filtered <- crashes_enriched[crashes_enriched[['STWD_VEHTYPE_AT_FAULT_CATEGORY']] %in% c("non-specified")]

crashes_filtered <- crashes_enriched[!crashes_enriched$AT_FAULT_SEX %in% c("M","F")] # specify stratum
crashes_filtered <- crashes_enriched[!crashes_enriched$STWD_VEHTYPE_AT_FAULT %in% c("L","N",
                                                                                   "A","B","C","D","E","F","G","H","I","J","M","O")] # specify stratum


nrow(crashes_filtered)
crashes_filtered <- crashes_enriched[crashes_enriched[['PCF_VIOL_CATEGORY']] %in% c("24")]
#######################################################
crash_counts <- crashes_filtered[, .N, by = .(COLLISION_DATE, CITY)]
setnames(crash_counts, "N", "CRASH_COUNT")
combinations <- CJ(
  COLLISION_DATE = unique(crashes$COLLISION_DATE),
  CITY = unique(crashes$CITY)
)


merge_by <- c("COLLISION_DATE", "CITY")

city_daily_series <- merge(
  combinations, 
  crash_counts, 
  by = merge_by, 
  all.x = TRUE
)

# Fill missing values with zero
city_daily_series[is.na(CRASH_COUNT), CRASH_COUNT := 0]
length(unique(city_daily_series[['CITY']]))



# Convert COLLISION_DATE and date columns to datetime format
city_daily_series[, COLLISION_DATE := as.Date(COLLISION_DATE)]
prism_geo_no_geom[, date := as.Date(date)]

# Standardize city name formats (e.g., uppercase)
city_daily_series[, CITY := toupper(CITY)]
prism_geo_no_geom[, NAME := toupper(NAME)]

daily_RTI_PRISM_main <- merge(
  city_daily_series,
  prism_geo_no_geom,
  by.x = c("CITY", "COLLISION_DATE"),
  by.y = c("NAME", "date"),
  all.x = TRUE
)


daily_RTI_PRISM_main[, year := year(COLLISION_DATE)]
daily_RTI_PRISM_main[, month := month(COLLISION_DATE)]
daily_RTI_PRISM_main[, dow := lubridate::wday(COLLISION_DATE, label = TRUE, abbr = FALSE)]
daily_RTI_PRISM_main <- daily_RTI_PRISM_main[!grepl("^2020", year)]
daily_RTI_PRISM_main[, stratum := factor(paste(CITY, year, month, dow, sep = ":"))]
daily_RTI_PRISM_main[, keep := sum(CRASH_COUNT) > 0, by = stratum]
daily_RTI_PRISM_main[, CRASH_RATE := (CRASH_COUNT / SE_A00001_) * 100000]

daily_RTI_PRISM_main

summary_stats <- daily_RTI_PRISM_main %>% 
  group_by(CITY) %>% 
  summarise(
    across(
      c(
        CRASH_RATE
      ), 
      ~ mean(.x, na.rm = TRUE), 
      .names = "mean_{col}"
    ),
    across(
      c(
        CRASH_COUNT
      ), 
      ~ 100* mean(.x, na.rm = TRUE), 
      .names = "mean_{col}"
    ),
    across(
      c(tmean, ppt), 
      ~ mean(.x, na.rm = TRUE), 
      .names = "mean_{col}"
    ),
    
    .groups = "drop"
  )


summary_across_rows <- summary_stats %>%
  summarise(
    across(
      -CITY,
      list(
        mean = ~mean(.x, na.rm = TRUE),
        std = ~sd(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE),
        q1 = ~quantile(.x, probs = 0.25, na.rm = TRUE),
        q3 = ~quantile(.x, probs = 0.75, na.rm = TRUE)
      ),
      .names = "{col}_{fn}"
    )
  )


# Pivot to long format first
summary_long <- summary_across_rows %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "statistic"),
    names_pattern = "(.*)_(mean|std|median|q1|q3|min|max)$",
    values_to = "value"
  )

# Pivot to wide format: variables as rows, statistics as columns
summary_wide <- summary_long %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

summary_wide <- summary_wide %>%
  mutate(iqr = q3 - q1)

# View the reshaped summary
print(summary_wide)

# write.csv(summary_wide, "summary_across_rows.csv", row.names = FALSE)
