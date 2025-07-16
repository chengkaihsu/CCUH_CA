severity_levels <- list(
  "all" = NULL,                     # All severities
  "severity_1" = c(1),
  "severity_2" = c(2),
  "severity_3_4" = c(3, 4)
)

compute_main_effects <- function(data, severity_vals = NULL, temp_var = "tmean_popwt") {
  # Filter by severity if specified
  if (!is.null(severity_vals)) {
    data <- data[COLLISION_SEVERITY %in% severity_vals]
  }
  
  # Prepare daily series
  crash_counts <- data[, .N, by = .(COLLISION_DATE, CITY)]
  setnames(crash_counts, "N", "CRASH_COUNT")
  
  combinations <- CJ(COLLISION_DATE = unique(crashes$COLLISION_DATE),
                     CITY = unique(crashes$CITY))
  
  city_daily_series <- merge(combinations, crash_counts, by = c("COLLISION_DATE", "CITY"), all.x = TRUE)
  city_daily_series[is.na(CRASH_COUNT), CRASH_COUNT := 0]
  city_daily_series[, COLLISION_DATE := as.Date(COLLISION_DATE)]
  
  # Merge with weather
  prism_geo[, date := as.Date(date)]
  prism_geo[, NAME := toupper(NAME)]
  city_daily_series[, CITY := toupper(CITY)]
  
  daily <- merge(city_daily_series, prism_geo[, !"geometry"], 
                 by.x = c("CITY", "COLLISION_DATE"), 
                 by.y = c("NAME", "date"), all.x = TRUE)
  
  daily[, year := year(COLLISION_DATE)]
  daily[, month := month(COLLISION_DATE)]
  daily[, dow := wday(COLLISION_DATE, label = TRUE)]
  daily <- daily[year != 2020]
  daily[, stratum := factor(paste(CITY, year, month, dow, sep = ":"))]
  daily[, keep := sum(CRASH_COUNT) > 0, by = stratum]
  
  # Stats
  temp_stats <- list(
    q01 = quantile(daily[[temp_var]], 0.01, na.rm = TRUE),
    median = median(daily[[temp_var]], na.rm = TRUE),
    q95 = quantile(daily[[temp_var]], 0.95, na.rm = TRUE),
    q99 = quantile(daily[[temp_var]], 0.99, na.rm = TRUE)
  )
  
  # Model
  knots_temp <- quantile(daily[[temp_var]], c(0.10, 0.75, 0.90), na.rm = TRUE)
  cbt <- crossbasis(daily[[temp_var]], lag = 2,
                    argvar = list(fun = "ns", knots = knots_temp),
                    arglag = list(knots = logknots(3, df = 3)))
  
  model <- gnm(CRASH_COUNT ~ cbt + ppt_popwt, eliminate = stratum,
               family = quasipoisson(), data = daily, subset = keep)
  
  pred <- crosspred(cbt, model, cum = TRUE, cen = temp_stats$q01, by = 0.1)
  
  # Extract RR and slope
  rr_99 <- pred$allRRfit[paste0(round(temp_stats$q99, 1))]
  rr_99_low <- pred$allRRlow[paste0(round(temp_stats$q99, 1))]
  rr_99_high <- pred$allRRhigh[paste0(round(temp_stats$q99, 1))]
  
  rr_95 <- pred$allRRfit[paste0(round(temp_stats$q95, 1))]
  slope <- (log(rr_99) - log(rr_95)) / (temp_stats$q99 - temp_stats$q95)
  
  return(list(
    RR = rr_99,
    RR_low = rr_99_low,
    RR_high = rr_99_high,
    Slope_95_99 = slope * 100
  ))
}

results_summary <- rbindlist(lapply(names(severity_levels), function(sev_name) {
  res <- compute_main_effects(crashes_enriched, severity_levels[[sev_name]])
  data.table(
    Severity = sev_name,
    RR = res$RR,
    RR_low = res$RR_low,
    RR_high = res$RR_high,
    Slope_95_99 = res$Slope_95_99
  )
}))

# fwrite(results_summary, "summary_main_effects_by_severity.csv")

