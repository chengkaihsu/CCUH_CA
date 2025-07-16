filter_list <- list(
  # MVIW_CATEGORY
  "Bicycle"   = list(column = "MVIW_CATEGORY", values = c("bicycle")),
  "Pedestrian"   = list(column = "MVIW_CATEGORY", values = c("pedestrian")),
  "Other motor vehicle" = list(column = "MVIW_CATEGORY", values = c("other motor vehicle")),
  "Other motor vehicle on other roadway" = list(column = "MVIW_CATEGORY", values = c("other motor vehicle \non other roadway")),
  "Fixed object"   = list(column = "MVIW_CATEGORY", values = c("fixed object")),
  
  # TYPE_OF_COLLISION - grouped
  "Multi-vehicle" = list(column = "TYPE_OF_COLLISION", values = c("A", "B", "C", "D")),
  "Single-vehicle" = list(column = "TYPE_OF_COLLISION", values = c("E", "F")),
  "Vehicle-pedestrian"   = list(column = "TYPE_OF_COLLISION", values = c("G")),
  
  # TYPE_OF_COLLISION - individual
  "Head-On" = list(column = "TYPE_OF_COLLISION", values = c("A")),
  "Sideswipe" = list(column = "TYPE_OF_COLLISION", values = c("B")),
  "Rear End"   = list(column = "TYPE_OF_COLLISION", values = c("C")),
  "Broadside"   = list(column = "TYPE_OF_COLLISION", values = c("D")),
  "Hit Object"   = list(column = "TYPE_OF_COLLISION", values = c("E")),
  "Overturned"   = list(column = "TYPE_OF_COLLISION", values = c("F")),
  
  # INTERSECTION
  "Intersection" = list(column = "INTERSECTION", values = c("Y")),
  "Non-intersection"   = list(column = "INTERSECTION", values = c("N")),
  
  # TIME_OF_DAY
  "Daytime" = list(column = "TIME_OF_DAY", values = c("Daytime")),
  "Nighttime"   = list(column = "TIME_OF_DAY", values = c("Nighttime")),
  
  # STWD_VEHTYPE_AT_FAULT_CATEGORY
  "Pedestrian at fault" = list(column = "STWD_VEHTYPE_AT_FAULT_CATEGORY", values = c("pedestrian")),
  "Bicycle at fault" = list(column = "STWD_VEHTYPE_AT_FAULT_CATEGORY", values = c("bicycle")),
  "Motor vehicle at fault"   = list(column = "STWD_VEHTYPE_AT_FAULT_CATEGORY", values = c("motor vehicle")),
  
  # PCF_VIOL_CATEGORY_CATEGORY - grouped
  "Pedestrian violation"   = list(column = "PCF_VIOL_CATEGORY_CATEGORY", values = c("Pedestrian violation")),
  "Driver-related violation" = list(column = "PCF_VIOL_CATEGORY_CATEGORY", values = c(
    "DUI", "Impeding Traffic", "Unsafe speed", "Unsafe lane changing", "Improper turning", 
    "Automobile right of way", "Pedestrian right of way", "Traffic signal and signs", "Other driver-related")),
  
  # PCF_VIOL_CATEGORY_CATEGORY - individual
  "DUI" = list(column = "PCF_VIOL_CATEGORY_CATEGORY", values = c("DUI")),
  "Impeding Traffic" = list(column = "PCF_VIOL_CATEGORY_CATEGORY", values = c("Impeding Traffic")),
  "Unsafe speed"   = list(column = "PCF_VIOL_CATEGORY_CATEGORY", values = c("Unsafe speed")),
  "Unsafe lane changing"   = list(column = "PCF_VIOL_CATEGORY_CATEGORY", values = c("Unsafe lane changing")),
  "Automobile right of way"   = list(column = "PCF_VIOL_CATEGORY_CATEGORY", values = c("Automobile right of way")),
  "Pedestrian right of way"   = list(column = "PCF_VIOL_CATEGORY_CATEGORY", values = c("Pedestrian right of way")),
  "Traffic signal and signs"   = list(column = "PCF_VIOL_CATEGORY_CATEGORY", values = c("Traffic signal and signs"))
)



# ---- Settings ----
crash_types <- c("all", "severe")
colors <- c("all" = "grey", "severe" = "purple")
shapes <- c("all" = 16, "severe" = 17)

# ---- Analysis Function ----
run_crash_temp_analysis <- function(temp_var, crash_type, filter_col, filter_vals) {
  message(sprintf("Running: %s in %s | temp = %s | crash = %s", 
                  filter_col, paste(filter_vals, collapse = ","), temp_var, crash_type))
  
  filtered <- copy(crashes_enriched)[get(filter_col) %in% filter_vals]
  if (crash_type == "severe") filtered <- filtered[COLLISION_SEVERITY %in% c(1, 2)]
  
  crash_counts <- filtered[, .N, by = .(COLLISION_DATE, CITY)]
  setnames(crash_counts, "N", "CRASH_COUNT")
  combinations <- CJ(COLLISION_DATE = unique(crashes$COLLISION_DATE),
                     CITY = unique(crashes$CITY))
  city_daily_series <- merge(combinations, crash_counts, by = c("COLLISION_DATE", "CITY"), all.x = TRUE)
  city_daily_series[is.na(CRASH_COUNT), CRASH_COUNT := 0]
  city_daily_series[, COLLISION_DATE := as.Date(COLLISION_DATE)]
  
  prism_geo[, date := as.Date(date)]
  prism_geo[, NAME := toupper(NAME)]
  city_daily_series[, CITY := toupper(CITY)]
  daily <- merge(city_daily_series, prism_geo[, !"geometry"], 
                 by.x = c("CITY", "COLLISION_DATE"), by.y = c("NAME", "date"), all.x = TRUE)
  
  daily[, `:=`(year = year(COLLISION_DATE), month = month(COLLISION_DATE), dow = wday(COLLISION_DATE, label = TRUE))]
  daily <- daily[year != 2020]
  daily[, stratum := factor(paste(CITY, year, month, dow, sep = ":"))]
  daily[, keep := sum(CRASH_COUNT) > 0, by = stratum]
  
  temp_stats <- list(
    q01 = quantile(daily[[temp_var]], 0.01, na.rm = TRUE),
    q50 = quantile(daily[[temp_var]], 0.50, na.rm = TRUE),
    q95 = quantile(daily[[temp_var]], 0.95, na.rm = TRUE),
    q99 = quantile(daily[[temp_var]], 0.99, na.rm = TRUE)
  )
  
  knots_temp <- quantile(daily[[temp_var]], c(0.10, 0.75, 0.90), na.rm = TRUE)
  cbt <- crossbasis(daily[[temp_var]], lag = 2, 
                    argvar = list(fun = "ns", knots = knots_temp), 
                    arglag = list(knots = logknots(3, df = 3)))
  
  model <- gnm(CRASH_COUNT ~ cbt + ppt_popwt, eliminate = stratum,
               family = quasipoisson(), data = daily, subset = keep)
  
  pred <- crosspred(cbt, model, cum = TRUE, cen = temp_stats$q01, by = 0.1)
  
  rr_95 <- pred$allRRfit[as.character(round(temp_stats$q95, 1))]
  rr_99 <- pred$allRRfit[as.character(round(temp_stats$q99, 1))]
  slope_95_99 <- (log(rr_99) - log(rr_95)) / (temp_stats$q99 - temp_stats$q95)
  
  list(
    RR = rr_99,
    RR_low = pred$allRRlow[as.character(round(temp_stats$q99, 1))],
    RR_high = pred$allRRhigh[as.character(round(temp_stats$q99, 1))],
    Slope_95_99 = slope_95_99
  )
}

# ---- Run all combinations ----
results_all <- list()
for (filter_name in names(filter_list)) {
  filter_col <- filter_list[[filter_name]]$column
  filter_vals <- filter_list[[filter_name]]$values
  
  temp_vars_dynamic <- if (grepl("Daytime", filter_name)) {
    c("tmax_popwt")
  } else if (grepl("Nighttime", filter_name)) {
    c("tmin_popwt")
  } else {
    c("tmean_popwt")
  }
  
  for (tv in temp_vars_dynamic) {
    for (ct in crash_types) {
      res <- run_crash_temp_analysis(tv, ct, filter_col, filter_vals)
      results_all[[length(results_all) + 1]] <- data.table(
        Filter = filter_name,
        Temp = tv,
        Crash = ct,
        RR = res$RR,
        RR_low = res$RR_low,
        RR_high = res$RR_high,
        Slope_95_99 = res$Slope_95_99
      )
    }
  }
}

# ---- Combine ----
plot_data <- rbindlist(results_all)
plot_data[, Crash := factor(Crash, levels = c("all", "severe"))]


# Step 1: Copy and format RR
plot_data_fmt <- copy(plot_data)
plot_data_fmt[, RR_fmt := sprintf("%.2f (%.2f, %.2f)", RR, RR_low, RR_high)]
plot_data_fmt[, Slope_fmt := sprintf("%.2f", Slope_95_99 * 100)]  # slope in %/°C

# Step 2: Reshape to wide format with both RR and slope
make_wide_table_by_suffix <- function(temp_suffix) {
  dt <- plot_data_fmt[endsWith(Temp, temp_suffix), .(Filter, Crash, RR_fmt, Slope_fmt)]
  
  # Keep original filter order
  dt[, Filter := factor(Filter, levels = unique(plot_data$Filter))]
  
  # Create wide table: each row = Filter, columns = Crash type
  wide_rr    <- dcast(dt, Filter ~ Crash, value.var = "RR_fmt")
  wide_slope <- dcast(dt, Filter ~ Crash, value.var = "Slope_fmt")
  
  # Merge and rename columns
  final <- merge(wide_rr, wide_slope, by = "Filter", suffixes = c("_RR", "_Slope"), sort = FALSE)
  setcolorder(final, c("Filter", "all_RR", "severe_RR", "all_Slope", "severe_Slope"))
  
  return(final)
}


table_popwt       <- make_wide_table_by_suffix("popwt")

# fwrite(table_popwt, "table_popwt.csv")


