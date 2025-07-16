# Recalculate percentages
percentages <- table(crashes_enriched$AT_FAULT_SEX) / 
  length(crashes_enriched$PEDESTRIAN_ACCIDENT) * 100



percentages_filtered <- percentages[!names(percentages) %in% c("")]
percentages_filtered <- percentages
# percentages_filtered <- percentages_filtered[order(as.numeric(names(percentages_filtered)))]


# Plot with reordered x-axis
barplot(percentages_filtered,
        # main = "Percentage of Severity Levels",
        # xlab = "Victim Severity",
        ylab = "Percentage",
        col = "lightgreen",
        border = "white")
################################################################################################
# Recode MVIW_CATEGORY
crashes_enriched <- crashes_enriched %>%
  mutate(MVIW_CATEGORY = case_when(
    MVIW == "B" ~ "pedestrian",
    MVIW == "C" ~ "other motor vehicle",
    MVIW == "D" ~ "other motor vehicle \non other roadway",
    MVIW == "G" ~ "bicycle",
    MVIW == "I" ~ "fixed object",
    TRUE ~ "others"
  ))

# Recalculate percentages
percentages <- table(crashes_enriched$MVIW_CATEGORY) / 
  length(crashes_enriched$PEDESTRIAN_ACCIDENT) * 100

# Order percentages in descending order
percentages <- sort(percentages, decreasing = TRUE)

# Plot with reordered x-axis
barplot(percentages, 
        ylab = "Percentage", 
        col = "lightgreen", 
        border = "white",
        las = 1)

################################################################################################
# Recode STWD_VEHTYPE_AT_FAULT
crashes_enriched <- crashes_enriched %>%
  mutate(STWD_VEHTYPE_AT_FAULT_CATEGORY = case_when(
    STWD_VEHTYPE_AT_FAULT == "-" ~ "non-specified",
    STWD_VEHTYPE_AT_FAULT == "L" ~ "bicycle",
    STWD_VEHTYPE_AT_FAULT == "N" ~ "pedestrian",
    TRUE ~ "motor vehicle"
  ))

# Recalculate percentages
percentages <- table(crashes_enriched$STWD_VEHTYPE_AT_FAULT_CATEGORY) / 
  length(crashes_enriched$PEDESTRIAN_ACCIDENT) * 100

# Order percentages in descending order
percentages <- sort(percentages, decreasing = TRUE)

# Plot with reordered x-axis
barplot(percentages, 
        ylab = "Percentage", 
        col = "lightgreen", 
        border = "white")


################################################################################################
# INTERSECTION
# Recalculate percentages
percentages <- table(crashes_enriched$INTERSECTION) / 
  length(crashes_enriched$PEDESTRIAN_ACCIDENT) * 100

# Order percentages in descending order
percentages <- sort(percentages, decreasing = TRUE)

# Plot with reordered x-axis
barplot(percentages, 
        ylab = "Percentage", 
        col = "lightgreen", 
        border = "white")

crashes_enriched$PCF_VIOL_CATEGORY
################################################################################################
# Recode STWD_VEHTYPE_AT_FAULT
crashes_enriched <- crashes_enriched %>%
  mutate(PCF_VIOL_CATEGORY_CATEGORY = case_when(
    PCF_VIOL_CATEGORY == "01" ~ "DUI",
    PCF_VIOL_CATEGORY == "02" ~ "Impeding Traffic",
    PCF_VIOL_CATEGORY == "03" ~ "Unsafe speed",
    PCF_VIOL_CATEGORY == "07" ~ "Unsafe lane changing",
    PCF_VIOL_CATEGORY == "08" ~ "Improper turning",
    PCF_VIOL_CATEGORY == "09" ~ "Automobile right of way",
    PCF_VIOL_CATEGORY == "10" ~ "Pedestrian right of way",
    PCF_VIOL_CATEGORY == "11" ~ "Pedestrian violation",
    PCF_VIOL_CATEGORY == "12" ~ "Traffic signal and signs",
    PCF_VIOL_CATEGORY %in% c("04","05","06","13","14","15","16","17","21","22","24") ~ "Other driver-related",
    TRUE ~ "others"
  ))

# Recalculate percentages
percentages <- table(crashes_enriched$PCF_VIOL_CATEGORY_CATEGORY) / 
  length(crashes_enriched$PEDESTRIAN_ACCIDENT) * 100

# Order percentages in descending order
percentages <- sort(percentages, decreasing = TRUE)

# Plot with reordered x-axis
barplot(percentages, 
        ylab = "Percentage", 
        col = "lightgreen", 
        border = "white",
        cex.names =0.5)
