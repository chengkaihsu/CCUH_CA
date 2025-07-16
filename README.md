# Road Traffic Crash and Temperature Analysis

This project contains six R scripts for analyzing the association between temperature conditions and road traffic crashes. The scripts include code for descriptive statistics, mapping, and modeling of temperature and crash data. Each script is briefly described below.

## 📁 Script Overview

### 1. `01_descriptives_table.R`
Generates descriptive summaries of road crash data across multiple stratifications (e.g., severity, collision type, vehicle type at fault, time of day, and driver sex). Includes multiple filtered subsets to support summary tables for analysis or manuscript reporting.

### 2. `01_descriptive_vis.R`
Produces basic descriptive visualizations, including bar plots, for variables such as the sex of the at-fault driver. Focuses on visual summaries for crash severity and involvement characteristics.

### 3. `01_mapping_temp.R`
Maps annual mean temperatures across California cities using PRISM climate data. Processes the data to summarize average temperatures per city and creates geographic visualizations using `ggplot2` and `maps`.

### 4. `01_mapping_crash.R`
Calculates and maps annual crash rates per 100,000 population for each city and year. Generates city-level visualizations of crash burden using spatial coordinates and population-adjusted metrics.

### 5. `02_analysis_main.R`
Estimates the main effects of temperature on daily crash counts. Defines severity categories, filters the crash dataset accordingly, and computes daily crash counts by city. Supports regression modeling for time-series analyses of temperature–crash associations.

### 6. `03_analysis_stratified.R`
Performs stratified analyses by crash or victim characteristics, such as crash type or road user category. Defines a detailed filter list to enable subgroup-specific modeling or summary statistics, supporting effect heterogeneity assessments.
