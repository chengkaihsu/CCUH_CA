This project contains six R scripts for analyzing road traffic crashes. The scripts include codes for descriptive statistics, mapping, and modeling of temperature and crash data. Each script is briefly described below.
📁 Script Overview
1. 01_descriptives_table.R
This script generates descriptive summaries of road crash data across multiple stratifications (e.g., severity, collision type, vehicle type at fault, time of day, and driver sex). It includes multiple filtered subsets to support different summary tables for analysis or manuscript reporting.

2. 01_descriptive_vis.R
This script produces basic descriptive visualizations, including bar plots, for variables such as the sex of the at-fault driver. It focuses on visual summaries for crash severity and involvement characteristics.

3. 01_mapping_temp.R
This script maps annual mean temperatures across California cities using PRISM climate data. It processes the data to summarize average temperatures per city and creates geographic visualizations using ggplot2 and maps.

4. 01_mapping_crash.R
This script calculates and maps annual crash rates per 100,000 population for each city and year. It generates city-level visualizations of crash burden using spatial coordinates and population-adjusted metrics.

5. 02_analysis_main.R
This script estimates the main effects of temperature on daily crash counts. It defines severity categories, filters the crash dataset accordingly, and computes daily crash counts by city. It likely supports regression modeling for time-series analyses of temperature-crash associations.

6. 03_analysis_stratified.R
This script performs stratified analyses by crash or victim characteristics, such as crash type or road user category. It defines a detailed filter list to allow subgroup-specific modeling or summary statistics, supporting effect heterogeneity assessments.

