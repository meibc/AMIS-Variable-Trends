# Trends in AMIS variables (2015-2022)

This repository documents the data standardization process used to compare responses across survey years from the American Men’s Internet Survey (AMIS). The goal is to harmonize indicators related to stigma and HIV care engagement over time to enable valid trend analyses at the U.S. state level.

---

## Overview

AMIS survey items may vary across years due to differences in survey respondents. To enable valid cross-year comparisons, this repository:

1. Aligns variable names, response options, and definitions across AMIS waves (2015–2022)
2. Constructs population-representative weights using state-level MSM demographic targets (age, race, income, education) 
3. Produces weighted estimates for key stigma and care engagement indicators by state and survey period
4. Outputs a clean, long-format dataset of harmonized indicators ready for analysis or visualization

---

## Methodology

The data standardization pipeline involves the following steps:

### 1. **Variable Harmonization**

- Survey items were reviewed across years to identify changes in wording, response categories, or missing years.
- Harmonized versions of each key indicator (e.g., anticipated healthcare stigma, internalized stigma, PrEP use) were derived and documented.

### 2. **Target Population Calibration**

- Demographic targets were obtained at the state level. (see https://github.com/meibc/MSM-Population-Estimates) 
- Variables used for calibration included:  
  - Age group  
  - Race/ethnicity  
  - Income  
  - Educational attainment  

### 3. **Weighting via Iterative Proportional Fitting (IPF)**

- Using the [`ipfp`](https://cran.r-project.org/web/packages/ipfp/index.html) package in R, survey weights were generated to align the joint distribution of respondents with the target population margins.
- Weighting was conducted separately for each year group and demographic dimension.
- Final weights were used to estimate population-level means and standard errors for each indicator.

### 4. **Data Output**

- Cleaned, weighted results were reshaped to long format with `mean`, `standard error`, `state`, `indicator`, and `year_group` fields.
- These standardized estimates support longitudinal comparison of key stigma and care indicators.

---

## Key Files

- `.R` scripts: Code for data cleaning, harmonization, raking, and estimation  
- `AMIS-trend-visualization.qmd`: Quarto source file for the visualization 
- `AMIS-trend-visualization.html`: Rendered static HTML version of the dashboard  

---

## Optional Visualization

An interactive Quarto dashboard is included to explore the cleaned data:

- Select a U.S. state to view trends across survey waves
- Visualize confidence intervals (±1 SE) around weighted estimates
- Faceted display for multiple stigma dimensions

### To view the dashboard:

In RStudio:
- Open `AMIS-trend-visualization.qmd`
- Click **"Render with Shiny"**

Or from terminal:
```bash
quarto preview AMIS-trend-visualization.qmd
