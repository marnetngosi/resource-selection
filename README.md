# Resource Selection and Spatial Memory in Translocated Elephants

![License](https://img.shields.io/badge/license-MIT-green.svg)  
This repository contains code, data structure, and documentation for analyzing resource selection in translocated elephants using **integrated Step Selection Functions (iSSFs)**.

## Overview

This study explores habitat and landscape features that influence elephant movememnt decisions in a new landscape following translocation. Using GPS collar data and environmental covariates, we applied iSSFs to assess seasonal and diel resource selection.

Key objectives:
- Quantify seasonal and diel resource selection patterns.

## Repository Structure
 project-root
├── data/ # Cleaned GPS and annotated step data
│ ├── cleaned_gps_data.csv
│ └── environmental_covariates/ # Raster and shapefile inputs
├── scripts/ # R scripts for preprocessing and modeling
│ ├── 01_prepare_data.R
│ ├── 02_fit_issf_models.R
│ ├── 03_model_selection.R
│ └── 04_plot_effects.R
├── outputs/ # Model objects and figures
│ ├── model_results/
│ └── figures/
├── docs/
│ ├── ISSF_method_details.pdf
│ └── references.bib
└── README.md


## Methods

- **Data source**: GPS collar data from translocated elephants (4–6 hr fix intervals).
- **Step generation**: `amt::steps_by_burst()` used to create used/available steps.
- **Distributions**:
  - **Gamma** for step lengths
  - **von Mises** for turning angles
- **Modeling**: Conditional logistic regression using `survival::clogit()`.
- **Covariates**:
  - Distance to fence, Distance to nearest water, Slope,Density of large trees, Canopy height and Basal Area
  - Time of day (day/night), season, and year
    ## Data Availability
Due to ethical and conservation concerns, the GPS collar data used in this study cannot be shared publicly. The repository includes all analysis scripts and instructions, and can be run with user-supplied data of similar format.

## Dependencies

To reproduce the analysis, install the following R packages:

```r
install.packages(c("amt", "survival", "ggplot2", "dplyr", "sf", "raster", "terra", "lubridate"))
