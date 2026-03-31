## Aggregation of climate variables to EM-DAT events

This repository contains the R and Python scripts needed to generate the main results and supplementary figures and tables.

### Requirements
python packages: xarray, numpy, geopandas, pandas, multiprocessing
R packages needed for the analysis are indicated in main_script.R

### Overview

#### main_script.R


This is the main script to access / generate the results.
Use this script to run 1_calculate_OR.R, 2_generate_main_figures.R & 3_generate_supplementary_figures.R


#### 0_list_functions.R

This R script contains all functions used in the analysis and needed for plotting the data.

#### 0_plot_settings.R

This R script contains all plot settings (color palette, themes) needed for the plots.

#### 1_calculate_OR.R

This R script is for the calculation of the odds ratio or the likelihood of being impacted (affected, killed, damaged).

#### 1_climate_fingerprints.ipynb.py

This python script is used to create the data necessary for the climate fingerprints per disaster type and per disaster type and sHDI group.

#### 2_generate_main_figures.R

This script is used for generating the figures of the main results.

#### 3_generate_supplementary_figures.R

This script is used for generating the figures of the supplementary materials.

#### 4_supplementary_tables_S1_S21.Rmd

This R notebook is for generating and visualizing the supplementary tables.

### How to Run the Scripts

Ensure that the required input files are available in the specified directories.
Run the scripts in the src folder to prepare the needed intermediate data.
Run each script in the appropriate order indicated in the names.

### Notes

