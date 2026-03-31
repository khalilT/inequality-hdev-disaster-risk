---
editor_options: 
  markdown: 
    wrap: 72
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

(c) Khalil Teber 2024-2025 \# Inequality in human development amplifies
    climate-related disaster risk

<!-- badges: start -->

<!-- badges: end -->

## Structure of the repository

This repository contains all the scripts necessary to generate all main
results and supplementary materials. Scripts used for data preprocessing
are also provided to document the full data-generation workflow.

```         
├── README.md
├── analysis                                   <- all scripts used to generate main results and supplementary materials
├── data
│   ├── raw_data                               <- raw data
│   └── intermediate_data                      <- processed data 
│   ├── input                                  <- data used for the analysis
│   └── output                                 <- data produced from analysis
├── figures
│   ├── main figures
│   ├── supplementary_figures
├── preprocess_01_aggregate_climate_variables  <- code needed for aggrgating the climate variables to geocoded events
│   ├── src                         
├── preprocess_02_create_analysis_data         <- code needed for joining disaster, climate and socioeconomic data
│   ├── src                         
```

## Table of Contents

-   [Climate aggregation](#(Preprocessing)-Climate-aggregation)
-   [Create analysis data](#(Preprocessing)-Create-analysis-data)
-   [Analysis](#Analysis)

## Reproducibility

All results presented in the manuscript can be reproduced
deterministically using the scripts provided in this repository,
together with EM-DAT data obtained directly from the original source.

The recommended reproduction workflow is:

1.  Run `analysis/0_create_dataframe.R` to merge the supplied data with
    EM-DAT and create the data used for the analysis. output:
    `data/input/disaster_information.csv`, the dataframe used for the
    analysis.

2.  Run `analysis/0_list_functions.R` and `analysis/0_plot_settings.R`
    to load needed functions and plot helper functions, themes, color
    palettes.

3.  Run `analysis/1_calculate_OR.R` to perform the odds ratio analysis.
    output: csv files with OR calculation and significance testing saved
    to `data/output/odds_sHDI_disasters_df.csv` and
    `data/output/odds_disasters_inequalities_df.csv`

4.  Run `analysis/2_generate_main_figures.R` to generate the main result
    figures output: 8 main figures presented in the main text saved to
    `figures/main_figures/...`

5.  Run `analysis/3_generate_supplementary_figures.R` to generate the
    supplementary figures. output: 22 supplementary figures presented in
    the supplements saved to `figures/supplementary_figures/...`

The workflow can be executed sequentially using `main_script.R`, which
automates all steps from data integration to figure generation.

To reproduce the Superposed Epoch Analysis (SEA), run the Python script
`analysis/1_climate_fingerprints.py`. The output is saved to the csv
files `/data/input/disaster_types_hdi_sd_climatology.csv` and
`data/input/disaster_types_sd_climatology.csv`. The supplementary tables
are created using the notebook: `4_supplementary_tables_S1_S21.Rmd`

The preprocessing code is provided to document the full data-generation
workflow and ensure methodological transparency. While executing the
complete preprocessing pipeline requires substantial computational
resources and access to large climate datasets, the workflow is fully
specified.

### (Preprocessing)-Climate-aggregation

We aggregate the climate variables in three different dimensions
(original units, anomalies and zscored anomalies), to obtain time series
that summarize the climatological conditions before during and after
each event. We use mostly global ERA5 data (0.25deg), and one variable
from GLEAM regridded to ERA5 resolution.

### (Preprocessing)-Create-analysis-data

In this step, we add the remaining ancillary information needed to the
analysis: the subnational human development index (sHDI) and its
components, population count and GDP of impacted regions. We keep only
the records that have all information essential to the analysis
available.

### Analysis {#analysis}

In this folder you will find the scripts used to generate all the
results of the paper as well as all figures and tables (main results and
supplementary materials).

### Data

We supply the data needed to generate the results, and some of the data
needed for the preprocessing steps. As redistributing EM-DAT data is not
allowed, users should download EM-DAT data independently and place the
file in `data/raw_data`, specifying the corresponding file path at the
beginning of `analysis/0_create_dataframe.R`. Administrative boundary
geometries in `data/input/gdl_map_simplified.gpkg` are derived from the
Global Data Lab (globaldatalab.org, data accessed August 2025) and are
redistributed here for non-commercial use in accordance with GDL terms
of use (globaldatalab.org/termsofuse).

### Figures

All generated figures will be saved in Figures, main or
supplementary_figures subfolders. Code used to generate new figures
during peer-review process was added to the corresponding scripts
`analysis/2_generate_main_figures.R` and
`analysis/3_generate_supplementary_figures.R`

### Notes

Code added during the peer-review process is integrated directly into the figure-generation scripts.


### Session info

To ensure reproducibility, the R session info used to generate the
results is provided below.

R version 4.5.2 (2025-10-31) Platform: x86_64-pc-linux-gnu Running
under: Ubuntu 24.04.3 LTS

Matrix products: default BLAS:
/usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 LAPACK:
/usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;
LAPACK version 3.12.0

locale: [1] LC_CTYPE=en_US.UTF-8 LC_NUMERIC=C LC_TIME=en_US.UTF-8
LC_COLLATE=en_US.UTF-8 LC_MONETARY=en_US.UTF-8\
[6] LC_MESSAGES=en_US.UTF-8 LC_PAPER=en_US.UTF-8 LC_NAME=C LC_ADDRESS=C
LC_TELEPHONE=C\
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C

time zone: Europe/Berlin tzcode source: system (glibc)

attached base packages: [1] parallel stats graphics grDevices utils
datasets methods base

other attached packages: [1] rlang_1.1.6 rnaturalearthdata_1.0.0
rnaturalearth_1.0.1 sf_1.0-21 patchwork_1.3.0 egg_0.4.5\
[7] gridExtra_2.3 energy_1.7-12 ggpmisc_0.6.1 ggpp_0.5.9 quantreg_6.1
SparseM_1.84-2\
[13] cowplot_1.1.3 xtable_1.8-4 ggpubr_0.6.1 gghighlight_0.5.0
ggforce_0.5.0 ggh4x_0.3.1\
[19] ggstream_0.1.0 ggridges_0.5.6 tidytext_0.4.3 lubridate_1.9.4
forcats_1.0.0 stringr_1.5.1\
[25] dplyr_1.1.4 purrr_1.0.4 readr_2.1.5 tidyr_1.3.1 tibble_3.3.0
ggplot2_3.5.2\
[31] tidyverse_2.0.0
