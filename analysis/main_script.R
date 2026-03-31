### (c) Khalil Teber 2024-2025 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"


### This code performs all steps to generate results and figures.
### Execute the different parts of the script in sequence.

rm(list=ls())
# 1 - Install needed packages --------------------------------------------------------

# list of packages we need for the analytics
packages2use = c(
  # for data wrangling
  "tidyverse","tidytext",
  # to plot ridges with ggplot
  "ggridges","ggstream","ggh4x","ggforce","gghighlight","ggpubr",
  # for additional plot themes
  "xtable",
  # to export latex tables
  "cowplot",
  # for quantile regression
  "quantreg", "ggpmisc",
  # for distance correlation
  "energy",
  # to tag ggplots
  "egg",
  # to arrange plots
  "patchwork",
  # to handle vector data
  "sf",
  # to load coastlines abd boundaries
  "rnaturalearth","rnaturalearthdata",
  # for parallel computation
  "parallel")

for(p in packages2use){
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# 2 - load scripts --------------------------------------------------------

source("analysis/0_plot_settings.R") #plot settings
source("analysis/0_list_functions.R") #analysis and plot functions
#create input data
source("analysis/0_create_dataframe.R")

################################################
#
# 1 - Calculate Odds ratio
#
################################################

source("analysis/1_calculate_OR.R")


################################################
#
# 2 - Generate main figures
#
################################################

source("analysis/2_generate_main_figures.R")

################################################
#
# 3 - Generate supplementary figures
#
################################################

source("analysis/3_generate_supplementary_figures.R")
