### (c) Khalil Teber 2024-2025 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"


### This code performs all steps to calculate Odds Ratio of impact likelihood and save results


library(tidyverse)
library(ggridges)#plot ridges
library(cowplot)#for ggplot2 themes
library(quantreg)#quantile regression
library(ggpmisc)#quantile reg stats
library(egg)#tag plots
library(patchwork)#arrange plots
library(parallel)
source("analysis/0_plot_settings.R")
source("analysis/0_list_functions.R")

# read data ---------------------------------------------------------------

disasters_df <- read_csv("data/input/disaster_information.csv")

# Calculate OR between sHDI groups ----------------------------------------

### across disaster types
# affected all
affected_all_df <- boot_odds_impact_all("normalized_affected")
# killed all
killed_all_df <- boot_odds_impact_all("normalized_deaths")
# Financial damages all
damages_all_df <- boot_odds_impact_all("normalized_total_damages")

### Likelihood affected, per disaster type
dis_list_affected <- as.list(unique(disasters_df$disaster_type))
dis_df_affected <- data.frame(disaster = unique(disasters_df$disaster_type))
dis_df_affected$case <- sapply(dis_list_affected, consider_disaster_affected)
dis_df_affected <- dis_df_affected %>% filter(case == 1)
dis_list <- as.list(dis_df_affected$disaster)
odds_glm <- lapply(dis_list, boot_odds_affected)
odds_glm_df <- do.call(rbind,odds_glm)

### Likelihood fatalities, per disaster type
dis_list_killed <- as.list(unique(disasters_df$disaster_type))
dis_df_killed <- data.frame(disaster = unique(disasters_df$disaster_type))
dis_df_killed$case <- sapply(dis_list_killed, consider_disaster_killed)
dis_df_killed <- dis_df_killed %>% filter(case == 1)
dis_list_killed <- as.list(dis_df_killed$disaster)
odds_glm_killed <- lapply(dis_list_killed, boot_odds_killed)
odds_glm_killed_df <- do.call(rbind,odds_glm_killed)

### Likelihood econ. losses, per disaster type
dis_list_damages <- as.list(unique(disasters_df$disaster_type))
dis_df_damages <- data.frame(disaster = unique(disasters_df$disaster_type))
dis_df_damages$case <- sapply(dis_list_damages, consider_disaster_damages)
dis_df_damages <- dis_df_damages %>%   filter(case == 1)
dis_list_damages <- as.list(dis_df_damages$disaster)
odds_glm_damages<- lapply(dis_list_damages, boot_odds_damages)
odds_glm_damages_df <- do.call(rbind,odds_glm_damages)

### Format and save results
affected_all_df$impact <- "Likelihood affected"
killed_all_df$impact <- "Likelihood fatalities"
damages_all_df$impact <- "Likelihood economic losses"

odds_glm_df$impact <- "Likelihood affected"
odds_glm_killed_df$impact <- "Likelihood fatalities"
odds_glm_damages_df$impact <- "Likelihood economic losses"

odds_disasters_df <- rbind(affected_all_df,odds_glm_df,
                           killed_all_df,odds_glm_killed_df,
                           damages_all_df,odds_glm_damages_df)
odds_disasters_df <- odds_disasters_df %>% 
  mutate(significance = case_when( pvalue > 0.05 ~ ".",
                                   pvalue <= 0.05 & pvalue > 0.01 ~ "* ",
                                   pvalue <= 0.01 & pvalue > 0.001 ~ " ** ",
                                   pvalue <= 0.001 ~ " *** "))

odds_disasters_df <- odds_disasters_df %>% 
  mutate(alpha_sig = case_when( pvalue > 0.05 ~ 0.8,
                                pvalue <= 0.05 & pvalue > 0.01 ~ 1,
                                pvalue <= 0.01 & pvalue > 0.001 ~ 1,
                                pvalue <= 0.001 ~ 1))
odds_disasters_df <- odds_disasters_df %>% 
  mutate(sHDI_category = str_trim(sHDI_category))

write_csv(odds_disasters_df, "data/output/odds_sHDI_disasters_df.csv")

rm(list = c("affected_all_df", "killed_all_df", "damages_all_df",
           "odds_glm", "odds_glm_df", "odds_glm_killed", "odds_glm_killed_df",
           "odds_glm_damages", "odds_glm_damages_df", "odds_disasters_df",
           "dis_list", "dis_list_affected", "dis_list_killed", "dis_list_damages",
           "dis_df_affected", "dis_df_killed", "dis_df_damages"))

# Calculate OR considering sHDI deviations ----------------------------------------

###across disaster types
### affected all
affected_shdi_deviation_all_df <- boot_odds_inequality_impacts_all("normalized_affected")
### killed all
killed_shdi_deviation_all_df <- boot_odds_inequality_impacts_all("normalized_deaths")
### Financial damages all
damages_shdi_deviation_all_df <- boot_odds_inequality_impacts_all("normalized_total_damages")

### Likelihood affected
dis_list_affected <- as.list(unique(disasters_df$disaster_type))
dis_df_affected <- data.frame(disaster = unique(disasters_df$disaster_type))
dis_df_affected$case <- sapply(dis_list_affected, consider_disaster_sHDIdeviation_affected)
dis_df_affected <- dis_df_affected %>% filter(case == 1)
dis_list <- as.list(dis_df_affected$disaster)
odds_inequality_affected_glm <- lapply(dis_list, boot_odds_inequality_affected_all)
odds_inequality_affected_glm_df <- do.call(rbind,odds_inequality_affected_glm)

### Likelihood fatalities
dis_list_fatalities <- as.list(unique(disasters_df$disaster_type))
dis_df_fatalities <- data.frame(disaster = unique(disasters_df$disaster_type))
dis_df_fatalities$case <- sapply(dis_list_fatalities, consider_disaster_sHDIdeviation_fatalities)
dis_df_fatalities <- dis_df_fatalities %>% filter(case == 1)
dis_list <- as.list(dis_df_fatalities$disaster)
odds_inequality_fatalities_glm <- lapply(dis_list, boot_odds_inequality_fatalities_all)
odds_inequality_fatalities_glm_df <- do.call(rbind,odds_inequality_fatalities_glm)

### Likelihood economic losses
dis_list_damages <- as.list(unique(disasters_df$disaster_type))
dis_df_damages <- data.frame(disaster = unique(disasters_df$disaster_type))
dis_df_damages$case <- sapply(dis_list_damages, consider_disaster_sHDIdeviation_damages)
dis_df_damages <- dis_df_damages %>% filter(case == 1)
dis_list <- as.list(dis_df_damages$disaster)
odds_inequality_damages_glm <- lapply(dis_list, boot_odds_inequality_damages_all)
odds_inequality_damages_glm_df <- do.call(rbind,odds_inequality_damages_glm)

### Format and save results

affected_shdi_deviation_all_df$impact <- "Likelihood affected"
killed_shdi_deviation_all_df$impact <- "Likelihood fatalities"
damages_shdi_deviation_all_df$impact <- "Likelihood economic losses"

odds_inequality_affected_glm_df$impact <- "Likelihood affected"
odds_inequality_fatalities_glm_df$impact <- "Likelihood fatalities"
odds_inequality_damages_glm_df$impact <- "Likelihood economic losses"

odds_disasters_inequalities_df <- rbind(affected_shdi_deviation_all_df,odds_inequality_affected_glm_df,
                                        killed_shdi_deviation_all_df,odds_inequality_fatalities_glm_df,
                                        damages_shdi_deviation_all_df,odds_inequality_damages_glm_df)


odds_disasters_inequalities_df <- odds_disasters_inequalities_df %>% 
  mutate(significance = case_when( pvalue > 0.05 ~ ".",
                                   pvalue <= 0.05 & pvalue > 0.01 ~ "* ",
                                   pvalue <= 0.01 & pvalue > 0.001 ~ " ** ",
                                   pvalue <= 0.001 ~ " *** "))

odds_disasters_inequalities_df <- odds_disasters_inequalities_df %>% 
  mutate(alpha_sig = case_when( pvalue > 0.05 ~ 0.2,
                                pvalue <= 0.05 & pvalue > 0.01 ~ 1,
                                pvalue <= 0.01 & pvalue > 0.001 ~ 1,
                                pvalue <= 0.001 ~ 1))

odds_disasters_inequalities_df <- odds_disasters_inequalities_df %>% 
  mutate(deviation_category = str_trim(deviation_category))


write_csv(odds_disasters_inequalities_df, "data/output/odds_disasters_inequalities_df.csv")

rm(list=ls())