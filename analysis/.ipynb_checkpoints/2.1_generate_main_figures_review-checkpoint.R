
library(tidyverse)
library(ggridges)#plot ridges
library(cowplot)#for ggplot2 themes
library(quantreg)#quantile regression
library(ggpmisc)#quantile reg stats
library(egg)#tag plots
library(patchwork)#arrange plots
library(parallel)
library(ggstream)
library(ggh4x)
source("analysis/0_plot_settings.R")
source("analysis/0_list_functions.R")


# read data ---------------------------------------------------------------

disasters_df <- read_csv("data/input/disaster_information.csv")

# Missing data ------------------------------------------------------------
#R1M1
#Table 3

df <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought")))

impact_vars <- c("total_affected", "total_deaths", "total_damages")

# Function to summarize missingness
summarise_missing <- function(data, group_vars = NULL) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(
      all_of(impact_vars),
      list(
        n_missing = ~sum(is.na(.)),
        pct_missing = ~round(mean(is.na(.))*100, 1)
      ),
      .names = "{.col}_{.fn}"
    ),
    n = n(),
    .groups = "drop"
    )
}

# 1. Overall
overall <- summarise_missing(df) %>%
  mutate(Level = "Total") %>%
  select(Level, everything())

# 2. By disaster_type
by_disaster <- summarise_missing(df, "disaster_type") %>%
  mutate(Level = paste0("Hazard: ", disaster_type)) %>%
  select(Level, everything(), -disaster_type)

# 3. By sHDI_category
by_sHDI <- summarise_missing(df, "sHDI_category") %>%
  mutate(Level = paste0("sHDI: ", sHDI_category)) %>%
  select(Level, everything(), -sHDI_category)

# 4. By both disaster_type and sHDI_category
by_both <- summarise_missing(df, c("disaster_type", "sHDI_category")) %>%
  mutate(Level = paste0(" ", disaster_type, "/", sHDI_category)) %>%
  select(Level, everything(), -disaster_type, -sHDI_category)

# Combine all
final_table <- bind_rows(
  overall,
  by_disaster,
  by_sHDI,
  by_both
)

final_table %>% xtable::xtable(caption = "Counts and percentages of missing values for the considered 
                               impact variables per disaster type and sHDI group") %>% print(include.rownames=FALSE)


# multidim vulnberability -------------------------------------------------
#R1M2
#Figure 2

print("###############################")
print("generating main figure N2")
print("###############################")

sHDI_subindices <- read_csv("data/intermediate_data/event_shdi.csv")
sHDI_subindices <- sHDI_subindices %>% rename(`Dis No` = "DisNo.")

sHDI_subindices <- sHDI_subindices %>% rename("health_index" = "healthindex",
                                              "income_index" = "incindex",
                                              "education_index" = "edindex")

disasters_df <- left_join(disasters_df, sHDI_subindices)

disasters_df <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought")))

subindices_df <- disasters_df %>% 
  select(c(`Dis No`, deviation_category, disaster_type,year,
           normalized_affected, normalized_deaths, normalized_total_damages,
           sHDI, income_index, education_index, health_index, sHDI_category, deviation_category, continent, region)) %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")))


subindices_wide_df <- subindices_df %>% pivot_longer(sHDI:health_index)


diff_wide <- subindices_df %>%
  mutate(
    income_diff = income_index - sHDI,
    education_diff = education_index - sHDI,
    health_diff = health_index - sHDI
  )

# Gather for plotting
df_long_diff <- diff_wide %>%
  pivot_longer(cols = ends_with("_diff"), names_to = "component", values_to = "difference")


df_long_abs <- diff_wide %>% 
  pivot_longer(income_index:health_index)

#difference to sHDI and absolute vlues

sHDI_diff_plots <- df_long_diff %>%
  ggplot(aes(x = sHDI_category,
             y = difference,
             fill = deviation_category)) +
  geom_hline(yintercept=0, color='tomato', size=0.3) +
  geom_boxplot(position = position_dodge(width = 0.6),
               color = "black",
               outlier.colour = "grey", outlier.shape = 1, outlier.size = 0.8) +
  facet_wrap(~ component, nrow = 1) +
  labs(subtitle = "Relative Contribution of Each HDI Component to Subnational HDI",
       x = NULL, y = "Component – sHDI", fill = "Deviation") +
  scale_fill_manual(values = c("worse-off" = "red2",
                               "nat-average" = "grey70",
                               "better-off" = "cornflowerblue")) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 14,angle = 45, hjust=1),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 20),
    panel.margin.y = unit(1, "lines"),
    panel.spacing = unit(0.5, "lines"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

sHDI_abs_values <- df_long_abs %>%
  ggplot(aes(x = sHDI_category, y = value, fill = deviation_category)) +
  geom_boxplot(position = position_dodge(width = 0.6),
               color = "black", outlier.colour = "grey", outlier.shape = 1, outlier.size = 0.8) +
  facet_wrap(~ name, nrow = 1) +
  labs(subtitle = "HDI Components by sHDI Group and National Deviation",
       x = NULL, y = "Index Value", fill = "Deviation") +
  scale_fill_manual(values = c("worse-off" = "red2",
                               "nat-average" = "grey70",
                               "better-off" = "cornflowerblue")) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 14,angle = 45, hjust=1),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 20),
    panel.margin.y = unit(1, "lines"),
    panel.spacing = unit(0.5, "lines"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

sHDI_diff_plots_tags <- tag_facet(sHDI_diff_plots,fontface = 1, size = 8)+ theme(strip.text = element_text(size = 20))
sHDI_abs_values_tags <- tag_facet(sHDI_abs_values,fontface = 1, size = 8,tag_pool=letters[4:26])+ theme(strip.text = element_text(size = 20))

combined_plot <- sHDI_diff_plots_tags / sHDI_abs_values_tags +
  plot_layout(heights = c(1, 1)) +plot_layout(guides = "collect") +
  plot_annotation(title = "HDI Component Variation by sHDI Category and National Deviation")

ggsave("figures/main_figures/fig2_sHDI_components_plot.pdf",
       combined_plot,
       width = 14, height = 14, dpi = 300, units = "in", device='pdf')

print("main figure N2 written to disk")

# #sHDI trajectory map ----------------------------------------------------
#HDI years

print("###############################")
print("generating main figure N8")
print("###############################")

sHDI_shape_data <- st_read("data/intermediate_data/gdl_map_simplified.gpkg")


#HDI evolution

coastlines <- ne_coastline(scale = "medium", returnclass = "sf")
shp_countries <- ne_countries(scale = "medium", returnclass = "sf")

#sHDI maps

shdi_1990_plot <- sHDI_shape_data %>% 
  mutate(hdi_1990 = factor(hdi_1990, levels=c("Low HDI","Medium HDI","High HDI","Very high HDI"))) %>% 
  ggplot()+
  ggplot2::geom_sf(data = shp_countries,lwd = 0.2, color="grey90", fill="grey97")+
  ggplot2::geom_sf(data = coastlines,lwd = 0.1, color="grey10")+
  geom_sf(aes(fill=hdi_1990),color = NA,show.legend = TRUE)+
  scale_fill_manual("subnational HDI",values = c("Low HDI" = low_hdi_col, "Medium HDI" = medium_hdi_col, 
                                                 "High HDI" = high_hdi_col, "Very high HDI"=vhigh_hdi_col, na.value = "blank"),
                    breaks = c("Low HDI","Medium HDI","High HDI","Very high HDI"))+
  coord_sf(crs = st_crs('ESRI:54030'))+
  cowplot::theme_minimal_grid()+
  ggtitle("Subnational HDI 1990")+xlab("")+ylab("")+
  theme(plot.title = element_text(size = 14, hjust = 0.5, vjust=-0.5, face = "plain"),
        legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))

shdi_2020_plot <- sHDI_shape_data %>% 
  mutate(hdi_2020 = factor(hdi_2020,level=c("Low HDI","Medium HDI","High HDI","Very high HDI"))) %>% 
  ggplot()+
  ggplot2::geom_sf(data = shp_countries,lwd = 0.2, color="grey90", fill="grey97")+
  ggplot2::geom_sf(data = coastlines,lwd = 0.1, color="grey10")+
  geom_sf(aes(fill=hdi_2020),color = NA,show.legend = TRUE)+
  scale_fill_manual("subnational HDI",values = c("Low HDI" = low_hdi_col, "Medium HDI" = medium_hdi_col, 
                                                 "High HDI" = high_hdi_col, "Very high HDI"=vhigh_hdi_col, na.value = "blank"),
                    breaks = c("Low HDI","Medium HDI","High HDI","Very high HDI"))+
  coord_sf(crs = st_crs('ESRI:54030'))+
  cowplot::theme_minimal_grid()+
  ggtitle("Subnational HDI 2020")+xlab("")+ylab("")+
  theme(plot.title = element_text(size = 14, hjust = 0.5, vjust=-0.5, face = "plain"),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))


#sHDI evolution
sHDI_shape_data_evolution <- sHDI_shape_data %>% 
  mutate(hdi_evolution = X2020 - X1990)

sHDI_shape_data_evolution <- sHDI_shape_data %>% 
  mutate(promotion = (hdi_1990 != hdi_2020) & (X2020 > X1990))


sHDI_evolution_map <- sHDI_shape_data_evolution %>% 
  ggplot()+
  ggplot2::geom_sf(data = shp_countries,lwd = 0.2, color="grey90", fill="grey97")+
  ggplot2::geom_sf(data = coastlines,lwd = 0.1, color="grey10")+
  geom_sf(aes(fill=promotion),color = NA,show.legend = TRUE)+
  scale_fill_manual("sHDI group change",values = c("TRUE" = "darkolivegreen2", "FALSE" = "tomato", na.value = "blank"),
                    breaks = c("TRUE", "FALSE"))+
  coord_sf(crs = st_crs('ESRI:54030'))+
  cowplot::theme_minimal_grid()+
  ggtitle("sHDI group change between 1990 and 2020")+xlab("")+ylab("")+
  theme(plot.title = element_text(size = 14, hjust = 0.5, vjust=-0.5, face = "plain"),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))


shdi_1990_plot <- tag_facet(shdi_1990_plot,fontface = 1, size = 8, tag_pool = "a")+ theme(strip.text = element_text(size = 20))
shdi_2020_plot <- tag_facet(shdi_2020_plot,fontface = 1, size = 8, tag_pool = "b")+ theme(strip.text = element_text(size = 20))

shdi_maps <- (shdi_1990_plot / shdi_2020_plot) #+ plot_layout(guides = "collect") + theme(legend.position='bottom')

sHDI_evolution_map <- tag_facet(sHDI_evolution_map,fontface = 1, size = 8, tag_pool = "c")+ theme(strip.text = element_text(size = 20))

shdi_change_maps <- (shdi_maps / sHDI_evolution_map)

ggsave(
  "figures/main_figures/fig8_hdi_change_maps.pdf",
  shdi_change_maps,
  width = 10, height = 14, dpi = 300, units = "in")


print("main figure N8 written to disk")
