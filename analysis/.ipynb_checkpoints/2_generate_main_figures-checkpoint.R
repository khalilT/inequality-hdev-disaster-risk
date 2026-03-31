### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"


### This code generates the main figures of the paper in folder: figures/main_figures


# library(tidyverse)
# library(ggridges)#plot ridges
# library(cowplot)#for ggplot2 themes
# library(quantreg)#quantile regression
# library(ggpmisc)#quantile reg stats
# library(egg)#tag plots
# library(patchwork)#arrange plots
# library(sf)
# library(rnaturalearth)#coastlines and boundaries
# library(rnaturalearthdata)#coastlines and boundaries
# source("analysis/0_plot_settings.R")
# source("analysis/0_list_functions.R")


# read data ---------------------------------------------------------------

disasters_df <- read_csv("data/input/disaster_information.csv")

#load geographic details for maps
coastlines <- ne_coastline(scale = "medium", returnclass = "sf")
shp_countries <- ne_countries(scale = "medium", returnclass = "sf")

# 1 - Disaster geographic distribution & sHDI inequality ------------------

print("###############################")
print("generating main figure N1")
print("###############################")

### Disaster distribution
#plot all disaster types
dis_type_list <- as.list(c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire",  "Drought"))
plots_events <- lapply(dis_type_list, make_map_plot)
#add legend
plots_events[[8]] <- make_legend()
#set layout of maps
events_plot <- plots_events %>% wrap_plots(ncol=3)


### sHDI inequality plot
plot_data <- disasters_df %>% as.data.frame() %>%
  mutate(national_hdi_d = cut(national_hdi,breaks=seq(0,1,0.1),include.lowest = F,right = T)) %>%
  dplyr::select(c(national_hdi,sHDI_category,disaster_type,national_hdi_d,deviation,deviation_category,
                  normalized_affected, normalized_deaths,normalized_total_damages)) %>%
  tidyr::gather(impact, value, normalized_affected:normalized_total_damages) %>% drop_na() %>% filter(value > 0) %>%
  group_by(sHDI_category,disaster_type,impact) %>%
  filter(n() >= 5) %>%
  ungroup()

quant_vals <- quantile(plot_data$deviation, c(0.2, 0.8))
xticks_sHDI <- c("[0.2,0.3]", "(0.3,0.4]", "(0.4,0.5]", "(0.5,0.6]", "(0.6,0.7]", "(0.7,0.8]","(0.8,0.9]","(0.9,1]")

deviation_plot <- ggplot(data=plot_data,aes(x=deviation, y=national_hdi_d,group=national_hdi_d,fill=after_stat(x)))+
  geom_density_ridges_gradient(bandwidth = 0.0045,
                               alpha = 0.25,jittered_points = T,
                               point_alpha=0.2,point_shape=20, point_size=0.025,
                               rel_min_height=.01, scale=1)+
  geom_vline(xintercept = 0, alpha=0.4, color="black", linewidth=0.1)+
  geom_vline(xintercept = quant_vals[1], color="tomato", linewidth=1)+
  annotate("text",y=7, x = quant_vals[1], size=7,color="tomato",label = "Threshold worse-off regions, p20", vjust = 5)+
  geom_vline(xintercept = quant_vals[2], color="#035096", linewidth=1)+
  annotate("text",y=7, x = quant_vals[2], size=7, color="#035096",label = "Threshold better-off regions, p80", vjust = -5)+
  coord_flip()+
  scale_fill_stepsn(colours = c("grey20", "grey40", "grey60", "grey80", "grey99"),name="Deviation from \nNational HDI")+
  geom_point(alpha=0,aes(fill=after_stat(x)))+
  scale_y_discrete(labels=xticks_sHDI, expand = expansion(0.1,0.1))+
  labs(title = "")+
  ylab("National HDI")+xlab("Deviation (sHDI - HDI)")+
  cowplot::theme_cowplot()+
  theme_bw()+
  theme(text = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(size=18,angle = 0,hjust = 0.4, vjust = 0.1),
        plot.margin = margin(0,1,0,0, "cm"))


#save plot
# Set theme for annotations
thm <- theme(plot.title = element_text(face = 2, size = 24))
maps_plot      <- wrap_elements((events_plot) + 
                                  plot_annotation(title = "a", theme = thm))
hdi_plot   <- wrap_elements((deviation_plot) + 
                              plot_annotation(title = "b", theme = thm))
events_hd_plot <- (maps_plot / plot_spacer() / hdi_plot) +
  plot_layout(heights = c(0.55,-0.05,0.35))
#Save final events & climatology plot
ggsave(
  "figures/main_figures/fig1_distribution_disasters_plot.pdf",
  events_hd_plot,
  width = 15, height = 14, dpi = 150, units = "in", device=cairo_pdf)

print("###############################")
print("main figure N1 written to disk")
print("###############################")

# 2 - Exposure & Impacts ------------------------------------------------------

print("###############################")
print("generating main figure N2")
print("###############################")

raw_numbers_sHDI_group <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>%
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  ungroup()

raw_numbers_all <-disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>%
  group_by(year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(sHDI_category = "all")

raw_numbers_total_exposure_impacts_data <- rbind(raw_numbers_all,raw_numbers_sHDI_group)

raw_numbers_total_exposure_impacts_data <- raw_numbers_total_exposure_impacts_data %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))

raw_numbers_sHDI_group <- raw_numbers_sHDI_group %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))


raw_numbers_total_exposure_impacts_plots <- 
  raw_numbers_total_exposure_impacts_data %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("all","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
  ggplot(aes(x=year, y=sum_var, fill=sHDI_category,color=sHDI_category))+
  geom_area(data=raw_numbers_sHDI_group)+
  stat_rq_eqn(tau = 0.5)+
  scale_color_manual("HDI group",values = c("all" = "black", "Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_fill_manual("sHDI group",values = c("all" = "black", "Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+#xlab("")+
  facet_wrap(~name,scales = "free_y", nrow=1,
             labeller = labeller(name = c(`Exposed population` = "Exposed population \n(Billion people)",
                                          `Total affected` = "Total affected \n(Million people)",
                                          `Total mortality` = "Total mortality \n(Thousand people)",
                                          `Exposed GDP` = "Exposed GDP \n(Trillion $)",
                                          `Total damages` = "Total damages \n(Billion $)")))+
  scale_y_continuous(position = "right", labels = function(n){format(n, scientific = FALSE)})+
  ylab("")+
  theme_test()+
  theme(legend.position = "none",
        text = element_text(size=24),
        axis.text.x = element_text(size = 16,angle = 45, hjust=1),
        axis.text.y = element_text(size = 16),
        panel.margin.y = unit(0, "lines"),
        #strip.text =  element_text(size = 22),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.05, color="grey85"),
        legend.text = element_text(size=28),
        legend.title = element_text(size=28),
        axis.title.y = element_text(size = 18,vjust = 10))

proportions_total_exposure_impacts_plots <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year, y=sum_var, fill=sHDI_category))+
  geom_area(stat = "identity", position = "fill")+
  scale_fill_manual("sHDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+xlab("")+
  facet_wrap(~name,scales = "free", nrow=1)+
  scale_y_continuous(position = "right")+
  ylab("")+
  theme_test()+
  theme(legend.position = "bottom",
        text = element_text(size=24),
        axis.text.x = element_text(size = 16,angle = 45, hjust=1),
        axis.text.y = element_text(size = 16),
        panel.margin.y = unit(0, "lines"),
        #strip.text =  element_text(size = 14),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.05, color="grey85"),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24),
        axis.title.y = element_text(size = 18,vjust = 10),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

raw_numbers_plot <- tag_facet(raw_numbers_total_exposure_impacts_plots,fontface = 1, size = 8, x=2017)+ theme(strip.text = element_text())
prop_numbers_plot <- tag_facet(proportions_total_exposure_impacts_plots,tag_pool=letters[6:26],fontface = 1, size = 8,x=1990,y=1)+ theme(strip.text = element_text())

total_exposure_impacts_plot <- (raw_numbers_plot / plot_spacer() / prop_numbers_plot)+ plot_layout(heights = c(4, -0.5 ,4))

ggsave("figures/main_figures/fig2_total_exposure_impacts_plot.pdf",
       total_exposure_impacts_plot,
       width = 24, height = 12, dpi = 150, units = "in", device='pdf')

print("main figure N2 written to disk")



# 3 - Impact rates --------------------------------------------------------

print("###############################")
print("generating main figure N3")
print("###############################")

fractions_affected_sHDI_group <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         impact="Affected") %>% 
  select(c(year, impact, sHDI_category, normalized_affected)) %>% drop_na() %>% 
  mutate(normalized_affected = normalized_affected * 1e4)

fractions_affected_all <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         impact="Affected") %>% 
  select(c(year, impact, sHDI_category, normalized_affected)) %>% drop_na() %>% 
  mutate(normalized_affected = normalized_affected * 1e4) %>% 
  mutate(sHDI_category = "Global")

fractions_affected_data_all <- rbind(fractions_affected_all,fractions_affected_sHDI_group)
count_affected_shdi <- fractions_affected_data_all %>% group_by(sHDI_category) %>% summarise(count = n())
counts_affected_text <- count_affected_shdi %>%
  mutate(sHDI_category = factor(sHDI_category, levels=c("Global","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
  arrange(sHDI_category) %>% 
  mutate(text = paste(sHDI_category, "=", count)) %>%
  summarise(text = paste(text, collapse = "; ")) %>%
  mutate(text = paste0("npoints: ",text)) %>% 
  pull(text)
counts_affected_text <- gsub("High sHDI", "\nHigh sHDI", counts_affected_text)
counts_affected_text <- gsub("sHDI", "", counts_affected_text)


affected_all_plot <- fractions_affected_data_all %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Global","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
  ggplot(aes(x=year, y=normalized_affected, color=sHDI_category))+
  geom_point(data=fractions_affected_sHDI_group,size=0.75, alpha=0.1)+
  geom_quantile(quantiles = c(0.5), lwd=1, color="black")+
  geom_quantile(quantiles = c(0.5), lwd=1.5,
                aes(colour = sHDI_category))+
  stat_rq_eqn(tau = 0.5)+
  annotate("text", x = Inf, y = Inf, label = counts_affected_text, 
           hjust = 1, vjust = 1.1, size = 3, color = "black", parse = FALSE)+
  facet_wrap(~impact,scales="free_y",drop = FALSE)+
  coord_cartesian_panels(
    panel_limits = tibble::tribble(
      ~impact, ~ymin, ~ymax,
      "Affected",      0,     200,
    ))+
  scale_color_manual(name = 'sHDI group', values = c("Global" = "black", "Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col),breaks = c("Global","Low sHDI", "Medium sHDI", "High sHDI", "Very high sHDI"), drop = FALSE)+
  ylab("Fraction per \U2031 of exposed population")+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "none",
        legend.box="horizontal",
        axis.text.x = element_text(size=10, angle = 45),
        axis.text.y = element_text(size=10, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 0),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 24),
        panel.spacing=unit(0.5,"lines"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        panel.grid.major.x = element_line(color = "grey80",size = 0.5,linetype = 1),
        strip.text.x = element_text(hjust = 1, margin=margin(l=0)),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0,0,0,0, "cm"))+
  guides(colour =guide_legend(title.position="left", title.hjust = 0.5))


### Fractions fatalities

fractions_fatalities_sHDI_group <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         impact="Fatalities") %>% 
  select(c(year, impact, sHDI_category, normalized_deaths)) %>% drop_na() %>% 
  mutate(normalized_deaths = normalized_deaths * 1e4)

fractions_fatalities_all <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         impact="Fatalities") %>% 
  select(c(year, impact, sHDI_category, normalized_deaths)) %>% drop_na() %>% 
  mutate(normalized_deaths = normalized_deaths * 1e4) %>% 
  mutate(sHDI_category = "Global")

fractions_fatalities_data_all <- rbind(fractions_fatalities_all,fractions_fatalities_sHDI_group)
count_fatalities_shdi <- fractions_fatalities_data_all %>% group_by(sHDI_category) %>% summarise(count = n())
counts_fatalities_text <- count_fatalities_shdi %>%
  mutate(sHDI_category = factor(sHDI_category, levels=c("Global","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
  arrange(sHDI_category) %>% 
  mutate(text = paste(sHDI_category, "=", count)) %>%
  summarise(text = paste(text, collapse = "; ")) %>%
  mutate(text = paste0("npoints: ",text)) %>% 
  pull(text)
counts_fatalities_text <- gsub("High sHDI", "\nHigh sHDI", counts_fatalities_text)
counts_fatalities_text <- gsub("sHDI", "", counts_fatalities_text)

fatalities_all_plot <- fractions_fatalities_data_all %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Global","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
  ggplot(aes(x=year, y=normalized_deaths, color=sHDI_category))+
  geom_point(data=fractions_fatalities_sHDI_group,size=0.75, alpha=0.1)+
  geom_quantile(quantiles = c(0.5), lwd=1, color="black")+
  geom_quantile(quantiles = c(0.5), lwd=1.5,
                aes(colour = sHDI_category))+
  stat_rq_eqn(tau = 0.5)+
  annotate("text", x = Inf, y = Inf, label = counts_fatalities_text, 
           hjust = 1, vjust = 1.1, size = 3, color = "black", parse = FALSE)+
  facet_wrap(~impact,scales="free_y",drop = FALSE)+
  coord_cartesian_panels(
    panel_limits = tibble::tribble(
      ~impact, ~ymin, ~ymax,
      "Fatalities",      0,     0.2,
    ))+
  scale_color_manual(name = 'sHDI group', values = c("Global" = "black", "Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col),breaks = c("Global","Low sHDI", "Medium sHDI", "High sHDI", "Very high sHDI"), drop = FALSE)+
  ylab("Fraction per \U2031 of exposed population")+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "none",
        legend.box="horizontal",
        axis.text.x = element_text(size=10, angle = 45),
        axis.text.y = element_text(size=10, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 0),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 24),
        panel.spacing=unit(0.5,"lines"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        panel.grid.major.x = element_line(color = "grey80",size = 0.5,linetype = 1),
        strip.text.x = element_text(hjust = 1, margin=margin(l=0)),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0,0,0,0, "cm"))+
  guides(colour =guide_legend(title.position="left", title.hjust = 0.5))

### Fractions damages

fractions_damages_sHDI_group <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         impact="Economic Damages") %>% 
  select(c(year, impact, sHDI_category, normalized_total_damages)) %>% drop_na() %>% 
  mutate(normalized_total_damages = normalized_total_damages * 1e2)

fractions_damages_all <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         impact="Economic Damages") %>% 
  select(c(year, impact, sHDI_category, normalized_total_damages)) %>% drop_na() %>% 
  mutate(normalized_total_damages = normalized_total_damages * 1e2) %>% 
  mutate(sHDI_category = "Global")

fractions_damages_data_all <- rbind(fractions_damages_all,fractions_damages_sHDI_group)
count_damages_shdi <- fractions_damages_data_all %>% group_by(sHDI_category) %>% summarise(count = n())
counts_damages_text <- count_damages_shdi %>%
  mutate(text = paste(sHDI_category, "=", count)) %>%
  mutate(sHDI_category = factor(sHDI_category, levels=c("Global","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
  arrange(sHDI_category) %>% 
  summarise(text = paste(text, collapse = "; ")) %>%
  mutate(text = paste0("npoints: ",text)) %>% 
  pull(text)
counts_damages_text <- gsub("High sHDI", "\nHigh sHDI", counts_damages_text)
counts_damages_text <- gsub("sHDI", "", counts_damages_text)

damages_all_plot <- fractions_damages_data_all %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Global","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
  ggplot(aes(x=year, y=normalized_total_damages, color=sHDI_category))+
  geom_point(data=fractions_damages_sHDI_group,size=0.75, alpha=0.1)+
  geom_quantile(quantiles = c(0.5), lwd=1, color="black")+
  geom_quantile(quantiles = c(0.5), lwd=1.5,
                aes(colour = sHDI_category))+
  stat_rq_eqn(tau = 0.5)+
  annotate("text", x = Inf, y = Inf, label = counts_damages_text, 
           hjust = 1, vjust = 1.1, size = 3, color = "black", parse = FALSE)+
  facet_wrap(~impact,scales="free_y",drop = FALSE)+
  coord_cartesian_panels(
    panel_limits = tibble::tribble(
      ~impact, ~ymin, ~ymax,
      "Economic Damages",      0,     0.5,
    ))+
  scale_color_manual(name = 'sHDI group', values = c("Global" = "black","Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), breaks = c("Global","Low sHDI", "Medium sHDI", "High sHDI", "Very high sHDI"), drop = FALSE)+
  ylab("Fraction per \U0025 of exposed GDP")+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box="horizontal",
        axis.text.x = element_text(size=10, angle = 45),
        axis.text.y = element_text(size=10, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 0),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 24),
        panel.spacing=unit(0.5,"lines"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        panel.grid.major.x = element_line(color = "grey80",size = 0.5,linetype = 1),
        strip.text.x = element_text(hjust = 1, margin=margin(l=0)),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0,0,0,0, "cm"))+
  guides(colour =guide_legend(title.position="left", title.hjust = 0.5))

affected_all_tag = tag_facet(affected_all_plot,tag_pool = letters[1],x=2016, size=6,fontface = 1, vjust = 3)
affected_all_tag = affected_all_tag+theme(strip.text.x = element_text(size = 18))
fatalities_all_tag = tag_facet(fatalities_all_plot,tag_pool = letters[2],x=2016, size=6,fontface = 1, vjust = 3)
fatalities_all_tag = fatalities_all_tag+theme(strip.text.x = element_text(size = 18))
damages_all_tag = tag_facet(damages_all_plot,tag_pool = letters[3],x=2016, size=6,fontface = 1, vjust = 3)
damages_all_tag = damages_all_tag+theme(strip.text.x = element_text(size = 18))

fractions_hd_tags_plot <- (affected_all_tag | fatalities_all_tag | damages_all_tag)+
  plot_layout(guides = "collect") & theme(legend.position = 'bottom',plot.margin = margin(0,0,0,0, "cm"))

#Save final events & climatology plot
ggsave(
  "figures/main_figures/fig3_fraction_evolution_plot_all_disasters.pdf",
  fractions_hd_tags_plot,
  width = 12, height = 6, dpi = 150, units = "in", device=cairo_pdf)

print("###############################")
print("main figure N3 written to disk")
print("###############################")

# 4 - Disaster fingerprint ------------------------------------------------

print("###############################")
print("generating main figure N4")
print("###############################")

disaster_clim_shdi_df <- read_csv("data/input/disaster_types_hdi_sd_climatology.csv")

#order variables
variables_order <- c("max.t2m", "min.t2m", "rel..humidity", "total.precip.", "total.runoff",
                     "surf..moisture", "max.windgust","SPEI.30", "cloud.cover")

disaster_clim_hdi_plot <- disaster_clim_shdi_df %>% 
  rename(max.t2m=`max t2m`, min.t2m=`min t2m`, rel..humidity = `rel. humidity`, total.precip. = `total precip.`, total.runoff =  `total runoff`, surf..moisture = `surf. moisture`, max.windgust =`max windgust`) %>% 
  pivot_longer(max.t2m:max.windgust) %>%
  mutate(disaster_type = factor(disaster_type, level=c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire')),
         hdi_category = factor(hdi_category, level=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")))%>%
  ggplot(aes(x=factor(name,level=variables_order),y=factor(time),fill=value))+
  geom_tile(color = "#F7F7F7",
            lwd = 0.1,
            linetype = 1) +
  scale_fill_gradientn(
    colors=c("tomato3","#F7F7F7","steelblue3"),
    limits=c(-2.2,2.2),
    "Zscored climate anomalies")+
  guides(fill = guide_colourbar(title.position="top"))+
  scale_x_discrete(labels=c('max t2m', 'min t2m', 'rel. humidity', 'tot. precip','tot. runoff','surf. moisture', 'max windgust'))+
  facet_grid(hdi_category~disaster_type, scales = "free",switch="y")+
  scale_y_discrete(position="right",limits=rev) +
  ylab("time in days")+xlab("")+
  theme_test()+
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size=10,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y = element_text(size=8),
    axis.ticks.y.right = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    strip.background =element_rect(fill="white"),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 18),
    panel.border = element_blank(),
    legend.text = element_text(size =  14),
    legend.title = element_text(size = 14, face = "bold",hjust=0.5),
    legend.key.height= unit(.5, 'cm'),
    legend.key.width= unit(2.5, 'cm'),
    plot.margin = unit(c(0.5, 0, 0.5, 0.5), "cm"),
    panel.spacing = unit(0, "lines"),
    legend.position="bottom",
    panel.background = element_rect(fill = NA, color = "black"))

#Save final events & climatology plot
ggsave(
  "figures/main_figures/fig4_disaster_fingerprints_hdi_climate.pdf",
  disaster_clim_hdi_plot,
  width = 10, height = 10, dpi = 150, units = "in", device='pdf'
)

print("main figure N4 written to disk")


# 5 - OR between sHDI groups ----------------------------------------------

print("###############################")
print("generating main figure N5")
print("###############################")

odds_disasters_df <- read_csv("data/output/odds_sHDI_disasters_df.csv")

odds_types_plot_significant <- odds_disasters_df %>% 
  filter(npoints >= 30) %>% 
  mutate(case = factor(case, levels=c("Low sHDI : Very high sHDI",
                                      "Medium sHDI : Very high sHDI",
                                      "High sHDI : Very high sHDI")),
         impact = factor(impact, levels=c("Likelihood affected","Likelihood fatalities","Likelihood economic damages"))) %>% 
  mutate(disaster = factor(disaster_type,levels = c("All types","Flood", "Storm", "Landslide", "Cold wave",
                                                    "Heat wave", "Wildfire", "Drought"))) %>% 
  ggplot(aes(x = log_odds, y=case, label=paste0("(x",signif(odds_ratio,2),")"))) +
  geom_errorbarh(aes(xmin = ci_log_low, xmax = ci_log_high,height = .25))+
  geom_point(aes(color=sHDI_category), pch=19, size = 3)+
  scale_color_manual(name = 'sHDI group', values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_shape_manual(values=c(0,1,2))+
  geom_text(aes(label=significance,x=ci_log_low+3), vjust=0.45, size=8, 
            check_overlap = T,color="tomato")+
  geom_text(aes(x=ci_log_low+1, alpha=alpha_sig, color=sHDI_category), vjust=1.2,hjust=0, size=5, fontface = "plain", show.legend = F)+
  geom_vline(aes(xintercept = 0), size=0.25,linetype = 2,color="black") +
  xlim(c(-10,10))+
  xlab(NULL) + ylab(NULL) +
  facet_grid(disaster~impact,switch = "y", margins = "vs")+
  theme_bw()+
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        strip.background =element_rect(fill="white"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text=element_text(size=14),
        panel.spacing = unit(0, "lines"),
        legend.title = element_text(size = 16, face = "plain"))+
  xlab("Log odds ratio")+
  ylab("Disaster type")

ggsave(
  "figures/main_figures/fig5_odds_ratio_per_disaster_type.pdf", 
  odds_types_plot_significant,
  width = 10, height = 10, dpi = 150, units = "in", device='pdf'
)

print("###############################")
print("main figure N5 written to disk")
print("###############################")

# 6 - OR between and within sHDI groups -----------------------------------

print("###############################")
print("generating main figure N6")
print("###############################")

odds_shDI_disasters_df <- read_csv("data/output/odds_disasters_inequalities_df.csv")

odds_ratio_per_disaster_shdi_inequality_type <-   odds_shDI_disasters_df %>% 
  filter(npoints >= 30) %>% 
  filter(disaster_type %in% c("All types","Flood", "Storm")) %>% 
  mutate(case = factor(case, levels=c("Low sHDI - worse-off : Very high sHDI",
                                      "Low sHDI - nat-average : Very high sHDI",
                                      "Low sHDI - better-off : Very high sHDI",
                                      "Medium sHDI - worse-off : Very high sHDI",
                                      "Medium sHDI - nat-average : Very high sHDI",
                                      "Medium sHDI - better-off : Very high sHDI",
                                      "High sHDI - worse-off : Very high sHDI",
                                      "High sHDI - nat-average : Very high sHDI",
                                      "High sHDI - better-off : Very high sHDI",
                                      "Very high sHDI - worse-off : Very high sHDI",
                                      "Very high sHDI - nat-average : Very high sHDI")),
         impact = factor(impact, levels=c("Likelihood affected","Likelihood fatalities","Likelihood economic damages"))) %>% 
  ggplot(aes(x = log_odds, y=case, label=paste0("(x",signif(odds_ratio,2),")"))) +
  geom_errorbarh(aes(xmin = ci_log_low, xmax = ci_log_high,height = .25))+
  geom_point(aes(shape = deviation_category, color=sHDI_category), size = 3)+
  scale_color_manual(name = 'sHDI group', values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_shape_manual(name = 'HDI deviation',values=c(15,16,17))+
  geom_text(aes(label=significance,x=ci_log_low+3), vjust=0.45, size=8, 
            check_overlap = T,color="tomato")+
  geom_text(aes(x=ci_log_low+1, alpha=alpha_sig,color=sHDI_category), vjust=1.2,hjust=0, size=4, fontface = "plain", show.legend = F)+
  geom_vline(aes(xintercept = 0), size=0.25,linetype = 2,color="black") +
  xlab(NULL) + ylab(NULL) +
  facet_grid(disaster_type~impact, switch = "y", margins = "vs")+
  guides(color = guide_legend("sHDI group",title.position = "top",hjust=0.5,
                              label.position = "bottom",
                              nrow = 1))+
  guides(shape = guide_legend("HDI deviation",title.position = "top",
                              label.position = "bottom",
                              nrow = 1))+
  theme_bw()+
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        strip.background =element_rect(fill="white"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text=element_text(size=14),
        panel.spacing = unit(0, "lines"),
        legend.title = element_text(size = 16, face = "plain"))+
  xlab("Log odds ratio")+
  ylab("Disaster type")

ggsave(
  "figures/main_figures/Fig6_odds_ratio_per_disaster_shdi_inequality_type.pdf", 
  odds_ratio_per_disaster_shdi_inequality_type,
  width = 10, height = 10, dpi = 150, units = "in", device='pdf'
)

print("###############################")
print("main figure N6 written to disk")
print("###############################")
