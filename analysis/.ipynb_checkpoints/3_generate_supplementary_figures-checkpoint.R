### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"


### This code generates the supplementary figures of the paper in folder: figures/supplementary_figures

# S1 - Disaster type fingerprints ------------------

print("###############################")
print("generating main figure S1")
print("###############################")

disaster_clim_df <- read_csv("data/input/disaster_types_sd_climatology.csv")

#order variables
variables_order <- c("max t2m", "min t2m", "rel. humidity", "total precip.", "total runoff",
                     "surf. moisture", "max windgust")

#plot climatological fingerprints
disaster_cli_plot <- disaster_clim_df %>% pivot_longer(`max t2m`:`max windgust`) %>%
  mutate(disaster_type = factor(disaster_type, levels=c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire'))) %>%
  ggplot(aes(x=factor(name,level=variables_order),y=factor(time),fill=value))+
  geom_tile(color = "#F7F7F7",
            lwd = 0.5,
            linetype = 1) +
  scale_y_discrete(limits=rev)+
  scale_fill_gradientn(
    colors=c("tomato3","#F7F7F7","#035096"),
    limits=c(-2.2,2.2),
    "Zscored \nclimate \nanomalies")+
  guides(fill = guide_colourbar(title.position="top"))+
  scale_x_discrete(labels=c('max t2m', 'min t2m', 'rel. humidity', 'tot. precip','tot. runoff','surf. moisture', 'max windgust'))+
  facet_grid(~factor(disaster_type, level=c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire')))+
  ylab("time in days")+xlab("")+
  theme_bw()+
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(size=16,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y = element_text(size=16),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    strip.background =element_rect(fill="white"),
    strip.text =  element_text(size = 20),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 20),
    panel.border = element_blank(),
    legend.text = element_text(size =  16),
    legend.title = element_text(size = 14, face = "bold",hjust=0),
    legend.key.height= unit(2.5, 'cm'),
    legend.key.width= unit(.5, 'cm'),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
    panel.spacing = unit(0.25, "lines"))

ggsave(
  "figures/supplementary_figures/S1_disaster_fingerprints_type_climate.pdf",
  disaster_cli_plot,
  width = 15, height = 7, dpi = 150, units = "in", device='pdf'
)

print("###############################")
print("main figure S1 written to disk")
print("###############################")

# S2 - Event counts per year and national HDI group ------------------

print("###############################")
print("generating main figure S2")
print("###############################")

disasters_total_count <- disasters_df %>% 
  filter(!is.na(sHDI_category)) %>% 
  mutate(sHDI_category = factor(sHDI_category, level=c("Very high sHDI", "High sHDI", "Medium sHDI","Low sHDI"))) %>% 
  group_by(year) %>% 
  summarise(event_count = n())

disasters_peryear_plot <- disasters_df %>% 
  filter(!is.na(sHDI_category)) %>%
  mutate(sHDI_category = factor(sHDI_category, level=c("Very high sHDI", "High sHDI", "Medium sHDI","Low sHDI"))) %>% 
  group_by(sHDI_category,year) %>% 
  ggplot(aes(x=factor(year), fill=sHDI_category))+
  geom_bar(stat="count", position = "stack", width=0.8)+
  geom_text(stat = "count", aes(label = ..count.., y = ..count..),face="bold",size=5,color="white",position=position_stack(0.5))+
  geom_label(data = disasters_total_count, aes(x=factor(year), y = event_count, label = event_count),size=5,vjust=-0.4,inherit.aes = FALSE)+
  scale_fill_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  ylim(0,400)+
  xlab("")+ylab("Events count")+
  #facet_wrap(~continent, scales = "free")+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.text.x = element_text(size=14,angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=14, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0.5,1,0.5,0.5, "cm"))

lon_dat <- disasters_df %>% 
  filter(natHDI_category != sHDI_category) %>% 
  select(continent,natHDI_category,sHDI_category) %>% 
  group_by(natHDI_category,sHDI_category) %>% 
  tally()

hdi_counts <- data.frame(table(disasters_df[["natHDI_category"]]))

national_subnational_hdi <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         natHDI_category = factor(natHDI_category, levels=c("Low HDI","Medium HDI","High HDI","Very high HDI"))) %>% 
  ggplot(aes(y=natHDI_category,fill=sHDI_category))+
  geom_bar(stat="count", position = "stack", width = 0.5)+
  geom_text(stat = "count", aes(label = ..count.., x = ..count..),size=5,color="white",face="bold",position=position_stack(0.5))+
  geom_text(data = hdi_counts, 
            aes(y = Var1, x = Freq+100, label = Freq), size=5,face="plain",
            inherit.aes = FALSE)+
  scale_fill_manual(name = 'sHDI group', values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  xlab("Event counts")+ylab("National HDI group")+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        axis.text.x = element_text(size=14,angle = 0, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=14, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

total_event_counts_plot <- (disasters_peryear_plot / plot_spacer() /national_subnational_hdi)+ plot_layout(heights = c(4, -0.5 ,3)) + plot_annotation(tag_levels = 'a')


ggsave("figures/supplementary_figures/S2_disasters_peryear_nathdi_plot.pdf",
       total_event_counts_plot,
       width = 18, height = 9, dpi = 150, units = "in", device='pdf')

print("###############################")
print("main figure S2 written to disk")
print("###############################")

# S3 - Total numbers total exposure and impacts ------------------

print("###############################")
print("generating main figure S3")
print("###############################")

rawnumbers_types_sHDI_exposure_impacts_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Total population`=total_population, `Total affected`=total_affected, `Total fatalities`=total_deaths, `Total GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Total population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Total population","Total affected","Total fatalities","Total GDP","Total damages"))) %>% 
  group_by(disaster_type,sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  mutate(across(everything(), ~replace(., . ==  0 , 1))) %>% 
  ungroup()


rawnumbers_types_all_exposure_impacts_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Total population`=total_population, `Total affected`=total_affected, `Total fatalities`=total_deaths, `Total GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Total population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Total population","Total affected","Total fatalities","Total GDP","Total damages"))) %>% 
  group_by(disaster_type,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  mutate(across(everything(), ~replace(., . ==  0 , 1))) %>% 
  ungroup() %>% 
  mutate(sHDI_category = "all")

rawnumbers_types_exposure_impacts_data <- rbind(rawnumbers_types_all_exposure_impacts_data,
                                                rawnumbers_types_sHDI_exposure_impacts_data)

rawnumbers_types_exposure_impacts_data <- rawnumbers_types_exposure_impacts_data %>% 
  mutate(sum_var = case_when(name=="Total population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total fatalities" ~ sum_var / 1e3,
                             name=="Total GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))

rawnumbers_types_sHDI_exposure_impacts_data <- rawnumbers_types_sHDI_exposure_impacts_data %>% 
  mutate(sum_var = case_when(name=="Total population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total fatalities" ~ sum_var / 1e3,
                             name=="Total GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))


rawnumbers_types_exposure_impacts_plots <- 
  rawnumbers_types_exposure_impacts_data %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("all","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>%
  ggplot(aes(x=year, y=sum_var, fill=sHDI_category,color=sHDI_category))+
  geom_area(data=rawnumbers_types_sHDI_exposure_impacts_data)+
  scale_color_manual("sHDI group",values = c("all" = "black", "Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_fill_manual("sHDI group",values = c("all" = "black", "Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+xlab("")+
  facet_grid(name~disaster_type,scales = "free_y",switch = "y",
             labeller = labeller(name = c(`Total population` = "Exposed population \n(Billion people)",
                                          `Total affected` = "Total affected \n(Million people)",
                                          `Total fatalities` = "Total mortality \n(Thousand people)",
                                          `Total GDP` = "Exposed GDP \n(Trillion $)",
                                          `Total damages` = "Total damages \n(Billion $)")))+
  scale_y_continuous(position = "right")+
  ylab("Total exposure & impact variables")+
  theme_test()+
  theme(legend.position = "bottom",
        text = element_text(size=28),
        axis.text.x = element_text(size = 16,angle = 45, hjust=1),
        axis.text.y = element_text(size = 16),
        panel.margin.y = unit(0, "lines"),
        strip.background =element_rect(fill="white"),
        strip.text =  element_text(size = 22),
        panel.grid.major.y = element_line( size=.05, color="grey85"),
        legend.text = element_text(size=28),
        legend.title = element_text(size=28),
        axis.title.y = element_text(vjust = 10),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))



ggsave("figures/supplementary_figures/S3_rawnumbers_types_exposure_impacts_plots.pdf",
       rawnumbers_types_exposure_impacts_plots,
       width = 26, height = 16, dpi = 150, units = "in", device='pdf')

print("###############################")
print("main figure S3 written to disk")
print("###############################")

# S4 - Proportion numbers total exposure and impacts ------------------

print("###############################")
print("generating main figure S4")
print("###############################")

proportions_types_exposure_impacts_plots <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Total population`=total_population, `Total affected`=total_affected, `Total fatalities`=total_deaths, `Total GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Total population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Total population","Total affected","Total fatalities","Total GDP","Total damages"))) %>% 
  group_by(disaster_type,sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  mutate(across(everything(), ~replace(., . ==  0 , 1))) %>% 
  ungroup() %>% 
  ggplot(aes(x=year, y=sum_var, fill=sHDI_category))+
  geom_stream(type = "proportional",bw =0.5,extra_span=0.1)+
  scale_fill_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+xlab("")+
  facet_grid2(name~disaster_type,scales = "free",switch = "y",render_empty = FALSE,
              labeller = labeller(name = c(`Total population` = "Exposed population",
                                           `Total affected` = "Total affected",
                                           `Total fatalities` = "Total mortality",
                                           `Total GDP` = "Exposed GDP",
                                           `Total damages` = "Total damages")))+
  scale_y_continuous(position = "right")+
  ylab("Proportion in exposure & impact variables")+
  theme_test()+
  theme(legend.position = "bottom",
        text = element_text(size=28),
        axis.text.x = element_text(size = 16,angle = 45, hjust=1),
        axis.text.y = element_text(size = 16),
        panel.margin.y = unit(0, "lines"),
        strip.background =element_rect(fill="white"),
        strip.text =  element_text(size = 22),
        panel.grid.major.y = element_line( size=.05, color="grey85"),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24),
        axis.title.y = element_text(size = 18,vjust = 10),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))



ggsave("figures/supplementary_figures/S4_proportions_types_exposure_impacts_plots.pdf",
       proportions_types_exposure_impacts_plots,
       width = 26, height = 16, dpi = 150, units = "in", device='pdf')

print("###############################")
print("main figure S4 written to disk")
print("###############################")

# S5 - Exposure and impacts in China and India ------------------

print("###############################")
print("generating main figure S5")
print("###############################")

#### China
china_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,country,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  filter(country == "China") %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T))

china_data <- china_data %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))

china_exposure_impacts <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,country,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year,y=sum_var, group=sHDI_category))+
  geom_area()+
  geom_area(data=china_data, aes(x=year,y=sum_var, group=sHDI_category, fill=sHDI_category))+
  facet_wrap(~name, scales = "free_y")+
  scale_color_manual("sHDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_fill_manual("sHDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+#xlab("")+
  facet_wrap(~name,scales = "free_y", nrow=1,
             labeller = labeller(name = c(`Exposed population` = "Exposed population \n(Billion people)",
                                          `Total affected` = "Total affected \n(Million people)",
                                          `Total mortality` = "Total mortality \n(Thousand people)",
                                          `Exposed GDP` = "Exposed GDP \n(Trillion $)",
                                          `Total damages` = "Total damages \n(Billion $)")))+
  scale_y_continuous(position = "right")+
  ylab("Values of exposure & impact variables")+xlab("")+
  theme_test()+
  theme(legend.position = "bottom",
        text = element_text(size=14),
        axis.text.x = element_text(size = 12,angle = 45, hjust=1),
        axis.text.y = element_text(size = 12),
        panel.margin.y = unit(0, "lines"),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line(size=.05, color="grey85"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        axis.title.y = element_text(size = 18,vjust = 10))

#### India
india_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,country,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  filter(country == "India") %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T))


india_data <- india_data %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))

india_exposure_impacts <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,country,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year,y=sum_var, group=sHDI_category))+
  geom_area()+
  geom_area(data=india_data, aes(x=year,y=sum_var, group=sHDI_category, fill=sHDI_category))+
  facet_wrap(~name, scales = "free_y")+
  scale_color_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_fill_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+#xlab("")+
  facet_wrap(~name,scales = "free_y", nrow=1,
             labeller = labeller(name = c(`Exposed population` = "Exposed population \n(Billion people)",
                                          `Total affected` = "Total affected \n(Million people)",
                                          `Total mortality` = "Total mortality \n(Thousand people)",
                                          `Exposed GDP` = "Exposed GDP \n(Trillion $)",
                                          `Total damages` = "Total damages \n(Billion $)")))+
  scale_y_continuous(position = "right")+
  ylab("Values of exposure & impact variables")+xlab("")+
  theme_test()+
  theme(legend.position = "none",
        text = element_text(size=14),
        axis.text.x = element_text(size = 12,angle = 45, hjust=1),
        axis.text.y = element_text(size = 12),
        panel.margin.y = unit(0, "lines"),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.05, color="grey85"),
        legend.text = element_text(size=28),
        legend.title = element_text(size=28),
        axis.title.y = element_text(size = 18,vjust = 10))


china_india_plot <- (china_exposure_impacts / india_exposure_impacts & ylab(NULL)) + plot_annotation(tag_levels = 'a')

china_india_plot <- wrap_elements(china_india_plot) +
  labs(tag = "Values of exposure & impact variables") +
  theme(
    plot.tag = element_text(size = 18, angle = 90),
    plot.tag.position = "right"
  )

#Save final events & climatology plot
ggsave(
  "figures/supplementary_figures/S5_china_india_exposure_impacts.pdf",
  china_india_plot,
  width = 14, height = 8, dpi = 150, units = "in", device=cairo_pdf)

print("###############################")
print("main figure S5 written to disk")
print("###############################")

# S6 - Most exposed and impacted countries ------------------

print("###############################")
print("generating main figure S6")
print("###############################")

most_exposed_impacted <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
  select(c(disaster_type,sHDI_category,year,iso,country,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>% 
  group_by(iso,sHDI_category) %>% 
  summarise(
    total_exposed_pop = sum(total_population, na.rm=T),
    total_affected = sum(total_affected, na.rm=T),
    total_fatalities = sum(total_deaths, na.rm=T),
    total_exposed_gdp = sum(total_gdp, na.rm=T),
    total_damages = sum(total_damages, na.rm=T)) %>% 
  rename(`Exposed population`=total_exposed_pop, `Affected`=total_affected, `Fatalities`=total_fatalities,
         `Exposed GDP`=total_exposed_gdp, `Economic damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Economic damages`) %>% 
  mutate(across(name, ~factor(., levels=c("Exposed population","Affected","Fatalities","Exposed GDP","Economic damages")))) %>% 
  group_by(iso, name) %>% 
  mutate(value_country = sum(value)) %>% 
  ungroup()

most_exposed <- most_exposed_impacted %>% select(iso,name, value_country)

most_exposed <- most_exposed %>% 
  distinct() %>% 
  group_by(name) %>% 
  slice_max(order_by = value_country, n = 10) %>% 
  mutate(selected = "true")

most_exposed_impacted <- left_join(most_exposed_impacted, most_exposed, by = c("iso","name","value_country"))

most_exposed_impacted <- most_exposed_impacted %>% 
  group_by(name) %>% 
  arrange(desc(value_country), .by_group = TRUE) %>% 
  mutate(iso=reorder_within(iso, value_country, name)) %>%
  ungroup()

most_exposed_impacted <- most_exposed_impacted %>%
  mutate(value_country = case_when(name=="Exposed population" ~ value_country / 1e9,
                                   name=="Affected" ~ value_country / 1e6,
                                   name=="Fatalities" ~ value_country / 1e3,
                                   name=="Exposed GDP" ~ value_country / 1e12,
                                   name=="Economic damages" ~ value_country / 1e9),
         value = case_when(name=="Exposed population" ~ value / 1e9,
                           name=="Affected" ~ value / 1e6,
                           name=="Fatalities" ~ value / 1e3,
                           name=="Exposed GDP" ~ value / 1e12,
                           name=="Economic damages" ~ value / 1e9),
         value_country = round(value_country,3),
         value = round(value,3))

most_exposed_impacted_plot <- most_exposed_impacted %>% 
  filter(selected == "true") %>% 
  ggplot()+
  scale_fill_manual("sHDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col,
                                            "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  geom_col(aes(x=iso, y=value, fill = sHDI_category),
           show.legend = T,stat = "identity")+
  facet_wrap(~name, scales = "free", nrow = 1,
             labeller = labeller(name = c(`Exposed population` = "Exposed population \n(Billion people)",
                                          `Affected` = "Total affected \n(Million people)",
                                          `Fatalities` = "Total mortality \n(Thousand people)",
                                          `Exposed GDP` = "Exposed GDP \n(Trillion $)",
                                          `Economic damages` = "Total damages \n(Billion $)")))+
  coord_flip()+
  scale_x_reordered()+
  theme_test()+xlab("")+ylab("")+
  theme(legend.position = "bottom",
        text = element_text(size=16),
        strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size = 8,angle = 0, hjust=0.5),
        axis.text.y = element_text(size = 14),
        panel.margin.y = unit(0, "lines"),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line(size=.05, color="grey85"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        axis.title.y = element_text(size = 12,vjust = 10))


#Save final events & climatology plot
ggsave(
  "figures/supplementary_figures/S6_most_exposed_impacted.pdf",
  most_exposed_impacted_plot,
  width = 18, height = 8, dpi = 150, units = "in", device=cairo_pdf)

print("###############################")
print("main figure S6 written to disk")
print("###############################")

# S7 - Trends of event-level impact rates per disaster type ------------------

print("###############################")
print("generating main figure S7")
print("###############################")

###Affected

affected_disasters_sHDI_data <- disasters_df %>% 
  filter(disaster_number_country != "2014-9429-MYS") %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave","Heat wave", "Wildfire", "Drought")), impact="Affected") %>% 
  select(c(year, impact, sHDI_category, normalized_affected,disaster_type,disaster_number_country)) %>% drop_na() %>% 
  group_by(sHDI_category,disaster_type) %>% 
  filter(n() >= 10) %>% 
  mutate(normalized_affected = normalized_affected * 1e4)

affected_disasters_all_data <- disasters_df %>% 
  filter(disaster_number_country != "2014-9429-MYS") %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave","Heat wave", "Wildfire", "Drought")), impact="Affected") %>% 
  select(c(year, impact, sHDI_category, normalized_affected,disaster_type,disaster_number_country)) %>% drop_na() %>% 
  group_by(disaster_type) %>% 
  filter(n() >= 10) %>% 
  mutate(normalized_affected = normalized_affected * 1e4) %>% 
  mutate(sHDI_category = "Global")

affected_disasters_data <- rbind(affected_disasters_all_data, affected_disasters_sHDI_data)

affected_disasters_plot <- affected_disasters_data %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Global","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
  ggplot(aes(x=year, y=normalized_affected, color=sHDI_category))+
  geom_point(data = affected_disasters_sHDI_data, size=1.5, alpha=0.15)+
  geom_quantile(quantiles = c(0.5), lwd=1.5,size=3,
                aes(colour = sHDI_category))+
  stat_rq_eqn(tau = 0.5)+
  facet_wrap(disaster_type~impact,scales="free_y",drop = FALSE)+
  geom_quantile(quantiles = c(0.5), lwd=1, color="black")+
  geom_quantile(quantiles = c(0.5), lwd=1.5,
                aes(colour = sHDI_category))+
  stat_rq_eqn(tau = 0.5)+
  facet_wrap(~disaster_type,ncol = 7,scales="free_y",drop = FALSE)+
  coord_cartesian_panels(
    panel_limits = tibble::tribble(
      ~disaster_type, ~ymin, ~ymax,
      "Flood",         0,     150,
      "Storm",         0,     150,
      "Landslide",     0,     50,
      "Cold wave",     0,     150,
      "Heat wave",     0,     50,
      "Wildfire",      0,     150,
      "Drought",       0,     5000,
    ))+
  scale_color_manual(name = 'sHDI group', values = c("Global" = "black", "Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col),breaks = c("Global","Low sHDI", "Medium sHDI", "High sHDI", "Very high sHDI"), drop = FALSE)+
  ylab("Affected (\U2031 exposed population)")+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "none",
        legend.box="horizontal",
        axis.text.x = element_text(size=12, angle = 45),
        axis.text.y = element_text(size=14, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 0),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 18),
        panel.spacing=unit(0.5,"lines"),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        panel.grid.major.x = element_line(color = "grey80",size = 0.5,linetype = 1),
        strip.text.x = element_text(hjust = 1, margin=margin(l=0)),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0,0,0,0, "cm"))+
  guides(colour =guide_legend(title.position="left", title.hjust = 0.5))


###Fatalities


fatalities_disasters_sHDI_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave","Heat wave", "Wildfire", "Drought")),
         impact="Fatalities") %>% 
  select(c(year, impact, sHDI_category, normalized_deaths,disaster_type,disaster_number_country)) %>% drop_na() %>% 
  group_by(sHDI_category,disaster_type) %>% 
  filter(n() >= 10) %>% 
  mutate(normalized_deaths = normalized_deaths * 1e4)

fatalities_disasters_all_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave","Heat wave", "Wildfire", "Drought")),
         impact="Fatalities") %>% 
  select(c(year, impact, sHDI_category, normalized_deaths,disaster_type,disaster_number_country)) %>% drop_na() %>% 
  group_by(disaster_type) %>% 
  filter(n() >= 10) %>% 
  mutate(normalized_deaths = normalized_deaths * 1e4) %>% 
  mutate(sHDI_category = "Global")

fatalities_disasters_data <- rbind(fatalities_disasters_all_data, fatalities_disasters_sHDI_data)

fatalities_disasters_plot <-  fatalities_disasters_data %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Global","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>%
  ggplot(aes(x=year, y=normalized_deaths, color=sHDI_category))+
  geom_point(data = fatalities_disasters_sHDI_data, size=1.5, alpha=0.15)+
  geom_quantile(quantiles = c(0.5), lwd=1, color="black")+
  geom_quantile(quantiles = c(0.5), lwd=1.5,size=3,
                aes(colour = sHDI_category))+
  stat_rq_eqn(tau = 0.5)+
  facet_wrap(disaster_type~impact,scales="free_y",drop = FALSE)+
  geom_quantile(quantiles = c(0.5), lwd=1.5,
                aes(colour = sHDI_category))+
  stat_rq_eqn(tau = 0.5)+
  facet_wrap(~disaster_type,ncol = 7,scales="free_y",drop = FALSE)+
  coord_cartesian_panels(
    panel_limits = tibble::tribble(
      ~disaster_type, ~ymin, ~ymax,
      "Flood",         0,     0.1,
      "Storm",         0,     0.1,
      "Landslide",     0,     1,
      "Cold wave",     0,     0.1,
      "Heat wave",     0,     0.25,
      "Wildfire",      0,     0.25,
      "Drought",       0,     1.0,
    ))+
  scale_color_manual(name = 'sHDI group', values = c("Global" = "black", "Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col),breaks = c("Global","Low sHDI", "Medium sHDI", "High sHDI", "Very high sHDI"), drop = FALSE)+
  ylab("Fatalities (\U2031 exposed population)")+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "none",
        legend.box="horizontal",
        axis.text.x = element_text(size=12, angle = 45),
        axis.text.y = element_text(size=14, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 0),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 18),
        panel.spacing=unit(0.5,"lines"),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        panel.grid.major.x = element_line(color = "grey80",size = 0.5,linetype = 1),
        strip.text.x = element_text(hjust = 1, margin=margin(l=0)),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0,0,0,0, "cm"))+
  guides(colour =guide_legend(title.position="left", title.hjust = 0.5))

###Damages

damages_disasters_sHDI_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave","Heat wave", "Wildfire", "Drought")),
         impact="Fatalities") %>% 
  select(c(year, impact, sHDI_category, normalized_total_damages,disaster_type,disaster_number_country)) %>% drop_na() %>% 
  group_by(sHDI_category,disaster_type) %>% 
  filter(n() >= 10) %>% 
  mutate(normalized_total_damages = normalized_total_damages * 1e2)

damages_disasters_all_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave","Heat wave", "Wildfire", "Drought")),
         impact="Fatalities") %>% 
  select(c(year, impact, sHDI_category, normalized_total_damages,disaster_type,disaster_number_country)) %>% drop_na() %>% 
  group_by(sHDI_category,disaster_type) %>% 
  filter(n() >= 10) %>% 
  mutate(normalized_total_damages = normalized_total_damages * 1e2) %>% 
  mutate(sHDI_category = "Global")

damages_disasters_data <- rbind(damages_disasters_all_data, damages_disasters_sHDI_data)

damages_disasters_plot <- damages_disasters_data %>%
  mutate(sHDI_category = factor(sHDI_category, levels=c("Global","Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>%
  ggplot(aes(x=year, y=normalized_total_damages, color=sHDI_category))+
  geom_point(data = damages_disasters_sHDI_data, size=1.5, alpha=0.15)+
  geom_quantile(quantiles = c(0.5), lwd=1, color="black")+
  geom_quantile(quantiles = c(0.5), lwd=1.5,size=3,
                aes(colour = sHDI_category))+
  stat_rq_eqn(tau = 0.5)+
  facet_wrap(disaster_type~impact,scales="free_y",drop = FALSE)+
  geom_quantile(quantiles = c(0.5), lwd=1.5,
                aes(colour = sHDI_category))+
  stat_rq_eqn(tau = 0.5)+
  facet_wrap(~disaster_type,ncol = 7,scales="free_y",drop = FALSE)+
  coord_cartesian_panels(
    panel_limits = tibble::tribble(
      ~disaster_type, ~ymin, ~ymax,
      "Flood",         0,     0.5,
      "Storm",         0,     0.5,
      "Landslide",     0,     1,
      "Cold wave",     0,     0.5,
      "Heat wave",     0,     0.5,
      "Wildfire",      0,     2,
      "Drought",       0,     1,
    ))+
  scale_color_manual(name = 'sHDI group', values = c("Global" = "black", "Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col),breaks = c("Global","Low sHDI", "Medium sHDI", "High sHDI", "Very high sHDI"), drop = FALSE)+
  ylab("Econ. damages (\U0025 exposed GDP)")+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box="horizontal",
        axis.text.x = element_text(size=12, angle = 45),
        axis.text.y = element_text(size=14, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 0),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 18),
        panel.spacing=unit(0.5,"lines"),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        panel.grid.major.x = element_line(color = "grey80",size = 0.5,linetype = 1),
        strip.text.x = element_text(hjust = 1, margin=margin(l=0)),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0,0,0,0, "cm"))+
  guides(colour =guide_legend(title.position="left", title.hjust = 0.5))

fractions_disasters_evolution_plot <- (affected_disasters_plot / fatalities_disasters_plot / damages_disasters_plot)+
  plot_layout(guides = "collect",heights = c(0.32,0.32,0.36)) & theme(legend.position = 'bottom')


#Save final events & climatology plot
ggsave(
  "figures/supplementary_figures/S7_fraction_disaster_evolution_plot.pdf",
  fractions_disasters_evolution_plot,
  width = 22, height = 14, dpi = 150, units = "in", device=cairo_pdf)

print("###############################")
print("main figure S7 written to disk")
print("###############################")

# S8 - Distance correlation hazards & Impact rates ------------------

print("###############################")
print("generating main figure S8")
print("###############################")

# variables_order <- c("max.t2m", "min.t2m", "total.precip.", "total.runoff",
#                      "surf..moisture", "max.windgust")
# 
# #Affected
# 
# affected_fraction_dcor_data <- disasters_df %>%
#   select(sHDI_category,disaster_type,normalized_affected,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf, max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor(.x,normalized_affected))) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   mutate(impacts = "affected fraction",
#          disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire'))))
# 
# affected_fraction_dcor_sig <- disasters_df %>%
#   select(sHDI_category,disaster_type,normalized_affected,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf, max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor.test(.x,normalized_affected, R=500)$p.value)) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   rename(pvalue = value)
# 
# affected_fraction_dcor_data <- left_join(affected_fraction_dcor_data,
#                                          affected_fraction_dcor_sig,by=c("disaster_type", "name"))
# 
# affected_fraction_dcor_data <- affected_fraction_dcor_data %>% 
#   mutate(sig = case_when(pvalue < 0.05 ~ "sig",
#                          pvalue >= 0.05 ~ "nonsig"))
# 
# 
# affected_fraction_dcor_plot <- affected_fraction_dcor_data %>% 
#   mutate(disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire')))) %>% 
#   ggplot(aes(x=factor(name,level=variables_order), y=disaster_type, fill=value))+
#   geom_tile(lwd = 1.5, color="white")+
#   geom_point(aes(x=factor(name,level=variables_order),y=disaster_type, shape = sig), size=7,
#              position = position_nudge(x = 0.325,y=0.3),show.legend = F)+
#   scale_shape_manual(values=c("", "*"))+
#   facet_wrap(~impacts)+
#   scale_colour_gradient("distance \ncorrelation",aesthetics = "fill",low="white", high="red",limits=c(0,1),breaks=seq(0,1,0.2))+
#   geom_text(aes(label = round(value,2)),size=6, color="black")+
#   ylab("disaster type")+xlab("")+
#   theme(text=element_text(size=16),
#         strip.text.x = element_text(size = 20),
#         axis.text.x = element_text(size=16,angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_rect(fill = "white"))
# 
# #fatalities  
# 
# mortality_fraction_dcor_data <- disasters_df %>%
#   select(sHDI_category,disaster_type,normalized_deaths,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf,
#          max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor(.x,normalized_deaths))) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   mutate(impacts = "mortality fraction",
#          disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire'))))
# 
# 
# mortality_fraction_dcor_sig <- disasters_df %>%
#   select(sHDI_category,disaster_type,normalized_deaths,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf,
#          max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor.test(.x,normalized_deaths,R=500)$p.value)) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   rename(pvalue = value)
# 
# 
# mortality_fraction_dcor_data <- left_join(mortality_fraction_dcor_data,mortality_fraction_dcor_sig,by=c("disaster_type", "name"))
# 
# mortality_fraction_dcor_data <- mortality_fraction_dcor_data %>% 
#   mutate(sig = case_when(pvalue < 0.05 ~ "sig",
#                          pvalue >= 0.05 ~ "nonsig"))
# 
# mortality_fraction_dcor_plot <- mortality_fraction_dcor_data %>% 
#   mutate(disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire')))) %>% 
#   ggplot(aes(x=factor(name,level=variables_order), y=disaster_type, fill=value))+
#   geom_tile(lwd = 1.5, color="white")+
#   geom_point(aes(x=factor(name,level=variables_order),y=disaster_type, shape = sig), size=7,
#              position = position_nudge(x = 0.325,y=0.3),show.legend = F)+
#   scale_shape_manual(values=c("", "*"))+
#   facet_wrap(~impacts)+
#   scale_colour_gradient("distance \ncorrelation",aesthetics = "fill",low="white", high="red",limits=c(0,1),breaks=seq(0,1,0.2))+
#   geom_text(aes(label = round(value,2)),size=6, color="black")+
#   ylab("")+xlab("climate variable")+
#   theme(text=element_text(size=16),
#         strip.text.x = element_text(size = 20),
#         axis.text.x = element_text(size=16,angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_rect(fill = "white"))
# 
# #econ damages
# 
# damages_fraction_dcor_data <- disasters_df %>%
#   select(sHDI_category,disaster_type,normalized_total_damages,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf,
#          max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor(.x,normalized_total_damages))) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   mutate(impacts = "total damages fraction",
#          disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire'))))
# 
# 
# damages_fraction_dcor_sig <- disasters_df %>%
#   select(sHDI_category,disaster_type,normalized_total_damages,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf,
#          max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor.test(.x,normalized_total_damages,R=500)$p.value)) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   rename(pvalue = value)
# 
# damages_fraction_dcor_data <- left_join(damages_fraction_dcor_data,damages_fraction_dcor_sig,by=c("disaster_type", "name"))
# 
# damages_fraction_dcor_data <- damages_fraction_dcor_data %>% 
#   mutate(sig = case_when(pvalue < 0.05 ~ "sig",
#                          pvalue >= 0.05 ~ "nonsig"))
# 
# damages_fraction_dcor_plot <- damages_fraction_dcor_data %>% 
#   mutate(disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire')))) %>% 
#   ggplot(aes(x=factor(name,level=variables_order), y=disaster_type, fill=value))+
#   geom_tile(lwd = 1.5, color="white")+
#   geom_point(aes(x=factor(name,level=variables_order),y=disaster_type, shape = sig), size=7,
#              position = position_nudge(x = 0.325,y=0.3),show.legend = F)+
#   scale_shape_manual(values=c("", "*"))+
#   facet_wrap(~impacts)+
#   scale_colour_gradient("distance \ncorrelation",aesthetics = "fill",low="white", high="red",limits=c(0,1),breaks=seq(0,1,0.2))+
#   geom_text(aes(label = round(value,2)),size=6, color="black")+
#   ylab("")+xlab("")+
#   theme(text=element_text(size=16),
#         strip.text.x = element_text(size = 20),
#         axis.text.x = element_text(size=16,angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_rect(fill = "white"))
# 
# 
# disaster_impact_correlations <- (affected_fraction_dcor_plot | mortality_fraction_dcor_plot | damages_fraction_dcor_plot) + 
#   plot_layout(guides = "collect") & 
#   theme(legend.position = 'bottom', legend.key.width = unit(2.5,"line"), 
#         legend.text = element_text(size = 14),legend.title = element_text(size = 14, face = "plain",vjust=1.5))
# 
# #Save final events & climatology plot
# ggsave(
#   "figures/supplementary_figures/S8_dcor_impactrates_climatevars_correlations.pdf",
#   disaster_impact_correlations,
#   width = 16, height = 8, dpi = 150, units = "in", device='pdf'
# )


print("###############################")
print("main figure S8 written to disk")
print("###############################")

# S9 - Distance correlation per sHDI group ------------------

print("###############################")
print("generating main figure S9")
print("###############################")

# variables_order <- c("max.t2m", "min.t2m", "total.precip.", "total.runoff","surf..moisture", "max.windgust")
# 
# ### Affected
# affected_fraction_dcor_sHDI_data <- disasters_df %>%
#   select(sHDI_category,disaster_type,sHDI_category,normalized_affected,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf,
#          max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type,sHDI_category) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor(.x,normalized_affected))) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   mutate(impacts = "affected fraction",
#          disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire'))),
#          sHDI_category = factor(sHDI_category, level=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")))
# 
# 
# affected_fraction_dcor_sHDI_sig <- disasters_df %>%
#   select(sHDI_category,disaster_type,sHDI_category,normalized_affected,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf,
#          max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type,sHDI_category) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor.test(.x,normalized_affected,R=500)$p.value)) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   rename(pvalue = value)
# 
# affected_fraction_dcor_sHDI_data <- left_join(affected_fraction_dcor_sHDI_data,
#                                               affected_fraction_dcor_sHDI_sig,by=c("disaster_type", "name","sHDI_category"))
# 
# affected_fraction_dcor_sHDI_data <- affected_fraction_dcor_sHDI_data %>% 
#   mutate(sig = case_when(pvalue < 0.05 ~ "sig",
#                          pvalue >= 0.05 ~ "nonsig"))
# 
# affected_fraction_dcor_sHDI_plot <- affected_fraction_dcor_sHDI_data %>% 
#   mutate(disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire'))),
#          sHDI_category = factor(sHDI_category, level=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
#   ggplot(aes(x=factor(name,level=variables_order), y=disaster_type, fill=value))+
#   geom_tile(lwd = 1.5, color="white")+
#   geom_point(aes(x=factor(name,level=variables_order),y=disaster_type, shape = sig), size=7,
#              position = position_nudge(x = 0.325,y=0.3),show.legend = F)+
#   scale_shape_manual(values=c("", "*"))+
#   facet_row(impacts~sHDI_category)+
#   scale_colour_gradient("distance \ncorrelation",aesthetics = "fill",low="white", high="red",limits=c(0,1),breaks=seq(0,1,0.2))+
#   geom_text(aes(label = round(value,2)),size=6, color="black")+
#   ylab("")+xlab("")+
#   theme(text=element_text(size=16),
#         axis.text.x = element_text(size=0,angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_rect(fill = "white"))
# 
# ### Fatalities
# mortality_fraction_dcor_sHDI_data <- disasters_df %>%
#   select(sHDI_category,disaster_type,sHDI_category,normalized_deaths,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf,
#          max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type,sHDI_category) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor(.x,normalized_deaths))) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   mutate(impacts = "mortality fraction",
#          disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire'))),
#          sHDI_category = factor(sHDI_category, level=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")))
# 
# 
# mortality_fraction_dcor_sHDI_sig <- disasters_df %>%
#   select(sHDI_category,disaster_type,sHDI_category,normalized_deaths,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf,
#          max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type,sHDI_category) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor.test(.x,normalized_deaths,R = 500)$p.value)) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   rename(pvalue = value)
# 
# 
# mortality_fraction_dcor_sHDI_data <- left_join(mortality_fraction_dcor_sHDI_data,
#                                                mortality_fraction_dcor_sHDI_sig,by=c("disaster_type", "name","sHDI_category"))
# 
# mortality_fraction_dcor_sHDI_data <- mortality_fraction_dcor_sHDI_data %>% 
#   mutate(sig = case_when(pvalue < 0.05 ~ "sig",
#                          pvalue >= 0.05 ~ "nonsig"))
# 
# mortality_fraction_dcor_sHDI_plot <- mortality_fraction_dcor_sHDI_data %>% 
#   mutate(disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire'))),
#          sHDI_category = factor(sHDI_category, level=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
#   ggplot(aes(x=factor(name,level=variables_order), y=disaster_type, fill=value))+
#   geom_tile(lwd = 1.5, color="white")+
#   geom_point(aes(x=factor(name,level=variables_order),y=disaster_type, shape = sig), size=7,
#              position = position_nudge(x = 0.325,y=0.3),show.legend = F)+
#   scale_shape_manual(values=c("", "*"))+
#   facet_row(impacts~sHDI_category)+
#   scale_colour_gradient("distance \ncorrelation",aesthetics = "fill",low="white", high="red",limits=c(0,1),breaks=seq(0,1,0.2))+
#   geom_text(aes(label = round(value,2)),size=6, color="black")+
#   ylab("disaster type")+xlab("")+
#   theme(text=element_text(size=16),
#         axis.text.x = element_text(size=0,angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_rect(fill = "white"))
# 
# 
# ### Econ damages
# damages_fraction_dcor_sHDI_data <- disasters_df %>%
#   select(sHDI_category,disaster_type,sHDI_category,normalized_total_damages,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf, max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type,sHDI_category) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor(.x,normalized_total_damages))) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   mutate(impacts = "total damages fraction",
#          disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire'))),
#          sHDI_category = factor(sHDI_category, level=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")))
# 
# damages_fraction_dcor_sHDI_sig <- disasters_df %>%
#   select(sHDI_category,disaster_type,sHDI_category,normalized_total_damages,ZMAX_t2mmax, ZMIN_t2mmin, ZMAX_tp, ZMAX_tro,ZMIN_smsurf, ZMAX_windgustmax) %>%
#   rename(max.t2m = ZMAX_t2mmax, min.t2m = ZMIN_t2mmin,  total.precip. = ZMAX_tp, total.runoff =ZMAX_tro, surf..moisture = ZMIN_smsurf, max.windgust = ZMAX_windgustmax ) %>% 
#   drop_na() %>%
#   group_by(disaster_type,sHDI_category) %>%
#   filter(n() >= 10) %>%
#   summarise(across(max.t2m:max.windgust,~dcor.test(.x,normalized_total_damages,R=500)$p.value)) %>% 
#   pivot_longer(max.t2m:max.windgust) %>% 
#   rename(pvalue = value)
# 
# damages_fraction_dcor_sHDI_data <- left_join(damages_fraction_dcor_sHDI_data,
#                                              damages_fraction_dcor_sHDI_sig,by=c("disaster_type", "name","sHDI_category"))
# 
# damages_fraction_dcor_sHDI_data <- damages_fraction_dcor_sHDI_data %>% 
#   mutate(sig = case_when(pvalue < 0.05 ~ "sig",
#                          pvalue >= 0.05 ~ "nonsig"))
# 
# damages_fraction_dcor_sHDI_plot <- damages_fraction_dcor_sHDI_data %>% 
#   mutate(disaster_type = factor(disaster_type, levels=rev(c('Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire'))),
#          sHDI_category = factor(sHDI_category, level=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
#   ggplot(aes(x=factor(name,level=variables_order), y=disaster_type, fill=value))+
#   geom_tile(lwd = 1.5, color="white")+
#   geom_point(aes(x=factor(name,level=variables_order),y=disaster_type, shape = sig), size=7,
#              position = position_nudge(x = 0.325,y=0.3),show.legend = F)+
#   scale_shape_manual(values=c("", "*"))+
#   facet_row(impacts~sHDI_category)+
#   scale_colour_gradient("distance \ncorrelation",aesthetics = "fill",low="white", high="red",limits=c(0,1),breaks=seq(0,1,0.2))+
#   geom_text(aes(label = round(value,2)),size=6, color="black")+
#   ylab("")+xlab("")+
#   theme(text=element_text(size=16),
#         axis.text.x = element_text(size=16,angle = 90, vjust = 0.5, hjust=1),
#         panel.background = element_rect(fill = "white"))
# 
# 
# disaster_impact_sHDI_correlations <- (affected_fraction_dcor_sHDI_plot / mortality_fraction_dcor_sHDI_plot / damages_fraction_dcor_sHDI_plot) +
#   plot_layout(guides = "collect") & 
#   theme(legend.position = 'bottom', legend.key.width = unit(2.5,"line"), legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14, face = "plain",vjust=1.5))
# 
# ggsave(
#   "figures/supplementary_figures/S9_dcor_impactrates_climatevars_sHDI_correlations.pdf",
#   disaster_impact_sHDI_correlations,
#   width = 18, height = 14, dpi = 150, units = "in", device='pdf'
# )


print("###############################")
print("main figure S9 written to disk")
print("###############################")

# S10 - Event anomalies bivariate distributions ------------------

print("###############################")
print("generating main figure S10")
print("###############################")

legend_shdi_bivar <- ggplot(disasters_df %>% mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))), aes(x=ZMAX_windgustmax, y=ZMAX_tp, color=sHDI_category))+
  geom_point()+
  scale_color_manual(name = 'sHDI category', 
                     values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col),
                     drop = FALSE)+
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size =  18),
        legend.title = element_text(size = 18, face = "bold"))+
  guides(colour = guide_legend(override.aes = list(size=9)))

### Zscored anomalies plots

#excess precipitation events

hot_dry_xlim_z = c(-5,5)
hot_dry_ylim_z = c(-1,5)
hot_dry_xlabel_z = "Surface moisture (SD)"
hot_dry_ylabel_z = "Maximum temperature (SD)"

coldwave_xlim_z = c(-2.5,2.5)
coldwave_ylim_z = c(-2.5,2.5)
coldwave_xlabel_z = "Surface moisture (SD)"
coldwave_ylabel_z = "Minimum temperature (SD)"

excessprecip_xlim_z = c(-1,16)
excessprecip_ylim_z = c(-5,30)
excessprecip_xlabel_z = "Maximum wind gust (SD)"
excessprecip_ylabel_z = "Total daily precipitation (SD)"

p_heatwave_z  <-  plot_dist("Heat wave","ZMIN_smsurf","ZMAX_t2mmax",hot_dry_xlim_z,hot_dry_ylim_z, hot_dry_xlabel_z, hot_dry_ylabel_z,"Zscored anomalies")
p_drought_z <- plot_dist("Drought","ZMIN_smsurf","ZMAX_t2mmax",hot_dry_xlim_z,hot_dry_ylim_z, hot_dry_xlabel_z, hot_dry_ylabel_z,"Zscored anomalies")
p_wildfire_z <- plot_dist("Wildfire","ZMIN_smsurf","ZMAX_t2mmax",hot_dry_xlim_z,hot_dry_ylim_z, hot_dry_xlabel_z, hot_dry_ylabel_z,"Zscored anomalies")

p_coldwave_z <- plot_dist("Cold wave","ZMIN_smsurf","ZMIN_t2mmin",coldwave_xlim_z,coldwave_ylim_z, coldwave_xlabel_z, coldwave_ylabel_z,"Zscored anomalies")

p_flood_z <- plot_dist("Flood","ZMAX_windgustmax","ZMAX_tp",excessprecip_xlim_z,excessprecip_ylim_z, excessprecip_xlabel_z, excessprecip_ylabel_z,"Zscored anomalies")
p_storm_z <- plot_dist("Storm","ZMAX_windgustmax","ZMAX_tp",excessprecip_xlim_z,excessprecip_ylim_z, excessprecip_xlabel_z, excessprecip_ylabel_z,"Zscored anomalies")
p_landslide_z <- plot_dist("Landslide","ZMAX_windgustmax","ZMAX_tp",excessprecip_xlim_z,excessprecip_ylim_z, excessprecip_xlabel_z, excessprecip_ylabel_z,"Zscored anomalies")

#hot and dry events

hot_dry_xlim_a = c(-0.15,0.15)
hot_dry_ylim_a = c(-5,20)
hot_dry_xlabel_a = expression(smsurf ~ anoms. ~ mm ~ d^-1)
hot_dry_ylabel_a = "Maximum temperature (°C)"

coldwave_xlim_a = c(-0.15,0.15)
coldwave_ylim_a = c(-25,10)
coldwave_xlabel_a = expression(smsurf ~ anoms. ~ mm ~ d^-1)
coldwave_ylabel_a = "Minimum temperature (°C)"

excessprecip_xlim_a = c(-25,120)
excessprecip_ylim_a = c(-20,140)
excessprecip_xlabel_a = expression(windgustmax ~ anoms. ~ km ~ h^-1)
excessprecip_ylabel_a = expression(precipitation ~ anoms. ~ mm ~ d^-1)

p_heatwave_a  <-  plot_dist("Heat wave","AMIN_smsurf","AMAX_t2mmax",hot_dry_xlim_a,hot_dry_ylim_a, hot_dry_xlabel_a, hot_dry_ylabel_a,"Original units anomalies")
p_drought_a <- plot_dist("Drought","AMIN_smsurf","AMAX_t2mmax",hot_dry_xlim_a,hot_dry_ylim_a, hot_dry_xlabel_a, hot_dry_ylabel_a,"Original units anomalies")
p_wildfire_a <- plot_dist("Wildfire","AMIN_smsurf","AMAX_t2mmax",hot_dry_xlim_a,hot_dry_ylim_a, hot_dry_xlabel_a, hot_dry_ylabel_a,"Original units anomalies")

p_coldwave_a <- plot_dist("Cold wave","AMIN_smsurf","AMIN_t2mmin",coldwave_xlim_a,coldwave_ylim_a, coldwave_xlabel_a, coldwave_ylabel_a,"Original units anomalies")

p_flood_a <- plot_dist("Flood","AMAX_windgustmax","AMAX_tp",excessprecip_xlim_a,excessprecip_ylim_a, excessprecip_xlabel_a, excessprecip_ylabel_a,"Original units anomalies")
p_storm_a <- plot_dist("Storm","AMAX_windgustmax","AMAX_tp",excessprecip_xlim_a,excessprecip_ylim_a, excessprecip_xlabel_a, excessprecip_ylabel_a,"Original units anomalies")
p_landslide_a <- plot_dist("Landslide","AMAX_windgustmax","AMAX_tp",excessprecip_xlim_a,excessprecip_ylim_a, excessprecip_xlabel_a, excessprecip_ylabel_a,"Original units anomalies")

### reorganize plots differently

flood_plot <- (p_flood_z | p_flood_a) + patchwork::plot_annotation("Floods", theme = theme(plot.title = element_text(face = "bold", size=18,hjust = 0.4)))
storm_plot <- (p_storm_z | p_storm_a) + patchwork::plot_annotation("Storms", theme = theme(plot.title = element_text(face = "bold", size=18,hjust = 0.4)))
landslide_plot <- (p_landslide_z | p_landslide_a) + patchwork::plot_annotation("Landslides", theme = theme(plot.title = element_text(face = "bold", size=18,hjust = 0.4)))

coldwaves_plot <- (p_coldwave_z | p_coldwave_a) + patchwork::plot_annotation("Cold waves", theme = theme(plot.title = element_text(face = "bold", size=18,hjust = 0.4)))

heatwaves_plot <- (p_heatwave_z | p_heatwave_a) + patchwork::plot_annotation("Heat waves", theme = theme(plot.title = element_text(face = "bold", size=18,hjust = 0.4)))
droughts_plot <- (p_drought_z | p_drought_a) + patchwork::plot_annotation("Droughts", theme = theme(plot.title = element_text(face = "bold", size=18,hjust = 0.4)))
wildfires_plot <- (p_wildfire_z | p_wildfire_a) + patchwork::plot_annotation("Wildfires", theme = theme(plot.title = element_text(face = "bold", size=18,hjust = 0.4)))



anomalies_plot <- ggpubr::ggarrange(flood_plot,storm_plot,landslide_plot,coldwaves_plot,
                                    heatwaves_plot,droughts_plot,wildfires_plot,legend_shdi_bivar, nrow = 4,ncol=2)

ggsave(
  "figures/supplementary_figures/S10_anomalies_bivariate_distribution.pdf", 
  anomalies_plot, 
  width = 18, height = 16, dpi = 150, units = "in", device='pdf'
)

print("###############################")
print("main figure S10 written to disk")
print("###############################")

# S11 - Continents exposure and impacts ------------------

print("###############################")
print("generating main figure S11")
print("###############################")

#### Africa
africa_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,continent,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  filter(continent == "Africa") %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T))

africa_data <- africa_data %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))


africa_exposure_impacts <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,country,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year,y=sum_var, group=sHDI_category))+
  geom_area()+
  geom_area(data=africa_data, aes(x=year,y=sum_var, group=sHDI_category, fill=sHDI_category))+
  facet_wrap(~name, scales = "free_y")+
  scale_color_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_fill_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+#xlab("")+
  facet_wrap(~name,scales = "free_y", nrow=1,
             labeller = labeller(name = c(`Exposed population` = "Exposed population \n(Billion people)",
                                          `Total affected` = "Total affected \n(Million people)",
                                          `Total mortality` = "Total mortality \n(Thousand people)",
                                          `Exposed GDP` = "Exposed GDP \n(Trillion $)",
                                          `Total damages` = "Total damages \n(Billion $)")))+
  scale_y_continuous(position = "right")+
  ylab("Values of exposure & impact variables")+xlab("")+
  theme_test()+
  theme(legend.position = "none",
        text = element_text(size=14),
        axis.text.x = element_text(size = 12,angle = 45, hjust=1),
        axis.text.y = element_text(size = 12),
        panel.margin.y = unit(0, "lines"),
        strip.text =  element_text(size = 16),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.05, color="grey85"),
        legend.text = element_text(size=28),
        legend.title = element_text(size=28),
        axis.title.y = element_text(size = 18,vjust = 10))

#### Asia
asia_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,continent,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  filter(continent == "Asia") %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T))

asia_data <- asia_data %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))



asia_exposure_impacts <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,country,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year,y=sum_var, group=sHDI_category))+
  geom_area()+
  geom_area(data=asia_data, aes(x=year,y=sum_var, group=sHDI_category, fill=sHDI_category))+
  facet_wrap(~name, scales = "free_y")+
  scale_color_manual("sHDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_fill_manual("sHDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+#xlab("")+
  facet_wrap(~name,scales = "free_y", nrow=1,
             labeller = labeller(name = c(`Exposed population` = "Exposed population \n(Billion people)",
                                          `Total affected` = "Total affected \n(Million people)",
                                          `Total mortality` = "Total mortality \n(Thousand people)",
                                          `Exposed GDP` = "Exposed GDP \n(Trillion $)",
                                          `Total damages` = "Total damages \n(Billion $)")))+
  scale_y_continuous(position = "right")+
  ylab("Values of exposure & impact variables")+xlab("")+
  theme_test()+
  theme(legend.position = "top",
        text = element_text(size=14),
        axis.text.x = element_text(size = 12,angle = 45, hjust=1),
        axis.text.y = element_text(size = 12),
        panel.margin.y = unit(0, "lines"),
        strip.text =  element_text(size = 16),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.05, color="grey85"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18),
        axis.title.y = element_text(size = 18,vjust = 10))

#### Europe
europe_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,continent,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  filter(continent == "Europe") %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T))

europe_data <- europe_data %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))


europe_exposure_impacts <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,country,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year,y=sum_var, group=sHDI_category))+
  geom_area()+
  geom_area(data=europe_data, aes(x=year,y=sum_var, group=sHDI_category, fill=sHDI_category))+
  facet_wrap(~name, scales = "free_y")+
  scale_color_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_fill_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+#xlab("")+
  facet_wrap(~name,scales = "free_y", nrow=1,
             labeller = labeller(name = c(`Exposed population` = "Exposed population \n(Billion people)",
                                          `Total affected` = "Total affected \n(Million people)",
                                          `Total mortality` = "Total mortality \n(Thousand people)",
                                          `Exposed GDP` = "Exposed GDP \n(Trillion $)",
                                          `Total damages` = "Total damages \n(Billion $)")))+
  scale_y_continuous(position = "right")+
  ylab("Values of exposure & impact variables")+xlab("")+
  theme_test()+
  theme(legend.position = "none",
        text = element_text(size=14),
        axis.text.x = element_text(size = 12,angle = 45, hjust=1),
        axis.text.y = element_text(size = 12),
        panel.margin.y = unit(0, "lines"),
        strip.text =  element_text(size = 16),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.05, color="grey85"),
        legend.text = element_text(size=28),
        legend.title = element_text(size=28),
        axis.title.y = element_text(size = 18,vjust = 10))

#### Americas
americas_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,continent,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  filter(continent == "Americas") %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T))

americas_data <- americas_data %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))


americas_exposure_impacts <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,country,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year,y=sum_var, group=sHDI_category))+
  geom_area()+
  geom_area(data=americas_data, aes(x=year,y=sum_var, group=sHDI_category, fill=sHDI_category))+
  facet_wrap(~name, scales = "free_y")+
  scale_color_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_fill_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+#xlab("")+
  facet_wrap(~name,scales = "free_y", nrow=1,
             labeller = labeller(name = c(`Exposed population` = "Exposed population \n(Billion people)",
                                          `Total affected` = "Total affected \n(Million people)",
                                          `Total mortality` = "Total mortality \n(Thousand people)",
                                          `Exposed GDP` = "Exposed GDP \n(Trillion $)",
                                          `Total damages` = "Total damages \n(Billion $)")))+
  scale_y_continuous(position = "right")+
  ylab("Values of exposure & impact variables")+xlab("")+
  theme_test()+
  theme(legend.position = "none",
        text = element_text(size=14),
        axis.text.x = element_text(size = 12,angle = 45, hjust=1),
        axis.text.y = element_text(size = 12),
        panel.margin.y = unit(0, "lines"),
        strip.text =  element_text(size = 16),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.05, color="grey85"),
        legend.text = element_text(size=28),
        legend.title = element_text(size=28),
        axis.title.y = element_text(size = 18,vjust = 10))

#### Oceania
oceania_data <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,continent,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  filter(continent == "Oceania") %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T))

oceania_data <- oceania_data %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3))



oceania_exposure_impacts <- disasters_df %>% 
  mutate(sHDI_category = factor(sHDI_category,levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")),
         disaster_type = factor(disaster_type,levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  select(c(disaster_type,sHDI_category,year,country,total_population, total_affected, total_deaths, total_gdp, total_damages)) %>%
  rename(`Exposed population`=total_population, `Total affected`=total_affected, `Total mortality`=total_deaths, `Exposed GDP`=total_gdp, `Total damages`=total_damages) %>% 
  pivot_longer(`Exposed population`:`Total damages`) %>% 
  mutate(name = factor(name,levels=c("Exposed population","Total affected","Total mortality","Exposed GDP","Total damages"))) %>% 
  group_by(sHDI_category,year,name) %>% 
  summarise(sum_var = sum(value, na.rm = T)) %>% 
  mutate(sum_var = case_when(name=="Exposed population" ~ sum_var / 1e9,
                             name=="Total affected" ~ sum_var / 1e6,
                             name=="Total mortality" ~ sum_var / 1e3,
                             name=="Exposed GDP" ~ sum_var / 1e12,
                             name=="Total damages" ~ sum_var / 1e9),
         sum_var = round(sum_var,3)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year,y=sum_var, group=sHDI_category))+
  geom_area()+
  geom_area(data=oceania_data, aes(x=year,y=sum_var, group=sHDI_category, fill=sHDI_category))+
  facet_wrap(~name, scales = "free_y")+
  scale_color_manual("sHDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_fill_manual("sHDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+#xlab("")+
  facet_wrap(~name,scales = "free_y", nrow=1,
             labeller = labeller(name = c(`Exposed population` = "Exposed population \n(Billion people)",
                                          `Total affected` = "Total affected \n(Million people)",
                                          `Total mortality` = "Total mortality \n(Thousand people)",
                                          `Exposed GDP` = "Exposed GDP \n(Trillion $)",
                                          `Total damages` = "Total damages \n(Billion $)")))+
  scale_y_continuous(position = "right")+
  ylab("Values of exposure & impact variables")+xlab("")+
  theme_test()+
  theme(legend.position = "none",
        text = element_text(size=14),
        axis.text.x = element_text(size = 12,angle = 45, hjust=1),
        axis.text.y = element_text(size = 12),
        panel.margin.y = unit(0, "lines"),
        strip.text =  element_text(size = 16),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.05, color="grey85"),
        legend.text = element_text(size=28),
        legend.title = element_text(size=28),
        axis.title.y = element_text(size = 18,vjust = 10))

all_continents_plot <- (asia_exposure_impacts / africa_exposure_impacts / europe_exposure_impacts /
                          americas_exposure_impacts / oceania_exposure_impacts & ylab(NULL)) + plot_annotation(tag_levels = 'a')


all_continents_plot <- wrap_elements(all_continents_plot) +
  labs(tag = "Values of exposure & impact variables") +
  theme(
    plot.tag = element_text(size = 18, angle = 90),
    plot.tag.position = "right"
  )

#Save final events & climatology plot
ggsave(
  "figures/supplementary_figures/S11_all_continents_plot.pdf",
  all_continents_plot,
  width = 18, height = 16, dpi = 150, units = "in", device=cairo_pdf)


print("###############################")
print("main figure S11 written to disk")
print("###############################")

# S12 - Evolution global and continental sHDI averages ------------------

print("###############################")
print("generating main figure S12")
print("###############################")

global_shdi <- disasters_df %>% 
  group_by(year) %>% 
  summarise(meanshdi = mean(subnational_hdi)) %>% 
  mutate(continent = "Global",
         ltype = "global")

continent_shdi <- disasters_df %>% 
  group_by(year,continent) %>% 
  summarise(meanshdi = mean(subnational_hdi)) %>% 
  mutate(ltype = "continent")

global_continent_shdi <- rbind(global_shdi,continent_shdi)

continent_hdi_evolution <- global_continent_shdi %>% 
  mutate(continent = factor(continent, levels = c("Global","Africa","Americas","Asia","Europe","Oceania"))) %>% 
  ggplot(aes(x=year, y=meanshdi, color=continent, linetype = ltype))+
  scale_color_manual("Continent",values = c("Global" = "black",
                                            "Africa" = "tomato",
                                            "Americas" = "deepskyblue1",
                                            "Asia" = "orange",
                                            "Europe" = "cadetblue3",
                                            "Oceania" = "chocolate"), drop = FALSE)+
  guides(linetype = "none")+
  ylim(c(0.25,1))+
  geom_line(lwd=1)+
  ylab("subnational HDI")+xlab("")+
  theme_test()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        panel.margin.y = unit(0, "lines"),
        strip.background =element_rect(fill="white"),
        panel.grid.major.y = element_line(size=.05, color="grey85"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        axis.title.y = element_text(size = 14))


ggsave(
  "figures/supplementary_figures/S12_continent_hdi_evolution.pdf",
  continent_hdi_evolution,
  width = 8, height = 6, dpi = 150, units = "in", device=cairo_pdf
)

print("###############################")
print("main figure S12 written to disk")
print("###############################")

# S13 - Evolution Country sHDI ------------------

print("###############################")
print("generating main figure S13")
print("###############################")

hdi_evolution <- disasters_df %>% 
  group_by(iso,year) %>% 
  summarise(meanshdi = median(subnational_hdi, na.rm=T)) %>% 
  summarise(min = min(meanshdi),
            max = max(meanshdi)) %>% 
  mutate(hdi_evolution = max - min) %>% 
  arrange(desc(hdi_evolution)) %>% 
  select(iso, hdi_evolution)

year_hdi <- disasters_df %>% 
  group_by(iso,year) %>% 
  summarise(meanshdi = median(subnational_hdi, na.rm=T))

continent_c <- disasters_df %>% select(c(iso, continent))

country_hdi_data <- left_join(year_hdi, hdi_evolution, by = "iso")
country_hdi_data <- left_join(country_hdi_data, continent_c, by = "iso")

country_hdi_data %>% 
  select(c(iso, hdi_evolution)) %>% 
  distinct() %>% 
  arrange(desc(hdi_evolution))


sHDI_rect <- data.frame(
  ymin = c(0,0.55,0.7,0.8),
  ymax = c(0.55,0.7,0.8,1),
  sHDI_cat = c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI")
)

developed_iso <- country_hdi_data %>% 
  select(c(iso, hdi_evolution,continent)) %>% 
  distinct() %>% 
  group_by(continent) %>%
  slice_max(order_by = hdi_evolution, n = 5) %>% 
  pull(iso)



hdi_evolution_plot <- country_hdi_data %>% 
  ggplot()+
  geom_line(aes(x=year, y=meanshdi, group=iso, color=continent),show.legend = T)+
  scale_color_manual("Continent",values = c("Africa" = "tomato",
                                            "Americas" = "deepskyblue1",
                                            "Asia" = "orange",
                                            "Europe" = "cadetblue3",
                                            "Oceania" = "chocolate"))+
  geom_hline(yintercept=0.55, linetype="dashed", color = medium_hdi_col, size=0.5)+
  geom_hline(yintercept=0.7, linetype="dashed", color = high_hdi_col, size=0.5)+
  geom_hline(yintercept=0.8, linetype="dashed", color = vhigh_hdi_col, size=0.5)+
  # scale_fill_manual("sHDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  xlab("")+ylab("subnational HDI")+
  gghighlight(iso %in% developed_iso, use_direct_label = T, label_params = list(lwd = 0.5, size=3),
              unhighlighted_params = list(lwd = 0.2, colour = alpha("black", 0.05)))+
  facet_wrap(~continent, scales = "free")+
  theme_test()+
  theme(#legend.position = "none",
    #legend.box="horizontal",
    text = element_text(size=14),
    axis.text.y.right = element_text(size = 5),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    #panel.margin.y = unit(0, "lines"),
    strip.background =element_rect(fill="white"),
    panel.grid.major.y = element_line(size=.05, color="grey85"),
    legend.text = element_text(size=14),
    legend.title = element_text(size=16),
    axis.title.y = element_text(size = 14))


ggsave(
  "figures/supplementary_figures/S13_hdi_evolution_plot.pdf",
  hdi_evolution_plot,
  width = 14, height = 8, dpi = 150, units = "in", device=cairo_pdf
)

country_df <- disasters_df %>% filter(iso %in% developed_iso) %>% 
  select(iso, country, continent) %>% 
  filter(continent == "Oceania") %>% 
  distinct() %>% 
  arrange(iso)

formatted_strings <- sprintf("%s (%s)", country_df$iso, country_df$country)
output_string <- paste(formatted_strings, collapse = ", ")
output_string

print("###############################")
print("main figure S13 written to disk")
print("###############################")

# S14 - Event counts per year and continent ------------------

print("###############################")
print("generating main figure S14")
print("###############################")

disasters_totals_count <- disasters_df %>% 
  filter(!is.na(sHDI_category)) %>% 
  mutate(sHDI_category = factor(sHDI_category, level=c("Very high sHDI", "High sHDI", "Medium sHDI","Low sHDI"))) %>% 
  group_by(year,continent) %>% 
  summarise(event_count = n())

disasters_peryear_continent_plot <- disasters_df %>% 
  filter(!is.na(sHDI_category)) %>% 
  mutate(sHDI_category = factor(sHDI_category, level=c("Very high sHDI", "High sHDI", "Medium sHDI","Low sHDI"))) %>% 
  group_by(sHDI_category,year) %>% 
  ggplot(aes(x=factor(year), fill=sHDI_category))+
  geom_bar(stat="count", position = "stack")+
  geom_text(stat = "count", aes(label = ..count.., y = ..count..),face="bold",size=3,color="grey95",position=position_stack(0.5))+
  geom_label(data = disasters_totals_count, aes(x=factor(year), y = event_count, label = event_count),size=4,vjust=-0.4,inherit.aes = FALSE)+
  scale_fill_manual("HDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  ylim(0,175)+
  xlab("")+ylab("Event count")+
  facet_wrap(~factor(continent, level=c("Africa","Asia","Americas","Europe","Oceania")), ncol = 1)+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))


ggsave("figures/supplementary_figures/S14_disasters_peryear_continent_plot.pdf",
       disasters_peryear_continent_plot,
       width = 10, height = 10, dpi = 150, units = "in", device='pdf')

print("###############################")
print("main figure S14 written to disk")
print("###############################")

# S15 -  Number events country sHDI ------------------

print("###############################")
print("generating main figure S15")
print("###############################")

disasters_country_count <- disasters_df %>% 
  filter(!is.na(sHDI_category)) %>% 
  mutate(sHDI_category = factor(sHDI_category, level=c("Very high sHDI", "High sHDI", "Medium sHDI","Low sHDI"))) %>% 
  group_by(iso) %>% 
  summarise(event_count = n())

country_counts <- data.frame(table(disasters_df[["iso"]]))
country_continents <- disasters_df %>% select(c(iso, continent)) %>% distinct() %>% rename(Var1 = iso)
country_counts <- left_join(country_counts,country_continents, by="Var1")


iso_events_continents_plot <- disasters_df %>% 
  filter(!is.na(sHDI_category)) %>% 
  mutate(sHDI_category = factor(sHDI_category, level=c("Very high sHDI", "High sHDI", "Medium sHDI","Low sHDI"))) %>% 
  group_by(sHDI_category,year) %>% 
  ggplot(aes(y=fct_rev(iso), fill=sHDI_category))+
  geom_bar(stat="count", position = "stack", width = 0.9)+
  geom_text(stat = "count", aes(label = ..count.., x = ..count..),size=2,color="white",face="bold",position=position_stack(0.5))+
  geom_text(data = country_counts, 
            aes(y = Var1, x = Freq+20, label = Freq), size=2,face="plain",
            inherit.aes = FALSE)+
  scale_fill_manual("sHDI group",values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col), drop = FALSE)+
  scale_x_continuous(limits = c(0,600), expand = c(0, 0))+
  xlab("Event count")+ylab("")+
  facet_col(~factor(continent, level=c("Africa","Asia","Americas","Europe","Oceania")), scales = 'free_y', space = 'free')+
  #facet_col(~continent,scales = "free_x", space = "free")+
  #facet_wrap(~continent, scales = "free")+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=5.5, vjust = 0.5, hjust=1),
        strip.text = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        panel.grid.major.x = element_line(color = "grey80",size = 0.5,linetype = 1),
        strip.text.x = element_text(hjust = 1, margin=margin(l=0)),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))



ggsave("figures/supplementary_figures/S15_iso_events_continents_plot.pdf",
       iso_events_continents_plot,
       width = 8, height = 12, dpi = 150, units = "in", device='pdf')

print("###############################")
print("main figure S15 written to disk")
print("###############################")

# S16 - Number events country disaster type ------------------

print("###############################")
print("generating main figure S16")
print("###############################")

iso_events_distype_continents_plot <- disasters_df %>% 
  filter(!is.na(sHDI_category)) %>% 
  mutate(disaster_type = factor(disaster_type, levels = c("Flood", "Storm", "Landslide", "Cold wave", "Heat wave", "Wildfire", "Drought"))) %>% 
  group_by(sHDI_category,year) %>% 
  ggplot(aes(y=fct_rev(iso), fill=disaster_type))+
  geom_bar(stat="count", position = "stack", width = 0.9)+
  geom_text(stat = "count", aes(label = ..count.., x = ..count..),size=2,color="white",face="bold",position=position_stack(0.5))+
  geom_text(data = country_counts, 
            aes(y = Var1, x = Freq+20, label = Freq), size=2,face="plain",
            inherit.aes = FALSE)+
  scale_fill_manual("Disaster \nType",values = c("Flood" = flood_color, "Storm" = storm_color, "Landslide" = landslide_color,"Heat wave"=heatwave_color,"Drought"=drought_color,"Wildfire"=wildfire_color,
                                                 "Cold wave"=coldwave_color),drop =FALSE)+
  scale_x_continuous(limits = c(0,600), expand = c(0, 0))+
  xlab("Event count")+ylab("")+
  facet_col(~factor(continent, level=c("Africa","Asia","Americas","Europe","Oceania")), scales = 'free_y', space = 'free')+
  #facet_col(~continent,scales = "free_x", space = "free")+
  #facet_wrap(~continent, scales = "free")+
  mytheme_wlegend+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=5.5, vjust = 0.5, hjust=1),
        strip.text = element_text(size = 10),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        panel.grid.major.x = element_line(color = "grey80",size = 0.5,linetype = 1),
        strip.text.x = element_text(hjust = 1, margin=margin(l=0)),
        panel.background = element_rect(fill = "grey99", color = NA),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))


ggsave("figures/supplementary_figures/S16_iso_events_distype_continents_plot.pdf",
       iso_events_distype_continents_plot,
       width = 8, height = 12, dpi = 150, units = "in", device='pdf')

print("###############################")
print("main figure S16 written to disk")
print("###############################")

# S17 - Disaster geographic distribution & sHDI inequality ------------------

print("###############################")
print("generating main figure S17")
print("###############################")

print("###############################")
print("main figure S17 written to disk")
print("###############################")