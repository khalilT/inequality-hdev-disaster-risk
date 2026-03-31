### (c) Khalil Teber 2024-2025 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"


### This code defines the functions used for plotting and for the analysis


###########################################################################
# Map Plotting functions --------------------------------------------------
###########################################################################

#plotting function
make_map_plot <- function(distype){
  
  data_plot <- disasters_df %>% filter(disaster_type == distype)
  data_plot <- st_as_sf(data_plot, coords = c("lon", "lat"), crs = 4326)
  coords <- st_coordinates(data_plot)
  # Add the extracted coordinates as new columns to the original dataframe
  data_plot$lon <- coords[, 1]  # Longitude
  data_plot$lat <- coords[, 2]  # Latitude
  
  
  number_events <- data_plot %>% nrow()
  
  map_disaster_plot <-
    data_plot %>% filter(disaster_type == distype) %>%
    mutate(sHDI_category = str_replace(sHDI_category,'Low sHDI', 'Low'),
           sHDI_category = str_replace(sHDI_category,'Medium sHDI', 'Medium'),
           sHDI_category = str_replace(sHDI_category,'High sHDI', 'High'),
           sHDI_category = str_replace(sHDI_category,'Very high sHDI', 'V. high'),
           sHDI_category = factor(sHDI_category, levels = c("Low", "Medium", "High", "V. high"))
    ) %>%
    ggplot()+
    ggplot2::geom_sf(data = shp_countries,lwd = 0.2, color="grey90", fill="grey97")+
    ggplot2::geom_sf(data = coastlines,lwd = 0.1, color="grey10")+
    geom_point(aes(color=sHDI_category,group=sHDI_category,geometry = geometry),
               alpha=(log2(number_events)/sqrt(number_events)),position = "jitter",stat = "sf_coordinates",show.legend = F) +
    scale_color_manual(values = c("Low" = low_hdi_col, "Medium" = medium_hdi_col, "High" = high_hdi_col, "V. high"=vhigh_hdi_col), drop = FALSE)+
    coord_sf(crs = st_crs('ESRI:54030'))+
    cowplot::theme_minimal_grid()+
    ggtitle(paste(distype,'-', number_events," events"))+xlab("")+ylab("")+
    theme(plot.margin = unit(c(0, 0, 0, 0), "mm"),
          plot.title = element_text(size = 18, hjust = 0.5, vjust=-0.5, face = "plain"),
          text = element_text(size = 20),
          legend.direction = "horizontal",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size =  20),
          legend.title = element_text(size = 20, face = "bold"))
  
  #plot_test2 <- ggMarginal(plot_test2,margins="y",size=8,fill="transparent")
  
  ydens <- data_plot %>%
    ggplot()+
    geom_density(aes(y = lat),n=256,adjust=0.2)+
    ylim(-85,85)+
    theme_void()
  
  return(ggdraw(insert_yaxis_grob(map_disaster_plot, ydens, grid::unit(.1, "null"), position = "right")))
}

### Legend
make_legend <- function(){
  data_plot <- disasters_df %>% slice(1:100)
  data_plot <- st_as_sf(data_plot, coords = c("lon", "lat"), crs = 4326)
  coords <- st_coordinates(data_plot)
  # Add the extracted coordinates as new columns to the original dataframe
  data_plot$lon <- coords[, 1]  # Longitude
  data_plot$lat <- coords[, 2]  # Latitude
  
  map_legend <-
    data_plot %>%
    mutate(sHDI_category = str_replace(sHDI_category,'Low sHDI', 'Low'),
           sHDI_category = str_replace(sHDI_category,'Medium sHDI', 'Medium'),
           sHDI_category = str_replace(sHDI_category,'High sHDI', 'High'),
           sHDI_category = str_replace(sHDI_category,'Very high sHDI', 'V. high'),
           sHDI_category = factor(sHDI_category, levels = c("Low", "Medium", "High", "V. high"))
    ) %>%
    ggplot()+
    ggplot2::geom_sf(data = coastlines,lwd = 0.2, color="grey70")+
    geom_point(aes(color=sHDI_category,group=sHDI_category,geometry = geometry),
               alpha=1,position = "jitter",stat = "sf_coordinates") +
    scale_color_manual(values = c("Low" = low_hdi_col, "Medium" = medium_hdi_col, "High" = high_hdi_col, "V. high"=vhigh_hdi_col),
                       drop = FALSE)+
    coord_sf(crs = st_crs('ESRI:54030'))+
    cowplot::theme_minimal_grid()+
    xlab("")+ylab("")+
    guides(color = guide_legend("sHDI group",title.position = "top",
                                label.position = "bottom",
                                nrow = 1,override.aes = list(size=8, alpha=0.9)))+
    guides(size = guide_legend("Affected fraction",title.position = "top",
                               label.position = "bottom",
                               nrow = 1))+
    theme(plot.margin = unit(c(0, 0, 0, 0), "mm"),
          panel.spacing = unit(0, "lines"),
          plot.subtitle = element_text(vjust = -9, hjust = 0.5),
          text = element_text(size = 12),
          legend.direction = "horizontal",
          legend.key.size = unit(0.45, "cm"),
          legend.text = element_text(size =  14),
          legend.title = element_text(size = 16, face = "bold", hjust = 0))
          #legend.title.align=0)
  
  ggdraw()+
    draw_plot(get_legend(map_legend),x = .33, y = 0)
}


###########################################################################
# Plot functions supplementary figures
###########################################################################

plot_dist <- function(dis_type,xvar,yvar,xlim_value,ylim_value,xvar_lab,yvar_lab,ptitle){
  
  data_plot <- disasters_df %>% filter((disaster_type == dis_type)) %>% 
    mutate(sHDI_category = factor(sHDI_category, levels=c("Low sHDI","Medium sHDI","High sHDI","Very high sHDI"))) %>% 
    mutate(overall = dis_type)
  
  pmain <-  data_plot %>% 
    ggplot(aes(x=eval(sym(xvar)), y=eval(sym(yvar)), color=sHDI_category)) + 
    geom_point(show.legend = FALSE, pch=16, size=2.5, alpha=0.5) + 
    scale_color_manual(name = 'legend', 
                       values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col),
                       drop = FALSE)+
    xlim(xlim_value[1],xlim_value[2])+ylim(ylim_value[1],ylim_value[2])+
    ggtitle(ptitle)+
    xlab(xvar_lab) +
    ylab(yvar_lab) +
    theme(plot.title = element_text(face = "plain", size=14),
          plot.title.position = "plot",
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_blank(), 
          panel.grid.major = element_line(colour = "grey80", size = 0.45))
  #panel.grid.minor = element_line(colour = "black", size = 0.125))
  
  xbox <- axis_canvas(pmain, axis = "x", coord_flip = TRUE) + 
    geom_boxplot(data = data_plot, aes(y = eval(sym(xvar)), x = factor(sHDI_category), fill = factor(sHDI_category)),width=0.75) + 
    scale_fill_manual(name = 'legend', 
                      values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col),
                      drop = FALSE)+
    scale_x_discrete() + coord_flip()
  ybox <- axis_canvas(pmain, axis = "y") + 
    geom_boxplot(data = data_plot, aes(y = eval(sym(yvar)), x = factor(sHDI_category), fill = factor(sHDI_category)),width=0.75) +
    scale_fill_manual(name = 'legend', 
                      values = c("Low sHDI" = low_hdi_col, "Medium sHDI" = medium_hdi_col, "High sHDI" = high_hdi_col, "Very high sHDI"=vhigh_hdi_col),
                      drop = FALSE)+
    scale_x_discrete()
  
  p1 <- insert_xaxis_grob(pmain, xbox, grid::unit(0.9, "in"), position = "top")
  p2 <- insert_yaxis_grob(p1, ybox, grid::unit(0.9, "in"), position = "right")
  
  return(ggdraw(p2))
}

###########################################################################
# Between sHDI Odds ratio calculations ------------------------------------
###########################################################################

# nodes for parallel computations
n_nodes = min(50, parallel::detectCores()-1)

### Calculate odds ratio per sHDI group across all disasters
### calculates OR for all 3 impact variables

boot_odds_impact_all <- function(impact_var){
  print(paste("calculate ORs per sHDI group across all disaster types for ", impact_var, " variable"))
  #sample data
  sample_disasters_df <- disasters_df %>%
    filter(!is.na(impact_var) & impact_var > 0) %>% 
    mutate(sHDI_category = factor(sHDI_category),deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_category = relevel(sHDI_category, "Very high sHDI")) %>% 
    dplyr::select(c(impact_var,sHDI_category, deviation_category,disaster_type,iso,continent)) %>% drop_na()
  
  #logistic regression
  logit_test <- function(d,indices) {
    d <- d[indices,]
    f <- paste(impact_var,"~","sHDI_category")
    fit <- glm(f, data = d, family = "binomial")
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(
    data = sample_disasters_df,
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disasters_df$sHDI_category)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disasters_df %>%
    group_by(sHDI_category) %>%
    summarise(nevents = n())
  
  
  low_hdi_count <- event_counts %>%
    filter(sHDI_category == "Low sHDI" | sHDI_category == "Very high sHDI") %>%
    mutate(npoints = sum(nevents), case = "Low sHDI : Very high sHDI") %>%
    select(case, npoints) %>% distinct()
  
  medium_hdi_count <- event_counts %>%
    filter(sHDI_category == "Medium sHDI" | sHDI_category == "Very high sHDI") %>%
    mutate(npoints = sum(nevents), case = "Medium sHDI : Very high sHDI") %>%
    select(case, npoints) %>% distinct()
  
  high_hdi_count <- event_counts %>%
    filter(sHDI_category == "High sHDI" | sHDI_category == "Very high sHDI") %>%
    mutate(npoints = sum(nevents), case = "High sHDI : Very high sHDI") %>%
    select(case, npoints) %>% distinct()
  
  point_counts <- rbind(low_hdi_count, medium_hdi_count, high_hdi_count)
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = "All types") %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("sHDI_category", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high sHDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>%
    mutate(shdi_category = case) %>%
    separate(shdi_category,sep = ":",into=c("sHDI_category","case2")) %>%
    select(-case2)
  
  res_all_df <- left_join(res_all_df,point_counts, by="case" )
  return(res_all_df)
}

### Calculate OR likelihood of being affected per sHDI group & disaster type
boot_odds_affected <- function(distype){
  print(paste("calculate ORs risk affected, per sHDI group for ", distype, " disaster type"))
  #sample data
  sample_disaster_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_affected) & normalized_affected > 0) %>% 
    mutate(sHDI_category = factor(sHDI_category),deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_category = relevel(sHDI_category, "Very high sHDI")) %>% 
    dplyr::select(c(normalized_affected,sHDI_category, deviation_category,disaster_type,iso,continent)) %>% drop_na()
  
  #logistic regression
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    fit <- glm(normalized_affected~sHDI_category, data = d, family = "binomial")  
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(  
    data = sample_disaster_hdi, 
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disaster_hdi$sHDI_category)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disaster_hdi %>% 
    group_by(sHDI_category) %>% 
    summarise(nevents = n())
  
  
  low_hdi_count <- event_counts %>% 
    filter(sHDI_category == "Low sHDI" | sHDI_category == "Very high sHDI") %>% 
    mutate(npoints = sum(nevents), case = "Low sHDI : Very high sHDI") %>% 
    select(case, npoints) %>% distinct()
  
  medium_hdi_count <- event_counts %>% 
    filter(sHDI_category == "Medium sHDI" | sHDI_category == "Very high sHDI") %>% 
    mutate(npoints = sum(nevents), case = "Medium sHDI : Very high sHDI") %>% 
    select(case, npoints) %>% distinct()
  
  high_hdi_count <- event_counts %>% 
    filter(sHDI_category == "High sHDI" | sHDI_category == "Very high sHDI") %>% 
    mutate(npoints = sum(nevents), case = "High sHDI : Very high sHDI") %>% 
    select(case, npoints) %>% distinct()
  
  point_counts <- rbind(low_hdi_count, medium_hdi_count, high_hdi_count)
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = distype) %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("sHDI_category", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high sHDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>% 
    mutate(shdi_category = case) %>% 
    separate(shdi_category,sep = ":",into=c("sHDI_category","case2")) %>% 
    select(-case2)
  
  res_all_df <- left_join(res_all_df,point_counts, by="case" )
  return(res_all_df)
}

### Select cases that have at least 30 data points representing all sHDI groups
consider_disaster_affected <- function(distype){
  disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_affected) & normalized_affected > 0) %>% 
    dplyr::select(c(normalized_affected,sHDI_category,disaster_type,iso,continent)) %>% drop_na() %>% 
    group_by(sHDI_category) %>% 
    tally() %>% summarise(nrow = nrow(.)==4, sumrows = sum(n) >= 30) %>% 
    mutate(case = nrow*sumrows) %>% pull(case)
}

### Calculate OR likelihood of fatalities per sHDI group & disaster type
boot_odds_killed <- function(distype){
  print(paste("calculate ORs risk fatalities, per sHDI group for ", distype, " disaster type"))
  #sample data  
  sample_disaster_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_deaths) & normalized_deaths > 0) %>% 
    mutate(sHDI_category = factor(sHDI_category),deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_category = relevel(sHDI_category, "Very high sHDI")) %>% 
    dplyr::select(c(normalized_deaths,sHDI_category, deviation_category,disaster_type,iso,continent)) %>% drop_na()
  
  #logistic regression
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    fit <- glm(normalized_deaths ~ sHDI_category, data = d, family = "binomial")  
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(  
    data = sample_disaster_hdi, 
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disaster_hdi$sHDI_category)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disaster_hdi %>% 
    group_by(sHDI_category) %>% 
    summarise(nevents = n())
  
  
  low_hdi_count <- event_counts %>% 
    filter(sHDI_category == "Low sHDI" | sHDI_category == "Very high sHDI") %>% 
    mutate(npoints = sum(nevents), case = "Low sHDI : Very high sHDI") %>% 
    select(case, npoints) %>% distinct()
  
  medium_hdi_count <- event_counts %>% 
    filter(sHDI_category == "Medium sHDI" | sHDI_category == "Very high sHDI") %>% 
    mutate(npoints = sum(nevents), case = "Medium sHDI : Very high sHDI") %>% 
    select(case, npoints) %>% distinct()
  
  high_hdi_count <- event_counts %>% 
    filter(sHDI_category == "High sHDI" | sHDI_category == "Very high sHDI") %>% 
    mutate(npoints = sum(nevents), case = "High sHDI : Very high sHDI") %>% 
    select(case, npoints) %>% distinct()
  
  point_counts <- rbind(low_hdi_count, medium_hdi_count, high_hdi_count)
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = distype) %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("sHDI_category", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high sHDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>% 
    mutate(shdi_category = case) %>% 
    separate(shdi_category,sep = ":",into=c("sHDI_category","case2")) %>% 
    select(-case2)
  
  res_all_df <- left_join(res_all_df,point_counts, by="case" )
  return(res_all_df)
}

### Select cases that have at least 30 data points representing all sHDI groups
consider_disaster_killed <- function(distype){
  disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_deaths) & normalized_deaths > 0) %>% 
    dplyr::select(c(normalized_deaths,sHDI_category,disaster_type,iso,continent)) %>% drop_na() %>% 
    group_by(sHDI_category) %>% 
    tally() %>% summarise(nrow = nrow(.)==4, sumrows = sum(n) >= 30) %>% 
    mutate(case = nrow*sumrows) %>% pull(case)
}

### Calculate OR likelihood of economic losses per sHDI group & disaster type
boot_odds_damages <- function(distype){
  print(paste("calculate ORs risk econ. losses, per sHDI group for ", distype, " disaster type"))
  #sample data 
  sample_disaster_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_total_damages) & normalized_total_damages > 0) %>% 
    mutate(sHDI_category = factor(sHDI_category),deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_category = relevel(sHDI_category, "Very high sHDI")) %>% 
    dplyr::select(c(normalized_total_damages,sHDI_category, deviation_category,disaster_type,iso,continent)) %>% drop_na()
  
  #logistic regression
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    fit <- glm(normalized_total_damages ~ sHDI_category, data = d, family = "binomial")  
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(  
    data = sample_disaster_hdi, 
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disaster_hdi$sHDI_category)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disaster_hdi %>% 
    group_by(sHDI_category) %>% 
    summarise(nevents = n())
  
  
  low_hdi_count <- event_counts %>% 
    filter(sHDI_category == "Low sHDI" | sHDI_category == "Very high sHDI") %>% 
    mutate(npoints = sum(nevents), case = "Low sHDI : Very high sHDI") %>% 
    select(case, npoints) %>% distinct()
  
  medium_hdi_count <- event_counts %>% 
    filter(sHDI_category == "Medium sHDI" | sHDI_category == "Very high sHDI") %>% 
    mutate(npoints = sum(nevents), case = "Medium sHDI : Very high sHDI") %>% 
    select(case, npoints) %>% distinct()
  
  high_hdi_count <- event_counts %>% 
    filter(sHDI_category == "High sHDI" | sHDI_category == "Very high sHDI") %>% 
    mutate(npoints = sum(nevents), case = "High sHDI : Very high sHDI") %>% 
    select(case, npoints) %>% distinct()
  
  point_counts <- rbind(low_hdi_count, medium_hdi_count, high_hdi_count)
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = distype) %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("sHDI_category", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high sHDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>% 
    mutate(shdi_category = case) %>% 
    separate(shdi_category,sep = ":",into=c("sHDI_category","case2")) %>% 
    select(-case2)
  
  res_all_df <- left_join(res_all_df,point_counts, by="case")
  return(res_all_df)
}

### Select cases that have at least 30 data points representing all sHDI groups
consider_disaster_damages <- function(distype){
  disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_total_damages) & normalized_total_damages > 0) %>% 
    dplyr::select(c(normalized_total_damages,sHDI_category,disaster_type,iso,continent)) %>% drop_na() %>% 
    group_by(sHDI_category) %>% 
    tally() %>% summarise(nrow = nrow(.)==4, sumrows = sum(n) >= 30) %>% 
    mutate(case = nrow*sumrows) %>% pull(case)
}


###########################################################################
# Deviation OR functions --------------------------------------------------
###########################################################################


### calculate OR across all impact variables, sHDI groups and deviation levels
boot_odds_inequality_impacts_all <- function(impact_var){
  print(paste("calculate ORs per sHDI group and HDI deviation subgroups across all types for ", impact_var, " variable"))
  sample_vhigh_hdi <- disasters_df %>% 
    filter(!is.na(eval(sym(impact_var))) & eval(sym(impact_var)) > 0 & sHDI_category == "Very high sHDI") %>% 
    mutate(sHDI_category = factor(sHDI_category),
           deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_deviation = factor(sHDI_category)) %>% 
    dplyr::select(c(sym(impact_var),sHDI_category,
                    deviation_category,sHDI_deviation,disaster_type,iso,continent)) %>% drop_na()
  
  
  sample_disaster_hdi <- disasters_df %>% 
    filter(!is.na(eval(sym(impact_var))) & eval(sym(impact_var)) > 0 & sHDI_category != "Very high sHDI") %>% 
    mutate(sHDI_category = factor(sHDI_category),
           deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_deviation = paste(sHDI_category,"-",deviation_category),
           sHDI_deviation = factor(sHDI_deviation)) %>% 
    dplyr::select(c(sym(impact_var),sHDI_category,
                    deviation_category,sHDI_deviation,disaster_type,iso,continent)) %>% drop_na()
  
  sample_disaster_hdi <- rbind(sample_disaster_hdi, sample_vhigh_hdi)
  
  sample_disaster_hdi <- sample_disaster_hdi %>% 
    mutate(sHDI_deviation = relevel(sHDI_deviation, "Very high sHDI"))
  
  #logistic regression
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    f <- paste(impact_var,"~","sHDI_deviation")
    fit <- glm(f, data = d, family = "binomial")  
    return(coef(fit))
  }
  
  
  #logistic regression
  boot_fit <- boot::boot(  
    data = sample_disaster_hdi, 
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disaster_hdi$sHDI_deviation)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  # count points per case
  event_counts <- sample_disaster_hdi %>% 
    group_by(sHDI_deviation) %>% 
    summarise(nevents = n())
  
  # count points per case
  npoints_cases = data.frame()
  for (case in event_counts$sHDI_deviation[2:12]) {
    
    single_case <- event_counts %>% 
      filter(sHDI_deviation == case | sHDI_deviation == "Very high sHDI") %>% 
      mutate(npoints = sum(nevents), case = paste(case,":","Very high sHDI")) %>% 
      select(case, npoints) %>% distinct() %>% as.data.frame()
    
    npoints_cases <- rbind(npoints_cases,single_case)}
  
  res_all_df = data.frame()
  
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = "All types") %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("sHDI_deviation", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high sHDI"))
    res_all_df <- rbind(res_all_df,res_df)
  }
  
  res_all_df <- res_all_df %>% 
    mutate(hdi_deviation = case) %>% 
    separate(hdi_deviation,sep = ":",into=c("case1","case2")) %>% 
    select(-case2) %>% 
    separate(case1,sep = " - ",into=c("sHDI_category","deviation_category"))
  
  
  res_all_df <- left_join(res_all_df,npoints_cases, by="case" )
  
  return(res_all_df) 
}

### calculate OR likelihood of being affected, sHDI groups and deviation levels
boot_odds_inequality_affected_all <- function(distype){
  print(paste("calculate ORs risk affected, per sHDI group and HDI deviation subgroups for ", distype, " disaster type"))
  #sample data
  sample_vhigh_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_affected) & normalized_affected > 0 & sHDI_category == "Very high sHDI") %>% 
    mutate(sHDI_category = factor(sHDI_category),
           deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_deviation = factor(sHDI_category)) %>% 
    dplyr::select(c(normalized_affected,sHDI_category, deviation_category,sHDI_deviation,disaster_type,iso,continent)) %>% drop_na()
  
  
  sample_disaster_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_affected) & normalized_affected > 0 & sHDI_category != "Very high sHDI") %>% 
    mutate(sHDI_category = factor(sHDI_category),
           deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_deviation = paste(sHDI_category,"-",deviation_category),
           sHDI_deviation = factor(sHDI_deviation)) %>% 
    dplyr::select(c(normalized_affected,sHDI_category, deviation_category,sHDI_deviation,disaster_type,iso,continent)) %>% drop_na()
  
  sample_disaster_hdi <- rbind(sample_disaster_hdi, sample_vhigh_hdi)
  
  sample_disaster_hdi <- sample_disaster_hdi %>% 
    mutate(sHDI_deviation = relevel(sHDI_deviation, "Very high sHDI"))
  
  #logistic regression
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    fit <- glm(normalized_affected~sHDI_deviation, data = d, family = "binomial")  
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(  
    data = sample_disaster_hdi, 
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disaster_hdi$sHDI_deviation)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disaster_hdi %>% 
    group_by(sHDI_deviation) %>% 
    summarise(nevents = n())
  
  
  # count points per case
  npoints_cases = data.frame()
  for (case in event_counts$sHDI_deviation[2:12]) {
    
    single_case <- event_counts %>% 
      filter(sHDI_deviation == case | sHDI_deviation == "Very high sHDI") %>% 
      mutate(npoints = sum(nevents), case = paste(case,":","Very high sHDI")) %>% 
      select(case, npoints) %>% distinct() %>% as.data.frame()
    
    npoints_cases <- rbind(npoints_cases,single_case)}
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = distype) %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("sHDI_deviation", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high sHDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>% 
    mutate(hdi_deviation = case) %>% 
    separate(hdi_deviation,sep = ":",into=c("case1","case2")) %>% 
    select(-case2) %>% 
    separate(case1,sep = " - ",into=c("sHDI_category","deviation_category"))
  
  
  res_all_df <- left_join(res_all_df,npoints_cases, by="case" )
  return(res_all_df)
}

### Select cases that have at least 30 data points representing all sHDI groups
consider_disaster_sHDIdeviation_affected <- function(distype){
  disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_affected)) %>% 
    dplyr::select(c(normalized_affected,sHDI_category,deviation_category,disaster_type,iso,continent)) %>% drop_na() %>% 
    mutate(sHDI_deviation = paste(sHDI_category,"-",deviation_category)) %>% 
    group_by(sHDI_deviation) %>% 
    tally() %>% summarise(nrow = nrow(.)==12, sumrows = sum(n) >= 30) %>% 
    mutate(case = nrow*sumrows) %>% pull(case)
}


### calculate OR likelihood of fatalities, sHDI groups and deviation levels
boot_odds_inequality_fatalities_all <- function(distype){
  print(paste("calculate ORs risk fatalities, per sHDI group and HDI deviation subgroups for ", distype, " disaster type"))
  #sample data
  sample_vhigh_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_deaths) & normalized_deaths > 0 & sHDI_category == "Very high sHDI") %>% 
    mutate(sHDI_category = factor(sHDI_category),
           deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_deviation = factor(sHDI_category)) %>% 
    dplyr::select(c(normalized_deaths,sHDI_category, deviation_category,sHDI_deviation,disaster_type,iso,continent)) %>% drop_na()
  
  
  sample_disaster_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_deaths) & normalized_deaths > 0 & sHDI_category != "Very high sHDI") %>% 
    mutate(sHDI_category = factor(sHDI_category),
           deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_deviation = paste(sHDI_category,"-",deviation_category),
           sHDI_deviation = factor(sHDI_deviation)) %>% 
    dplyr::select(c(normalized_deaths,sHDI_category, deviation_category,sHDI_deviation,disaster_type,iso,continent)) %>% drop_na()
  
  sample_disaster_hdi <- rbind(sample_disaster_hdi, sample_vhigh_hdi)
  
  sample_disaster_hdi <- sample_disaster_hdi %>% 
    mutate(sHDI_deviation = relevel(sHDI_deviation, "Very high sHDI"))
  
  #logistic regression
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    fit <- glm(normalized_deaths~sHDI_deviation, data = d, family = "binomial")  
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(  
    data = sample_disaster_hdi, 
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disaster_hdi$sHDI_deviation)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disaster_hdi %>% 
    group_by(sHDI_deviation) %>% 
    summarise(nevents = n())
  
  
  # count points per case
  npoints_cases = data.frame()
  for (case in event_counts$sHDI_deviation[2:12]) {
    
    single_case <- event_counts %>% 
      filter(sHDI_deviation == case | sHDI_deviation == "Very high sHDI") %>% 
      mutate(npoints = sum(nevents), case = paste(case,":","Very high sHDI")) %>% 
      select(case, npoints) %>% distinct() %>% as.data.frame()
    
    npoints_cases <- rbind(npoints_cases,single_case)}
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = distype) %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("sHDI_deviation", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high sHDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>% 
    mutate(hdi_deviation = case) %>% 
    separate(hdi_deviation,sep = ":",into=c("case1","case2")) %>% 
    select(-case2) %>% 
    separate(case1,sep = " - ",into=c("sHDI_category","deviation_category"))
  
  
  res_all_df <- left_join(res_all_df,npoints_cases, by="case" )
  return(res_all_df)
}

### Select cases that have at least 30 data points representing all sHDI groups
consider_disaster_sHDIdeviation_fatalities <- function(distype){
  disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_deaths)) %>% 
    dplyr::select(c(normalized_deaths,sHDI_category,deviation_category,disaster_type,iso,continent)) %>% drop_na() %>% 
    mutate(sHDI_deviation = paste(sHDI_category,"-",deviation_category)) %>% 
    group_by(sHDI_deviation) %>% 
    tally() %>% summarise(nrow = nrow(.)==12, sumrows = sum(n) >= 30) %>% 
    mutate(case = nrow*sumrows) %>% pull(case)
}

### calculate OR likelihood of economci losses, sHDI groups and deviation levels
boot_odds_inequality_damages_all <- function(distype){
  print(paste("calculate ORs risk econ. losses, per sHDI group and HDI deviation subgroups for ", distype, " disaster type"))
  #sample data
  sample_vhigh_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_total_damages) & normalized_total_damages > 0 & sHDI_category == "Very high sHDI") %>% 
    mutate(sHDI_category = factor(sHDI_category),
           deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_deviation = factor(sHDI_category)) %>% 
    dplyr::select(c(normalized_total_damages,sHDI_category, deviation_category,sHDI_deviation,disaster_type,iso,continent)) %>% drop_na()
  
  
  sample_disaster_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_total_damages) & normalized_total_damages > 0 & sHDI_category != "Very high sHDI") %>% 
    mutate(sHDI_category = factor(sHDI_category),
           deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           sHDI_deviation = paste(sHDI_category,"-",deviation_category),
           sHDI_deviation = factor(sHDI_deviation)) %>% 
    dplyr::select(c(normalized_total_damages,sHDI_category, deviation_category,sHDI_deviation,disaster_type,iso,continent)) %>% drop_na()
  
  sample_disaster_hdi <- rbind(sample_disaster_hdi, sample_vhigh_hdi)
  
  sample_disaster_hdi <- sample_disaster_hdi %>% 
    mutate(sHDI_deviation = relevel(sHDI_deviation, "Very high sHDI"))
  
  #logistic regression
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    fit <- glm(normalized_total_damages~sHDI_deviation, data = d, family = "binomial")  
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(  
    data = sample_disaster_hdi, 
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disaster_hdi$sHDI_deviation)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disaster_hdi %>% 
    group_by(sHDI_deviation) %>% 
    summarise(nevents = n())
  
  
  # count points per case
  npoints_cases = data.frame()
  for (case in event_counts$sHDI_deviation[2:12]) {
    
    single_case <- event_counts %>% 
      filter(sHDI_deviation == case | sHDI_deviation == "Very high sHDI") %>% 
      mutate(npoints = sum(nevents), case = paste(case,":","Very high sHDI")) %>% 
      select(case, npoints) %>% distinct() %>% as.data.frame()
    
    npoints_cases <- rbind(npoints_cases,single_case)}
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = distype) %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("sHDI_deviation", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high sHDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>% 
    mutate(hdi_deviation = case) %>% 
    separate(hdi_deviation,sep = ":",into=c("case1","case2")) %>% 
    select(-case2) %>% 
    separate(case1,sep = " - ",into=c("sHDI_category","deviation_category"))
  
  
  res_all_df <- left_join(res_all_df,npoints_cases, by="case" )
  return(res_all_df)
}

### Select cases that have at least 30 data points representing all sHDI groups
consider_disaster_sHDIdeviation_damages <- function(distype){
  disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_total_damages)) %>% 
    dplyr::select(c(normalized_total_damages,sHDI_category,deviation_category,disaster_type,iso,continent)) %>% drop_na() %>% 
    mutate(sHDI_deviation = paste(sHDI_category,"-",deviation_category)) %>% 
    group_by(sHDI_deviation) %>% 
    tally() %>% summarise(nrow = nrow(.)==12, sumrows = sum(n) >= 30) %>% 
    mutate(case = nrow*sumrows) %>% pull(case)
}


###########################################################################
# National HDI analysis ---------------------------------------------------
###########################################################################

boot_odds_impact_nationalHDI_all <- function(impact_var){
  print(paste("calculate ORs per National HDI group across all disaster types for ", impact_var, " variable"))
  #sample data
  sample_disasters_df <- disasters_df %>%
    filter(!is.na(impact_var) & impact_var > 0) %>% 
    mutate(natHDI_category = factor(natHDI_category),deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           natHDI_category = relevel(natHDI_category, "Very high HDI")) %>% 
    dplyr::select(c(impact_var,natHDI_category, deviation_category,disaster_type,iso,continent)) %>% drop_na()
  
  #logistic regression
  logit_test <- function(d,indices) {
    d <- d[indices,]
    f <- paste(impact_var,"~","natHDI_category")
    fit <- glm(f, data = d, family = "binomial")
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(
    data = sample_disasters_df,
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disasters_df$natHDI_category)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disasters_df %>%
    group_by(natHDI_category) %>%
    summarise(nevents = n())
  
  
  low_hdi_count <- event_counts %>%
    filter(natHDI_category == "Low HDI" | natHDI_category == "Very high HDI") %>%
    mutate(npoints = sum(nevents), case = "Low HDI : Very high HDI") %>%
    select(case, npoints) %>% distinct()
  
  medium_hdi_count <- event_counts %>%
    filter(natHDI_category == "Medium HDI" | natHDI_category == "Very high HDI") %>%
    mutate(npoints = sum(nevents), case = "Medium HDI : Very high HDI") %>%
    select(case, npoints) %>% distinct()
  
  high_hdi_count <- event_counts %>%
    filter(natHDI_category == "High HDI" | natHDI_category == "Very high HDI") %>%
    mutate(npoints = sum(nevents), case = "High HDI : Very high HDI") %>%
    select(case, npoints) %>% distinct()
  
  point_counts <- rbind(low_hdi_count, medium_hdi_count, high_hdi_count)
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = "All types") %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("natHDI_category", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high HDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>%
    mutate(shdi_category = case) %>%
    separate(shdi_category,sep = ":",into=c("natHDI_category","case2")) %>%
    select(-case2)
  
  res_all_df <- left_join(res_all_df,point_counts, by="case" )
  return(res_all_df)
}

### Calculate OR likelihood of being affected per national HDI group & disaster type
boot_odds_affected_nationalHDI <- function(distype){
  print(paste("calculate ORs risk affected, per sHDI group for ", distype, " disaster type"))
  #sample data
  sample_disaster_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_affected) & normalized_affected > 0) %>% 
    mutate(natHDI_category = factor(natHDI_category),deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           natHDI_category = relevel(natHDI_category, "Very high HDI")) %>% 
    dplyr::select(c(normalized_affected,natHDI_category, deviation_category,disaster_type,iso,continent)) %>% drop_na()
  
  #logistic regression
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    fit <- glm(normalized_affected~natHDI_category, data = d, family = "binomial")  
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(  
    data = sample_disaster_hdi, 
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disaster_hdi$natHDI_category)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disaster_hdi %>% 
    group_by(natHDI_category) %>% 
    summarise(nevents = n())
  
  
  low_hdi_count <- event_counts %>% 
    filter(natHDI_category == "Low HDI" | natHDI_category == "Very high HDI") %>% 
    mutate(npoints = sum(nevents), case = "Low HDI : Very high HDI") %>% 
    select(case, npoints) %>% distinct()
  
  medium_hdi_count <- event_counts %>% 
    filter(natHDI_category == "Medium HDI" | natHDI_category == "Very high HDI") %>% 
    mutate(npoints = sum(nevents), case = "Medium HDI : Very high HDI") %>% 
    select(case, npoints) %>% distinct()
  
  high_hdi_count <- event_counts %>% 
    filter(natHDI_category == "High HDI" | natHDI_category == "Very high HDI") %>% 
    mutate(npoints = sum(nevents), case = "High HDI : Very high HDI") %>% 
    select(case, npoints) %>% distinct()
  
  point_counts <- rbind(low_hdi_count, medium_hdi_count, high_hdi_count)
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = distype) %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("natHDI_category", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high HDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>% 
    mutate(shdi_category = case) %>% 
    separate(shdi_category,sep = ":",into=c("natHDI_category","case2")) %>% 
    select(-case2)
  
  res_all_df <- left_join(res_all_df,point_counts, by="case" )
  return(res_all_df)
}

### Select cases that have at least 30 data points representing all national HDI groups
consider_disaster_affected_nationalHDI <- function(distype){
  disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_affected) & normalized_affected > 0) %>% 
    dplyr::select(c(normalized_affected,natHDI_category,disaster_type,iso,continent)) %>% drop_na() %>% 
    group_by(natHDI_category) %>% 
    tally() %>% summarise(nrow = nrow(.)==4, sumrows = sum(n) >= 30) %>% 
    mutate(case = nrow*sumrows) %>% pull(case)
}

### Calculate OR likelihood of fatalities per national HDI group & disaster type
boot_odds_killed_nationalHDI <- function(distype){
  print(paste("calculate ORs risk fatalities, per National HDI group for ", distype, " disaster type"))
  #sample data  
  sample_disaster_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_deaths) & normalized_deaths > 0) %>% 
    mutate(natHDI_category = factor(natHDI_category),deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           natHDI_category = relevel(natHDI_category, "Very high HDI")) %>% 
    dplyr::select(c(normalized_deaths,natHDI_category, deviation_category,disaster_type,iso,continent)) %>% drop_na()
  
  #logistic regression
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    fit <- glm(normalized_deaths ~ natHDI_category, data = d, family = "binomial")  
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(  
    data = sample_disaster_hdi, 
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disaster_hdi$natHDI_category)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disaster_hdi %>% 
    group_by(natHDI_category) %>% 
    summarise(nevents = n())
  
  
  low_hdi_count <- event_counts %>% 
    filter(natHDI_category == "Low HDI" | natHDI_category == "Very high HDI") %>% 
    mutate(npoints = sum(nevents), case = "Low HDI : Very high HDI") %>% 
    select(case, npoints) %>% distinct()
  
  medium_hdi_count <- event_counts %>% 
    filter(natHDI_category == "Medium HDI" | natHDI_category == "Very high HDI") %>% 
    mutate(npoints = sum(nevents), case = "Medium HDI : Very high HDI") %>% 
    select(case, npoints) %>% distinct()
  
  high_hdi_count <- event_counts %>% 
    filter(natHDI_category == "High HDI" | natHDI_category == "Very high HDI") %>% 
    mutate(npoints = sum(nevents), case = "High HDI : Very high HDI") %>% 
    select(case, npoints) %>% distinct()
  
  point_counts <- rbind(low_hdi_count, medium_hdi_count, high_hdi_count)
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = distype) %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("natHDI_category", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high HDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>% 
    mutate(shdi_category = case) %>% 
    separate(shdi_category,sep = ":",into=c("natHDI_category","case2")) %>% 
    select(-case2)
  
  res_all_df <- left_join(res_all_df,point_counts, by="case" )
  return(res_all_df)
}

### Select cases that have at least 30 data points representing all sHDI groups
consider_disaster_killed_nationalHDI <- function(distype){
  disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_deaths) & normalized_deaths > 0) %>% 
    dplyr::select(c(normalized_deaths,natHDI_category,disaster_type,iso,continent)) %>% drop_na() %>% 
    group_by(natHDI_category) %>% 
    tally() %>% summarise(nrow = nrow(.)==4, sumrows = sum(n) >= 30) %>% 
    mutate(case = nrow*sumrows) %>% pull(case)
}

### Calculate OR likelihood of economic losses per sHDI group & disaster type
boot_odds_damages_nationalHDI <- function(distype){
  print(paste("calculate ORs risk econ. losses, per National HDI group for ", distype, " disaster type"))
  #sample data 
  sample_disaster_hdi <- disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_total_damages) & normalized_total_damages > 0) %>% 
    mutate(natHDI_category = factor(natHDI_category),deviation_category = factor(deviation_category),
           iso = factor(iso),continent = factor(continent),
           natHDI_category = relevel(natHDI_category, "Very high HDI")) %>% 
    dplyr::select(c(normalized_total_damages,natHDI_category, deviation_category,disaster_type,iso,continent)) %>% drop_na()
  
  #logistic regression
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    fit <- glm(normalized_total_damages ~ natHDI_category, data = d, family = "binomial")  
    return(coef(fit))
  }
  
  boot_fit <- boot::boot(  
    data = sample_disaster_hdi, 
    statistic = logit_test,
    R = 5000,strata = as.numeric(factor(sample_disaster_hdi$natHDI_category)),
    parallel = "multicore",
    ncpus = getOption("boot.ncpus", n_nodes), cl = NULL)
  
  res_all_df = data.frame()
  
  # count points per case
  event_counts <- sample_disaster_hdi %>% 
    group_by(natHDI_category) %>% 
    summarise(nevents = n())
  
  
  low_hdi_count <- event_counts %>% 
    filter(natHDI_category == "Low HDI" | natHDI_category == "Very high HDI") %>% 
    mutate(npoints = sum(nevents), case = "Low sHDI : Very high HDI") %>% 
    select(case, npoints) %>% distinct()
  
  medium_hdi_count <- event_counts %>% 
    filter(natHDI_category == "Medium HDI" | natHDI_category == "Very high HDI") %>% 
    mutate(npoints = sum(nevents), case = "Medium HDI : Very high HDI") %>% 
    select(case, npoints) %>% distinct()
  
  high_hdi_count <- event_counts %>% 
    filter(natHDI_category == "High HDI" | natHDI_category == "Very high HDI") %>% 
    mutate(npoints = sum(nevents), case = "High HDI : Very high HDI") %>% 
    select(case, npoints) %>% distinct()
  
  point_counts <- rbind(low_hdi_count, medium_hdi_count, high_hdi_count)
  
  #collect results
  for (i in 2:length(boot_fit$t0)) {
    std_error <- sd(boot_fit$t[,i])
    ci_boot <- boot::boot.ci(boot_fit, type="perc",index=i)
    res_df <- data.frame(odds_ratio = exp(boot_fit$t0[i]), ci_low=exp(ci_boot$percent[,4]),ci_high=exp(ci_boot$percent[,5]),
                         log_odds=boot_fit$t0[i], ci_log_low=ci_boot$percent[,4],ci_log_high=ci_boot$percent[,5],
                         disaster_type = distype) %>%
      mutate(pvalue = 2*pnorm(-abs(log_odds/std_error)))
    res_df <- rownames_to_column(res_df) %>% rename(case=rowname)
    res_df$case <- sapply(res_df$case, function(x) gsub("natHDI_category", "", x, fixed = TRUE))
    res_df$case <- sapply(res_df$case, function(x) paste0(x, " : Very high HDI"))
    res_all_df <- rbind(res_all_df,res_df)}
  
  res_all_df <- res_all_df %>% 
    mutate(shdi_category = case) %>% 
    separate(shdi_category,sep = ":",into=c("natHDI_category","case2")) %>% 
    select(-case2)
  
  res_all_df <- left_join(res_all_df,point_counts, by="case")
  return(res_all_df)
}

### Select cases that have at least 30 data points representing all national HDI groups
consider_disaster_damages_nationalHDI <- function(distype){
  disasters_df %>% 
    filter(disaster_type == distype & !is.na(normalized_total_damages) & normalized_total_damages > 0) %>% 
    dplyr::select(c(normalized_total_damages,natHDI_category,disaster_type,iso,continent)) %>% drop_na() %>% 
    group_by(natHDI_category) %>% 
    tally() %>% summarise(nrow = nrow(.)==4, sumrows = sum(n) >= 30) %>% 
    mutate(case = nrow*sumrows) %>% pull(case)
}
