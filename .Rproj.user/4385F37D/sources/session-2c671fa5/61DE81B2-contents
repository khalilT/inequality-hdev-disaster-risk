### (c) Khalil Teber 2024-2025 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This code generates the dataframe used in the analysis.

library(readxl)
library(tidyverse)

#read EM-DAT - add EM-DAT excel file to "data/raw_data"
emdat <- read_excel("data/raw_data/YourEM-DATfile.xlsx")

#select relevant data columns from EM-DAT
emdat_cols <- emdat %>% select(c(`DisNo.`,`Disaster Type`,`Disaster Subtype`,`Total Deaths`,
                                 `Total Affected`,`Total Damage ('000 US$)`, CPI))
#read ancillary data
extracted_disaster_data <- read_csv("data/input/disaster_extracted_data.csv")


#Change name of EM-DAT columns
emdat_cols <- emdat_cols %>% rename(`Dis No` = `DisNo.`,
                                    disaster_type = `Disaster Type`, disaster_subtype = `Disaster Subtype`,
                                    total_deaths = `Total Deaths`, total_affected = `Total Affected`,
                                    total_damages = `Total Damage ('000 US$)`) %>% mutate(total_damages = total_damages * 1000)



#Join EM-DAT columns and other ancillary data
disaster_information <- left_join(extracted_disaster_data, emdat_cols, by = "Dis No")

#Economic Impact Adjustment
#extract 2011 CPI for constant USD
cpi2011 <- disaster_information %>% filter(year == 2011) %>% drop_na() %>% pull(CPI) %>% unique()
#convert total damages to 2011 constant USD
disaster_information <- disaster_information %>% mutate(total_damages = (cpi2011 / CPI)*total_damages)
disaster_information %>% mutate(test = total_damages / total_gdp) %>% 
  select(c(test, normalized_total_damages))

#EM-DAT classification
disaster_information <- disaster_information %>% mutate(disaster_type = case_when(disaster_subtype == "Heat wave" ~ "Heat wave",
                                                                                  disaster_subtype == "Severe winter conditions" ~ "Cold wave",
                                                                                  disaster_subtype == "Cold wave" ~ "Cold wave",
                                                                                  disaster_type == "Mass movement (dry)" ~ "Landslide",
                                                                                  disaster_type == "Mass movement (wet)" ~ "Landslide",
                                                                                  .default = disaster_type))


remove(list = c("emdat_cols", "extracted_disaster_data"))
write_csv(disaster_information, "data/input/disaster_information.csv")
