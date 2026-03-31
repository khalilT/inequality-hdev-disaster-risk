### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script is for creation of the dataframe that will be used throught the analysis. It combines necessary information about the disaster and impacts (EM-DAT), about hazard intensity (ERA5, Gleam), the socioeconomic condition (sHDI) from the global data lab.

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd

#read data
#disaster information with normalized impacts
disaster_df = pd.read_csv('../data/intermediate_data/disaster_events_normalized_df.csv')
disaster_df = disaster_df.set_index('DisNo.')

#sHDI data
gdl_data = pd.read_csv("../data/intermediate_data/event_shdi.csv")
gdl_data = gdl_data.rename(columns={"sHDI":"subnational_hdi"})
gdl_data = gdl_data.drop(columns="Unnamed: 0").set_index('DisNo.')
gdl_data = gdl_data[["subnational_hdi"]]

# select relevant variables for hazard intensity variables
clim_summary_data = pd.read_csv("../data/intermediate_data/climate_summary_data.csv")
clim_summary_data_selected = clim_summary_data[['event','AMAX_t2mmax','AMAX_windgustmax','AMAX_smsurf','AMAX_tp', 'AMAX_tro','AMIN_t2mmin','AMIN_smsurf','AMIN_rh',
                                                        'ZMAX_t2mmax','ZMAX_windgustmax','ZMAX_smsurf','ZMAX_tp', 'ZMAX_tro','ZMIN_t2mmin','ZMIN_smsurf','ZMIN_rh']].rename(columns={'event':"Dis No"})
clim_summary_data_selected = clim_summary_data_selected.set_index('Dis No')

#join data and remove entries with missing sHDI
disaster_df = disaster_df.join(gdl_data).join(clim_summary_data_selected)
disaster_df = disaster_df[~np.isnan(disaster_df.subnational_hdi)]

#1 determine the sHDI categories
#######################################

def case_when(value):
    if value >= 0.8:
        return "Very high sHDI"
    elif (value >= 0.7) & (value < 0.8):
        return "High sHDI"
    elif (value >= 0.55) & (value < 0.7):
        return "Medium sHDI"
    elif value < 0.55:
        return "Low sHDI"
    
disaster_df["sHDI_category"] = [case_when(x) for x in disaster_df.subnational_hdi]

#2 determine the deviation from nat-average
#######################################

national_hdi = pd.read_csv("../data/intermediate_data/national_hdi.csv")
national_hdi = national_hdi.rename(columns={"iso_code":"iso","shdi":"national_hdi"})[["iso","year","national_hdi"]]

disaster_df = disaster_df.reset_index()
disaster_df = disaster_df.set_index(["iso","year"])
national_hdi = national_hdi.set_index(["iso","year"])
disaster_df = disaster_df.join(national_hdi).reset_index()
disaster_df["deviation"] = disaster_df["subnational_hdi"] - disaster_df["national_hdi"]

#determine the thresholds of the deviation from national HDI
deviation_quantiles = np.quantile(disaster_df.deviation,q=[0.2,0.8])
deviation_quantiles

def deviation_category(value):
    if value <= deviation_quantiles[0]:
        return "worse-off"
    elif value >= deviation_quantiles[1]:
        return "better-off"
    else:
        return "nat-average"
    
disaster_df["deviation_category"] = [deviation_category(x) for x in disaster_df.deviation]


#3 determine the HDI categories
#######################################

def case_when_national(value):
    if value >= 0.8:
        return "Very high HDI"
    elif (value >= 0.7) & (value < 0.8):
        return "High HDI"
    elif (value >= 0.55) & (value < 0.7):
        return "Medium HDI"
    elif value < 0.55:
        return "Low HDI"
    
disaster_df["natHDI_category"] = [case_when_national(x) for x in disaster_df.national_hdi]
disaster_df = disaster_df.sort_values("disaster_number_country").reset_index().drop(columns={"index"})
disaster_df = disaster_df.rename(columns={"DisNo.":"Dis No"})


#4 correct disaster types
#######################################

#need to correct landslides, cold waves and heat waves
disaster_df.loc[disaster_df.disaster_type == 'Mass movement (dry)', "disaster_type"] = "Landslide"
disaster_df.loc[disaster_df.disaster_type == 'Mass movement (wet)', "disaster_type"] = "Landslide"
disaster_df.loc[disaster_df.disaster_subtype == 'Cold wave', "disaster_type"] = "Cold wave"
disaster_df.loc[disaster_df.disaster_subtype == 'Severe winter conditions', "disaster_type"] = "Cold wave"
disaster_df.loc[disaster_df.disaster_subtype == 'Heat wave', "disaster_type"] = 'Heat wave'

#5 correct and adjust the geodataframe containing the geometries
#######################################

disasters = gpd.read_file("../data/intermediate_data/disaster_events_national_1990_2020.gpkg")
disasters_selected = disasters.set_index("DisNo.").loc[disaster_df["Dis No"]].drop(columns={"disaster_type"})
shdi_data = disaster_df[['Dis No', 'disaster_type','longitude_min', 'longitude_max','latitude_min', 'latitude_max',
                         'subnational_hdi','sHDI_category', 'national_hdi', 'deviation','deviation_category', 'natHDI_category']]
disasters_selected = disasters_selected.join(shdi_data.set_index("Dis No"))
disasters_selected = disasters_selected.reset_index().rename(columns = {"DisNo.":"Dis No"})
disasters_selected.to_file('../data/intermediate_data/disaster_geo.gpkg', driver="GPKG")

#6 create the analysis ready dataframe
#######################################

disasters_selected["lon"] = disasters_selected.centroid.x
disasters_selected["lat"] = disasters_selected.centroid.y
disasters_coordinates = disasters_selected[['lon','lat']]

disaster_df = disaster_df.join(disasters_coordinates)

disaster_df.to_csv("../data/input/disaster_information.csv")
