### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script is for the extraction of the population and GDP corresponding to each event location. The objective is the normalization of the human losses and financial damage impact data of the geolocated events.
### 1) we extract the population data: global population data is used from 1990 to 2020. For each event, the sum of the population at the affected geometry is used to normalize the data and calculate the fraction of affected people. In some cases, the fraction exceeds 1, in this case, it is manually capped to 0.999.
### 2) Same is done for global GDP data. Except capping max losses, as they can exceed region's GDP 

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd
import rioxarray

#1 read data
#######################################

population_data = xr.open_zarr('../data/intermediate_data/population_data_ghs_native_res_1990_2020.zarr')
population_data.rio.write_crs("epsg:4326", inplace=True)

gdp_data = xr.open_zarr('../data/intermediate_data/gdp_data_90_20_extrapol.zarr')
gdp_data.rio.write_crs("epsg:4326", inplace=True)

disasters = gpd.read_file("../data/intermediate_data/disaster_events_national_1990_2020.gpkg")
disasters = disasters.set_index("DisNo.")

# 2 extract population and GDP in every geometry corresponding to disaster event
#######################################

def get_population(event):
    print(event)
    disasters_1 = disasters[disasters.index == event]
    pop_dis = population_data.sel(band = disasters_1.year.values[0]).rio.clip(disasters_1.geometry,disasters.crs,all_touched=True)
    return {event:pop_dis.band_data.sum().values}

def get_gdp(event):
    print(event)
    disasters_1 = disasters[disasters.index == event]
    gdp_dis = gdp_data.sel(time = disasters_1.year.values[0]).rio.clip(disasters_1.geometry,disasters.crs,all_touched=True)
    return({event:gdp_dis.GDP_PPP.sum().values})

### extract pop data
events = disasters.index
from multiprocessing.pool import ThreadPool

%%time
results = ThreadPool(20).map(get_population, events)


pop_dat = pd.DataFrame(results)
pop_df = pop_dat.stack()
pop_df = pop_df.reset_index()
pop_df = pop_df.rename(columns={'level_1':'disaster_number_country',0:'population'}).drop(columns=['level_0'])
pop_df = pop_df.set_index('disaster_number_country')
pop_df.population = pop_df.population.astype(int)

pop_df.to_csv('../data/intermediate_data/event_population_ghs.csv')


### extract GDP data
%%time
results_gdp = ThreadPool(20).map(get_gdp, events)

gdp_df = pd.DataFrame(results_gdp)
gdp_df = gdp_df.stack()
gdp_df = gdp_df.reset_index()
gdp_df = gdp_df.rename(columns={'level_1':'disaster_number_country',0:'gdp'}).drop(columns=['level_0'])
gdp_df = gdp_df.set_index('disaster_number_country')
gdp_df.gdp = gdp_df.gdp.astype(int)

gdp_df.to_csv('../data/intermediate_data/event_gdp_extrapolated.csv')