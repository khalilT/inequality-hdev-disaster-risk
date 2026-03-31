### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script is used to calculate the population weights for every event area.
### Most events areas are formed by a combination of Admin 1 and Admin 2 regions. In order to estimate the subnational human development index (sHDI) corresponding to the whole impacted area, we need to weight the average of sHDI for every location by population. For this, we need to extract population at the location level, determine sHDI by location, and finally calculate the population weighted average to have sHDI for the whole impacted area.

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd
import rioxarray


#1 select events from 1990 to 2020
#######################################

disasters = gpd.read_file('../../data/intermediate_data/geolocated_climate_events_1990-2023_simplified_clean.gpkg')
disasters['year'] = disasters['DisNo.'].str[:4]
disasters['year'] = disasters['year'].astype(int)
disasters["occ_number"] = (disasters.groupby("DisNo.").cumcount()+1).astype(str)
disasters['event_poly_id'] = disasters[['DisNo.','occ_number']].agg('-'.join, axis=1)
disasters = disasters.sort_values("event_poly_id").drop(columns={"occ_number"})
disasters = disasters[disasters["year"] < 2021]


#2 read pop data and harmonize CRS
#######################################
population_data = xr.open_zarr('../../data/intermediate_data/population_data_ghs_native_res_1990_2020.zarr')
population_data.rio.write_crs("epsg:4326", inplace=True)

print(population_data.rio.crs)
print(disasters.crs)
disasters = disasters.to_crs(population_data.rio.crs)


### function to extract population of every location
def get_population(event):
    print(event)
    disasters_1 = disasters[disasters.index == event]
    pop_dis = population_data.sel(band = disasters_1.year.values[0]).rio.clip(disasters_1.geometry,disasters.crs,all_touched=True)
    return {event:pop_dis.band_data.sum().values}



#3 Extract pop per location and calculate weights
#######################################
disasters = disasters.set_index('event_poly_id')
events = disasters.index

from multiprocessing.pool import ThreadPool
pop_per_region = ThreadPool(30).map(get_population, events)

### Calculate weights 
pop_per_region_df = pd.DataFrame(pop_per_region)
pop_per_region_df = pop_per_region_df.stack()
pop_per_region_df = pop_per_region_df.reset_index()
pop_per_region_df = pop_per_region_df.rename(columns={'level_1':'event_poly_id',0:'population'}).drop(columns=['level_0'])
pop_per_region_df.population = pop_per_region_df.population.astype(int)
pop_per_region_df = pop_per_region_df.round()
pop_per_region_df = pop_per_region_df.set_index('event_poly_id')

disasters_merge_data = disasters[['DisNo.']]
disasters_merge_data = disasters_merge_data.join(pop_per_region_df)

disasters_merge_data["pop_weight"] = disasters_merge_data.groupby('DisNo.')['population'].transform(lambda x: x/x.sum())
disasters_merge_data[["pop_weight"]] = disasters_merge_data[["pop_weight"]].round(3)

disasters_merge_data = disasters_merge_data.reset_index()

### save weights df
disasters_merge_data.to_csv("../../data/intermediate_data/to_merge_popweight_subnational.csv")
