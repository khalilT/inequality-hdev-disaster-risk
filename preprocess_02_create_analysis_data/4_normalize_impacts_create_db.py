### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script is for the normalization of the disaster impact data (human loss & economic damage)

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd

### read data
population_data = pd.read_csv('../data/intermediate_data/event_population_ghs.csv')
population_data = population_data.set_index('disaster_number_country')

gdp_data = pd.read_csv('../data/intermediate_data/event_gdp_extrapolated.csv')
gdp_data = gdp_data.set_index('disaster_number_country')

disasters = gpd.read_file("../data/intermediate_data/disaster_events_national_1990_2020.gpkg")
disasters = disasters.set_index("DisNo.")

#1 normalize human losses
#######################################

disasters_national_human_losses = pd.DataFrame(pd.concat([disasters.loc[:,['disaster_number_country','iso','year']], disasters.loc[:,'total_deaths':'No. Homeless']],axis=1))

disasters_national_human_losses['normalized_affected'] = disasters_national_human_losses['total_affected'] / population_data['population']
disasters_national_human_losses['normalized_deaths'] = disasters_national_human_losses['total_deaths'] / population_data['population']

#in 88 events (mostly floods and storms) normalized affected > 1 #excessive_affected
excessive_affected = disasters_national_human_losses[disasters_national_human_losses['normalized_affected'] > 1]
disasters_national_human_losses.loc[disasters_national_human_losses['normalized_affected'] > 1.,'normalized_affected'] = 0.999 #cap normalized affected to 0.999
disasters_national_human_losses['total_population'] = population_data['population']


#2 normalize economic damage
#######################################

disasters_national_financial_damages = disasters.loc[:,['disaster_number_country','iso','year',"total_damages","CPI"]]
disasters_national_financial_damages[disasters_national_financial_damages.year == 2011]
#value for normalization: 73.822751
ref_cpi = 73.822751

#adjust to 2011 dollars
disasters_national_financial_damages["correction"] = ref_cpi / disasters_national_financial_damages.CPI

disasters_national_financial_damages["total_damages"] = disasters_national_financial_damages["total_damages"] * disasters_national_financial_damages["correction"]
disasters_national_financial_damages = disasters_national_financial_damages.drop(columns={"CPI","correction"})
disasters_national_financial_damages = disasters_national_financial_damages.join(gdp_data)
disasters_national_financial_damages['total_damages'] = disasters_national_financial_damages['total_damages'] * 1000 #damage reported in 1000$ units
disasters_national_financial_damages['normalized_total_damages'] = disasters_national_financial_damages['total_damages'] / disasters_national_financial_damages['gdp']
disasters_national_financial_damages = disasters_national_financial_damages.rename(columns={'gdp':'total_gdp'})

null_gdp = disasters_national_financial_damages[disasters_national_financial_damages.total_gdp == 0]
# 36 events with gdp = 0 will be removed from dataset
# 17 events had normalized economic damage > 1 (capped to 0.999)

disasters_national_financial_damages.loc[disasters_national_financial_damages['normalized_total_damages'] > 1.,'normalized_total_damages'] = 0.999
normalized_impacts = pd.merge(disasters_national_human_losses.loc[:,'normalized_affected':'total_population'],disasters_national_financial_damages.loc[:,'total_damages':'normalized_total_damages'], on='DisNo.')
disasters = pd.merge(disasters,normalized_impacts, on='DisNo.')
disasters = disasters.drop(null_gdp.index)

#drop unwanted columns
disasters = disasters.drop(columns={"total_damages_x","total_damages_adjusted","insured_damages","insured_damages_adjusted"})
disasters = disasters.rename(columns={"total_damages_y":"total_damages"})

#3 Extract geographic bounds of regions
#######################################

#add bounds of longitudes and latitudes to the disasters_df
bounds_geom = disasters['geometry'].bounds
bounds_geom = bounds_geom.rename(columns={'minx':'longitude_min','miny':'latitude_min','maxx':'longitude_max','maxy':'latitude_max'})
bounds_geom = bounds_geom[['longitude_min','longitude_max','latitude_min','latitude_max']]
bounds_geom = bounds_geom.round(3)

disasters = disasters.join(bounds_geom,on='DisNo.')
disasters = gpd.GeoDataFrame(disasters)


# write data
disasters.to_file("../data/intermediate_data/disaster_events_normalized.gpkg", driver="GPKG")
disasters_df = disasters.drop(columns='geometry')
disasters_df.to_csv('../data/intermediate_data/disaster_events_normalized_df.csv')