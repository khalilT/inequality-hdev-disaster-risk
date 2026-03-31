### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script is for the extraction of the climate variables corresponding to the disaster hazards. We extract the maximum, minimum, and mean values of the variables as indicated below for all events in a window of 2 days before and after the reported start.

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd
import multiprocessing


disasters_df = pd.read_csv('../data/intermediate_data/disaster_events_normalized_df.csv')
disasters_df = disasters_df.set_index('DisNo.')

clim_data = xr.open_zarr('../data/intermediate_data/climate_data.zarr')

var_list = ['t2m','t2mmax','t2mmin','windgustmean','windgustmax','rh','smsurf','tp','tro']

#Extract climate variables in the 3 dimensions: origianl, anomalies and zscored
##################################################

#1 Extract anomalies in the original units
#######################################

def summarize_event_max_anomalies_originals_duringevent(x):
    xev = disasters_df.loc[[x]]
    res = clim_data.sel(units='anomalies',event=xev.disaster_number_country.values,time=slice(xev.start_day.values[0]-2,xev.end_day.values[0]+2))[var_list].groupby('landcover').max('time').compute().to_dataframe()
    res_df = pd.DataFrame(res.reset_index().set_index('event').query("landcover == 'all'")[var_list])
    return(res_df)

def summarize_event_min_anomalies_originals_duringevent(x):
    xev = disasters_df.loc[[x]]
    res = clim_data.sel(units='anomalies',event=xev.disaster_number_country.values,time=slice(xev.start_day.values[0]-2,xev.end_day.values[0]+2))[var_list].groupby('landcover').min('time').compute().to_dataframe()
    res_df = pd.DataFrame(res.reset_index().set_index('event').query("landcover == 'all'")[var_list])
    return(res_df)

def summarize_event_mean_anomalies_originals_duringevent(x):
    xev = disasters_df.loc[[x]]
    res = clim_data.sel(units='anomalies',event=xev.disaster_number_country.values,time=slice(xev.start_day.values[0]-2,xev.end_day.values[0]+2))[var_list].groupby('landcover').mean('time').compute().to_dataframe()
    res_df = pd.DataFrame(res.reset_index().set_index('event').query("landcover == 'all'")[var_list])
    return(res_df)

events_list = disasters_df.index
#max
with multiprocessing.Pool(30) as p:
    res_during_max = p.map(summarize_event_max_anomalies_originals_duringevent, events_list)
    
max_anomalies_during = pd.concat(res_during_max)
max_anomalies_during_df = max_anomalies_during.add_prefix("AMAX_")

#min
with multiprocessing.Pool(30) as p:
    res_during_min = p.map(summarize_event_min_anomalies_originals_duringevent, events_list)
    
min_anomalies_during = pd.concat(res_during_min)
min_anomalies_during_df = min_anomalies_during.add_prefix("AMIN_")
    
#mean
with multiprocessing.Pool(30) as p:
    res_during_mean = p.map(summarize_event_mean_anomalies_originals_duringevent, events_list)

mean_anomalies_during = pd.concat(res_during_mean)
mean_anomalies_during_df = mean_anomalies_during.add_prefix("AMEAN_")

clim_summary_anomalies_original = max_anomalies_during_df.join(min_anomalies_during_df).join(mean_anomalies_during_df)

#2 Extract anomalies in the Zscored units
#######################################

def summarize_event_max_anomalies_zscore_duringevent(x):
    xev = disasters_df.loc[[x]]
    res = clim_data.sel(units='zscored',event=xev.disaster_number_country.values,time=slice(xev.start_day.values[0]-2,xev.end_day.values[0]+2))[var_list].groupby('landcover').max('time').compute().to_dataframe()
    res_df = pd.DataFrame(res.reset_index().set_index('event').query("landcover == 'all'")[var_list])
    return(res_df)

def summarize_event_min_anomalies_zscore_duringevent(x):
    xev = disasters_df.loc[[x]]
    res = clim_data.sel(units='zscored',event=xev.disaster_number_country.values,time=slice(xev.start_day.values[0]-2,xev.end_day.values[0]+2))[var_list].groupby('landcover').min('time').compute().to_dataframe()
    res_df = pd.DataFrame(res.reset_index().set_index('event').query("landcover == 'all'")[var_list])
    return(res_df)

def summarize_event_mean_anomalies_zscore_duringevent(x):
    xev = disasters_df.loc[[x]]
    res = clim_data.sel(units='zscored',event=xev.disaster_number_country.values,time=slice(xev.start_day.values[0]-2,xev.end_day.values[0]+2))[var_list].groupby('landcover').mean('time').compute().to_dataframe()
    res_df = pd.DataFrame(res.reset_index().set_index('event').query("landcover == 'all'")[var_list])
    return(res_df)

events_list = disasters_df.index
#max
with multiprocessing.Pool(30) as p:
    res_during_zmax = p.map(summarize_event_max_anomalies_zscore_duringevent, events_list)
zmax_anomalies_during = pd.concat(res_during_zmax)
zmax_anomalies_during_df = zmax_anomalies_during.add_prefix("ZMAX_")

#min
with multiprocessing.Pool(30) as p:
    res_during_zmin = p.map(summarize_event_min_anomalies_zscore_duringevent, events_list)
zmin_anomalies_during = pd.concat(res_during_zmin)
zmin_anomalies_during_df = zmin_anomalies_during.add_prefix("ZMIN_")

#mean
with multiprocessing.Pool(30) as p:
    res_during_zmean = p.map(summarize_event_mean_anomalies_zscore_duringevent, events_list)
zmean_anomalies_during = pd.concat(res_during_zmean)
zmean_anomalies_during_df = zmean_anomalies_during.add_prefix("ZMEAN_")

clim_summary_anomalies_zscored = zmax_anomalies_during_df.join(zmin_anomalies_during_df).join(zmean_anomalies_during_df)

#3 Extract values in the original units
#######################################

def summarize_event_max_values_duringevent(x):
    xev = disasters_df.loc[[x]]
    res = clim_data.sel(units='original',event=xev.disaster_number_country.values,time=slice(xev.start_day.values[0]-2,xev.end_day.values[0]+2))[var_list].groupby('landcover').max('time').compute().to_dataframe()
    res_df = pd.DataFrame(res.reset_index().set_index('event').query("landcover == 'all'")[var_list])
    return(res_df)

def summarize_event_min_values_duringevent(x):
    xev = disasters_df.loc[[x]]
    res = clim_data.sel(units='original',event=xev.disaster_number_country.values,time=slice(xev.start_day.values[0]-2,xev.end_day.values[0]+2))[var_list].groupby('landcover').min('time').compute().to_dataframe()
    res_df = pd.DataFrame(res.reset_index().set_index('event').query("landcover == 'all'")[var_list])
    return(res_df)

def summarize_event_mean_values_duringevent(x):
    xev = disasters_df.loc[[x]]
    res = clim_data.sel(units='original',event=xev.disaster_number_country.values,time=slice(xev.start_day.values[0]-2,xev.end_day.values[0]+2))[var_list].groupby('landcover').mean('time').compute().to_dataframe()
    res_df = pd.DataFrame(res.reset_index().set_index('event').query("landcover == 'all'")[var_list])
    return(res_df)



events_list = disasters_df.index

#max
with multiprocessing.Pool(30) as p:
    res_during_omax = p.map(summarize_event_max_values_duringevent, events_list)

omax_during = pd.concat(res_during_omax)
omax_during_df = omax_during.add_prefix("OMAX_")

#min
with multiprocessing.Pool(30) as p:
    res_during_omin = p.map(summarize_event_min_values_duringevent, events_list)

omin_during = pd.concat(res_during_omin)
omin_during_df = omin_during.add_prefix("OMIN_")

#mean
with multiprocessing.Pool(30) as p:
    res_during_omean = p.map(summarize_event_mean_values_duringevent, events_list)

omean_during = pd.concat(res_during_omean)
omean_during_df = omean_during.add_prefix("OMEAN_")


clim_summary_original = omax_during_df.join(omin_during_df).join(omean_during_df)

#4 merge and save data
#######################################

climate_data_during_events = clim_summary_anomalies_original.join(clim_summary_anomalies_zscored).join(clim_summary_original)
climate_data_during_events.to_csv("../data/intermediate_data/climate_summary_data.csv")