### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This code is for processing the ERA5 global (0.25deg), and calculating the anomalies in the origianl units by removing the mean seasonal cycle of the whole climatology (1985 - 2023), and then calculating the normalized anomalies per pixel by dividing the anomnalies by the standard deviation of the whole climatology.

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd
import datetime


#read the data
era5_data_a = xr.open_zarr("../data/intermediate_data/climate_data/original/ERA5Data_a_85_23.zarr/")
era5_data_b = xr.open_zarr("../data/intermediate_data/climate_data/original/ERA5Data_b_85_23.zarr/")
smoisture_data = xr.open_zarr("../data/intermediate_data/climate_data/original/smoisture_85_23.zarr/")

#functions to calculate anomalies and normalized anomalies
def calculate_anomalies(x):
    return x - x.mean(dim="time")

def calculate_normalized_anomalies(x):
    return (x - x.mean(dim="time"))/x.std(dim="time")


#caluclate anomalies
era5_data_a_anomalies = era5_data_a.groupby("time.month").map(calculate_anomalies)
print("calculated: ","era5_data_a_anomalies")
era5_data_b_anomalies = era5_data_b.groupby("time.month").map(calculate_anomalies)
print("calculated: ","era5_data_b_anomalies")
smoisture_data_anomalies = smoisture_data.groupby("time.month").map(calculate_anomalies)
print("calculated: ","smoisture_data_anomalies")
print("all anomalies calculated")

print("################")

#rechunk data
era5_data_a_anomalies = era5_data_a_anomalies.chunk({'longitude':60, 'latitude':60, 'time':5844})
era5_data_b_anomalies = era5_data_b_anomalies.chunk({'longitude':60, 'latitude':60, 'time':5844})
smoisture_data_anomalies = smoisture_data_anomalies.chunk({'longitude':60, 'latitude':60, 'time':5844})

print("data rechunking complete")
print("################")

#write data
era5_data_a_anomalies.to_zarr('../data/intermediate_data/climate_data/anomalies/era5_data_a_anomalies.zarr')
print("written: ","era5_data_a_anomalies")
era5_data_b_anomalies.to_zarr('../data/intermediate_data/climate_data/anomalies/era5_data_b_anomalies.zarr')
print("written: ","era5_data_b_anomalies")
smoisture_data_anomalies.to_zarr('../data/intermediate_data/climate_data/anomalies/smoisture_data_anomalies.zarr')
print("written: ","smoisture_data_anomalies")
print("all anomalies written")
print("################")

####################
#caluclate zscored anomalies
era5_data_a_zscore = era5_data_a.groupby("time.month").map(calculate_normalized_anomalies)
print("calculated: ","era5_data_a_zscore")
era5_data_b_zscore = era5_data_b.groupby("time.month").map(calculate_normalized_anomalies)
print("calculated: ","era5_data_b_zscore")
smoisture_data_zscore = smoisture_data.groupby("time.month").map(calculate_normalized_anomalies)
print("calculated: ","smoisture_data_zscore")
print("all zscore anomalies calculated")
print("################")

#rechunk data
era5_data_a_zscore = era5_data_a_zscore.chunk({'longitude':60, 'latitude':60, 'time':5844})
era5_data_b_zscore = era5_data_b_zscore.chunk({'longitude':60, 'latitude':60, 'time':5844})
smoisture_data_zscore = smoisture_data_zscore.chunk({'longitude':60, 'latitude':60, 'time':5844})

print("data rechunking complete")
print("################")

#write data
era5_data_a_zscore.to_zarr('../data/intermediate_data/climate_data/zscored/era5_data_a_zscore.zarr')
print("written: ","era5_data_a_zscore")
era5_data_b_zscore.to_zarr('../data/intermediate_data/climate_data/zscored/era5_data_b_zscore.zarr')
print("written: ","era5_data_b_zscore")
smoisture_data_zscore.to_zarr('../data/intermediate_data/climate_data/zscored/smoisture_data_zscore.zarr')
print("written: ","smoisture_data_zscore")
print("all zscored anomalies written")
print("################")