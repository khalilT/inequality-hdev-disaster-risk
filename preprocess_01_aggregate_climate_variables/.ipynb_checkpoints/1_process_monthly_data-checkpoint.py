### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This code is for processing the ERA5 global (0.25deg) data downloaded with an hourly temporal resolution, and aggregate them to daily.
### For temperature, daily mean, min, max are calculated
### For total precipitation, daily sum is calculated
### For win gust, daily mean and max are calculated
### We also calculate relative humidity from mean daily and mean wet bulb temperature.

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd
import multiprocessing


def hourly_to_daily_era5(file):
    
    print(f'the file is {file}')

    #interpolate data to match the daily ERA5 
    monthly_era5 = xr.open_dataset(file)
    monthly_era5 = monthly_era5.assign_coords(longitude=(((monthly_era5.longitude + 180) % 360) - 180)).sortby('longitude')

    #daily average variables
    era5_daily_mean = monthly_era5[['d2m','z','fg10']].resample(time='1D').mean()
    era5_daily_mean = era5_daily_mean.rename({'d2m':'d2mmean','z':'geopotential','fg10':'windgustmean'})


    #daily max variables
    era5_daily_max = monthly_era5[['fg10']].resample(time='1D').max()
    era5_daily_max = era5_daily_max.rename({'fg10':'windgustmax'})
    
    
    #daily sum variables
    era5_daily_sum = monthly_era5[['ro']].resample(time='1D').sum()
    era5_daily_sum = era5_daily_sum.rename({'ro':'tro'})


    #write monthly objects
    era5_daily_mean.to_netcdf('../data/raw_data/climate_data/daily_data/era5_mean/'+file[34:56])
    era5_daily_max.to_netcdf('../data/raw_data/climate_data/daily_data/era5_max/'+file[34:56])
    era5_daily_sum.to_netcdf('../data/raw_data/climate_data/daily_data/era5_sum/'+file[34:56])


if __name__ == '__main__':
    
    print(f'get_file_names')

    #list of hourly files to aggregate from houly to daily
    import glob
    import os

    log_file_path = '../data/raw_data/climate_data/daily_data/processed_files.txt'

    # Load list of already processed files
    if os.path.exists(log_file_path):
        with open(log_file_path, 'r') as log_file:
            processed_files = set(log_file.read().splitlines())
    else:
        processed_files = set()

    # Get the list of all files to process
    file_paths = glob.glob('../data/raw_data/ERA5/*')
    file_paths.sort()

    # Filter out files that have already been processed
    files_to_process = [f for f in file_paths if f not in processed_files]

    print(f'Starting multiprocessing with {len(files_to_process)} files to process')

    def process_and_log(file_path):
        try:
            # Call your processing function
            hourly_to_daily_era5(file_path)
            # Log the file as processed
            with open(log_file_path, 'a') as log_file:
                log_file.write(file_path + '\n')
        except Exception as e:
            # Handle any exceptions, log if necessary
            print(f"Failed to process {file_path}: {e}")
    
    # Start multiprocessing
    if files_to_process:
        with multiprocessing.Pool(6) as pool:
            pool.map(process_and_log, files_to_process)
    else:
        print("All files have been processed.")
    
    
    print(f'read_monthly_files')

    #read new data
    ds_max = xr.open_mfdataset('../data/raw_data/climate_data/daily_data/era5_max/*')#wind gust
    ds_sum = xr.open_mfdataset('../data/raw_data/climate_data/daily_data/era5_sum/*')#run off
    ds_mean = xr.open_mfdataset('../data/raw_data/climate_data/daily_data/era5_mean/*')#windgust#geopotential

    #combine data in the same object
    new_data = xr.combine_by_coords([ds_mean,ds_sum, ds_max])
    
    print(f'calculate relative humidity')

    #read mean temperature data, needed to calculate relative humidity
    ds = xr.open_zarr('../data/intermediate_data/climate_data/original/ERA5Data_a_85_23.zarr/')

    t2m = ds['t2m'] #mean temp
    d2m = new_data['d2mmean'] #wet bulb temperature

    rh = 100 * (np.exp(17.625*(d2m / (243.04+d2m))) / np.exp(17.625 * (t2m / (243.04 + t2m))))

    #in cases where rh exceeds 100%, cap value to 100
    ds_rh = rh.where(rh.data < 100, 100)

    #add relative humidity to new data
    new_data['rh'] = ds_rh
    
    print(f'chunk and write data')

    #select variables to be aggregated
    new_data = new_data[['geopotential','tro','windgustmean','windgustmax','rh','tro']]
    new_data = new_data.chunk({'longitude':60, 'latitude':60, 'time':5844})
    new_data = new_data.astype(np.float32)

    #write new variables to a zarr object
    new_data.to_zarr('../data/intermediate_data/climate_data/original/ERA5Data_b_85_23.zarr')
    
    print(f'mission accomplished')