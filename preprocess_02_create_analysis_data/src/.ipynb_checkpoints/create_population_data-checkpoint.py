### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script preprocesses the GHS population data and prepares it for use in order to estimate the exposed population in every geocoded event location.
### We use the GHS 5 year prodcut, which we intrerpolate between the years in order to obtain the yearly global population counts.

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd
import glob


#1 preprocess population data and write as chunked zarr
#######################################

ordered_file_paths = glob.glob('../../data/raw_data/pop_data/*.tif')
ordered_file_paths.sort()

band_90 = xr.open_dataset(ordered_file_paths[0])
x = band_90.x.values
y = band_90.y.values

for i in np.arange(len(ordered_file_paths)):
    band = xr.open_dataset(ordered_file_paths[i])
    band = band.assign_coords({'x':x, 'y':y})
    band = band[['band','x','y','band_data']]
    band = band.chunk({'x':4300, 'y':4300})
    band.to_zarr('../data/intermediate_data/population_data/population_ghs_native_res_'+ordered_file_paths[i][65:69]+'.zarr')


#2 Read, concatenate & linearly interpolate population data
#######################################

##read and concatenate data
ordered_file_paths = glob.glob('/net/projects/xaida/intermediate_data/population_data/population_ghs_native_res_*.zarr')
ordered_file_paths.sort()

pop_data_years = xr.open_mfdataset(ordered_file_paths,combine='nested',concat_dim='band')
pop_data_years = pop_data_years[["band_data"]]

pop_data_years = pop_data_years.assign_coords({'band' : [1990, 1995, 2000, 2005, 2010, 2015, 2020]})

pop_data_years = pop_data_years.sel(y=slice(84.979167,-57.979167))
pop_data = pop_data_years[['band','x','y','band_data']]


##linear interpolation
interp_years = np.linspace(1990,2020,num=31)

interp_years = np.setdiff1d(interp_years, pop_data.band.values)

data_interpolated = pop_data.interp(band=interp_years)
pop_data_allyears = xr.concat([pop_data, data_interpolated],dim='band')
pop_data_allyears = pop_data_allyears.sortby("band")

##rechunk and write data
del pop_data_allyears['band_data'].encoding['chunks']

pop_data_allyears = pop_data_allyears.chunk({'x':2800, 'y':2800, 'band':1})

pop_data_allyears.to_zarr('../../data/intermediate_data/population_data_ghs_native_res_1990_2020.zarr')