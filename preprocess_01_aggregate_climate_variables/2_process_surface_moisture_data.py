### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This code is for processing the GLEAM data global (0.1deg) and interpolate it to the same resolution as ERA5 (0.25deg)

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd


import glob

# Define the base directory path
base_dir = "/net/data/GLEAM/data/v4.1a/daily/"

# Generate file paths for all years from 1985 to 2023
file_paths = []
for year in range(1985, 2023 + 1):
    year_files = glob.glob(f"{base_dir}/{year}/SMs_{year}_GLEAM_v4.1a.nc")
    file_paths.extend(year_files)

# Open all files as a single xarray.Dataset
smoisture = xr.open_mfdataset(file_paths, combine='by_coords')

# data to interpolate to
ds_daily = xr.open_zarr('/net/projects/xaida/intermediate_data/climate_data/original/ERA5Data_a_85_23.zarr/',consolidated=True)

#interpolate to the exact grid of Era5
smoisture_resampled = smoisture.interp(lat=ds_daily.latitude, lon=ds_daily.longitude)

#drop unused coordinates
smoisture_resampled = smoisture_resampled.drop_vars(["lat","lon"])

smoisture_resampled = smoisture_resampled.chunk({'longitude':60, 'latitude':60, 'time':5844})
smoisture_resampled = smoisture_resampled.astype(np.float32)

#write new variables to a zarr object
smoisture_resampled.to_zarr('/net/projects/xaida/intermediate_data/climate_data/original/smoisture_85_23.zarr')