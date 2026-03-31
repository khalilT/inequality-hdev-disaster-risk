### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script preprocesses the gridded GDP data and prepares it for use in order to estimate the exposed GDP in every geocoded event location.
### We use the kummu et al GDP prodcut (https://www.nature.com/articles/sdata20184), which we intrerpolate between the available years (until 2015) and extrapolate from 2016 to 2020 in order to obtain the yearly global GDP.

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd

from scipy.stats import linregress
from multiprocess import Pool

#1 preprocess GDP data and interpolate until 2015
#######################################

gdp_data = xr.open_dataset('../../data/raw_data/gdp/gdp_data/gdp.nc')
gdp_data = gdp_data.coarsen(longitude=5).sum().coarsen(latitude=5).sum()

#interpolate between years
interp_years_gdp = np.linspace(1990,2015,num=26)
interp_years_gdp = np.setdiff1d(interp_years_gdp, gdp_data.time.values)

gdp_data_interpolated = gdp_data.interp(time=interp_years_gdp)

gdp_data_90_15 = xr.concat([gdp_data, gdp_data_interpolated],dim='time')


#2 Extrapolate GDP data from 2015 to 2016
#######################################

def extrapolate_single_point(args):
    y, time_numeric, new_time_numeric = args
    mask = ~np.isnan(y)
    if np.sum(mask) > 1:  # Need at least 2 points to fit a linear model
        slope, intercept, _, _, _ = linregress(time_numeric[mask], y[mask])
        return intercept + slope * new_time_numeric
    return np.full(len(new_time_numeric), np.nan)

def extrapolate_data_array_time(da: xr.DataArray, new_times: np.ndarray) -> xr.DataArray:
    """Extrapolates a data array for new time steps using Linear Extrapolation.

    Parameters
    ----------
    da : xr.DataArray
        Original data array with longitude and latitude dimensions.
    new_times : np.ndarray
        New time steps for extrapolation (as floats).

    Returns
    -------
    xr.DataArray
        Extrapolated data array.
    """
    # Ensure time values are numeric
    time_numeric = np.array(da.time.values, dtype=np.float64)
    new_time_numeric = np.array(new_times, dtype=np.float64)

    # Combine old and new times for indexing
    all_times = np.concatenate([time_numeric, new_time_numeric])

    # Prepare the new DataArray to hold the extrapolated values
    new_da = xr.DataArray(
        np.full((len(all_times), da.shape[1], da.shape[2]), np.nan),
        coords=[all_times, da.latitude, da.longitude],
        dims=["time", "latitude", "longitude"]
    )
    
    # Copy the original data to the new DataArray
    new_da.loc[dict(time=da.time.values)] = da.values

    # Prepare arguments for parallel processing
    args = [(da[:, j, i].values, time_numeric, new_time_numeric) for j in range(len(da.latitude)) for i in range(len(da.longitude))]

    # Perform the extrapolation in parallel
    with Pool(50) as pool:
        results = pool.map(extrapolate_single_point, args)

    # Assign the extrapolated values to the new DataArray
    for idx, future_y in enumerate(results):
        j = idx // len(da.longitude)
        i = idx % len(da.longitude)
        new_da[len(time_numeric):, j, i] = future_y

    # Return only the new times
    return new_da.sel(time=new_time_numeric)

#3 concatenate and write data
#######################################

gdp_data_16_20 = extrapolate_data_array_time(gdp_data_90_15.GDP_PPP, [2016, 2017, 2018, 2019, 2020])

gdp_data_16_20_dataset = gdp_data_16_20.to_dataset(name="GDP_PPP")

gdp_data_90_20_extrapol = xr.concat([gdp_data_90_15 ,gdp_data_16_20_dataset] , dim = "time")
gdp_data_90_20_extrapol = gdp_data_90_20_extrapol.sortby("time")

gdp_data_90_20_extrapol = gdp_data_90_20_extrapol.chunk(dict(longitude=900,latitude=400,time=31))

gdp_data_90_20_extrapol.to_zarr('../../data/intermediate_data/gdp_data_90_20_extrapol.zarr')