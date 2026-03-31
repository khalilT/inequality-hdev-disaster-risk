### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script is for reading the data necessary for the aggregation of the climate variable to the EM-DAT events.

import xarray as _xr
import numpy as _np
import geopandas as _gpd
import pandas as _pd

########################################### Read raster data #################################################

###########################################
#read ERA5 climate data
###########################################

def climate_data():
    #original units
    ERA5_p1_original = _xr.open_zarr("../data/intermediate_data/climate_data/original/ERA5Data_a_85_23.zarr/",consolidated=True)
    ERA5_p1_original['t2mmax'] = ERA5_p1_original.t2mmax - 273.15
    ERA5_p1_original['t2m'] = ERA5_p1_original.t2m - 273.15
    ERA5_p1_original['t2mmin'] = ERA5_p1_original.t2mmin - 273.15
    ERA5_p1_original['tp'] = ERA5_p1_original.tp * 1000
    
    ERA5_p2_original = _xr.open_zarr("../data/intermediate_data/climate_data/original/ERA5Data_b_85_23.zarr/",consolidated=True)
    ERA5_p2_original['windgustmax'] = ERA5_p2_original.windgustmax * 3.6
    ERA5_p2_original['windgustmean'] = ERA5_p2_original.windgustmean * 3.6
    ERA5_p2_original['tro'] = ERA5_p2_original.tro * 1000
    
    soil_moisture = _xr.open_zarr("../data/intermediate_data/climate_data/original/smoisture_85_23.zarr/",consolidated=True)
    aggregated_originals = _xr.combine_by_coords([ERA5_p1_original, ERA5_p2_original, soil_moisture])
    
        
    #anomalies
    ERA5_p1_anomalies = _xr.open_zarr("../data/intermediate_data/climate_data/anomalies/era5_data_a_anomalies.zarr/",consolidated=True)
    ERA5_p1_anomalies['tp'] = ERA5_p1_anomalies.tp * 1000
    ERA5_p2_anomalies = _xr.open_zarr("../data/intermediate_data/climate_data/anomalies/era5_data_b_anomalies.zarr/",consolidated=True)
    ERA5_p2_anomalies['windgustmax'] = ERA5_p2_anomalies.windgustmax * 3.6
    ERA5_p2_anomalies['windgustmean'] = ERA5_p2_anomalies.windgustmean * 3.6
    ERA5_p2_anomalies['tro'] = ERA5_p2_anomalies.tro * 1000
    soil_moisture_anomalies = _xr.open_zarr("../data/intermediate_data/climate_data/anomalies/smoisture_data_anomalies.zarr/",consolidated=True)
    aggregated_anomalies = _xr.combine_by_coords([ERA5_p1_anomalies, ERA5_p2_anomalies,soil_moisture_anomalies],combine_attrs='drop_conflicts')
    
    #zscored
    ERA5_p1_norm_anoms = _xr.open_zarr("../data/intermediate_data/climate_data/zscored/era5_data_a_zscore.zarr/",consolidated=True)
    ERA5_p2_norm_anoms = _xr.open_zarr("../data/intermediate_data/climate_data/zscored/era5_data_b_zscore.zarr/",consolidated=True)
    soil_moisture_norm_anoms = _xr.open_zarr("../data/intermediate_data/climate_data/zscored/smoisture_data_zscore.zarr/",consolidated=True)
    aggregated_norm_anoms = _xr.combine_by_coords([ERA5_p1_norm_anoms, ERA5_p2_norm_anoms,soil_moisture_norm_anoms],combine_attrs='drop_conflicts')
    
    aggregated_clim_data = _xr.concat([aggregated_originals, aggregated_anomalies, aggregated_norm_anoms],dim="units")
    
    aggregated_clim_data = aggregated_clim_data.assign_coords({"units": ["original","anomalies","zscored"]})
    aggregated_clim_data = aggregated_clim_data.transpose("units", "time", "latitude", "longitude")
    
    return(aggregated_clim_data)

# ###########################################
# #read land cover data
# ###########################################

def landcover_data():
    landcover = _xr.open_zarr('../data/intermediate_data/landcover/ESACCI_1990_2023.zarr/',consolidated=False)
    return(landcover)

# ###########################################
# #read latitudinal weights data
# ###########################################

def lat_weights_data():
    lat_vals = _np.cos(_np.deg2rad(_np.tile(climate_data().latitude.values.reshape(721,1),(1,1440))))
    latitudes_array = _xr.DataArray(lat_vals, coords = dict(latitude = climate_data().latitude,
                                                      longitude = climate_data().longitude))
    return(latitudes_array)

# ########################################### Read vector data #################################################

# ###########################################
# #read geolocations of extreme events
# ###########################################

def disaster_gaul_data():
    gaul_events = _gpd.read_file('../data/intermediate_data/geolocated_climate_events_1990-2023_national_clean.gpkg')
    gaul_events = gaul_events.set_index('DisNo.')
    gaul_events = gaul_events.to_crs(4326)
    return(gaul_events)

# ###########################################
# #read country maps
# ###########################################

def world_map_data():
    world_map = _gpd.read_file('../data/raw_data/gaul_maps/gaul_admin0_clean.gpkg')
    world_map = world_map.set_index('iso3')
    world_map = world_map.to_crs(4326)
    return(world_map)