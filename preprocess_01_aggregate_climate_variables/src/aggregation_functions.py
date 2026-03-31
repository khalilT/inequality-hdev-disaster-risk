### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This module contains the needed functions to the aggregation of the climate variables to the EM-DAT events.
### The module contains 11 functions:

#1. get_land_cover_data(event_id): Retrieves and clips land cover data (urban, croplands, natural areas) for a given event based on its geographic shape and year.
#2. get_events_country(iso_code): Fetches disaster events and corresponding geographic boundaries for a specified country, along with latitudinal weights for spatial analysis.
#3. create_empty_event(columns, t1, t2, data_var): Creates an empty xarray dataset to hold time series data for a specific event, initializing it with NaN values.
#4. pad_ts(x): Pads a time series array with NaN values to ensure a consistent length of 2191 time steps, in case the climate data available is less than the needed 6 years
#5. reshape_event_variable_landcover(aggregated, unit): Reshapes and organizes event data by land cover type (urban, cropland, natural) and time dimensions for analysis.
#6. get_clim_data(iso_code, units_str, i): Retrieves climate data for a specific country and variable, slicing it by geographic boundaries and unit type (e.g., original, anomalies).
#7. aggregate_units_parallel(iso_code): Aggregates disaster event data by land cover type for a specified country, utilizing parallel processing to improve efficiency.
#8. aggregate_parallel(iso_code, units_str): Aggregates climate data and land cover data in parallel for different unit types (original, anomalies, z-scored) across events.
#9. parallel_px_overlap(x): Uses parallel processing to compute pixel overlap between event geometries and climate data, allowing spatial analysis at scale.
#10. reshape_small_variable_iso(iso_code, unit_str): Reshapes and organizes event variables for a specific ISO code and unit type into a dataset with land cover and temporal dimensions.
#11. reshape_small_iso_units(iso_code): Reshapes event data for a given ISO code across different unit types (original, anomalies, z-scored), combining the data into one xarray dataset.

#################################################

import xarray as _xr
import numpy as _np
import geopandas as _gpd
import pandas as _pd
import xagg as _xa
import gc
import copy
import rioxarray

try:
    from dea_tools.spatial import xr_vectorize
except ImportError:
    from dea_tools.spatial import xr_vectorize


import src.read_data as data
    
clim_data = data.climate_data()
landcover_data = data.landcover_data()
gaul_events = data.disaster_gaul_data()
world_map = data.world_map_data()
lat_array = data.lat_weights_data()

#helper function
#helper function
def get_land_cover_data(event_id):
    
    landcover = data.landcover_data()
    
    event_shape = gaul_events[gaul_events.index == event_id]
    event_year = int(event_shape.year.values[0])
    xmin, ymin, xmax, ymax = event_shape.total_bounds
    
    
    landcover_data_cropped = landcover.sel(x=slice(xmin, xmax), 
                                            y=slice(ymax,ymin), band = event_year).drop('band')
    
    landcover_data_cropped = landcover_data_cropped.band_data
    landcover_data_cropped = landcover_data_cropped.compute()
    landcover_data_cropped.rio.write_crs("EPSG:4326", inplace=True)
    
    try:
        landcover_data_clipped = landcover_data_cropped.rio.clip(event_shape.geometry.values, gaul_events.crs, all_touched=True)
    except:
        landcover_data_clipped = None
    
    lc_pixels = {}
    for i in range(1,4):
        try:
            lc_pixels[i] = xr_vectorize(landcover_data_clipped.where(landcover_data_clipped == i),
                   crs=landcover_data_clipped.rio.crs,
                   transform=landcover_data_clipped.rio.transform(),
                   mask=landcover_data_clipped.values == i).dissolve()
        except:
            lc_pixels[i] = _gpd.GeoDataFrame(columns=["geometry","attribute"], crs="EPSG:4326", geometry="geometry")
    
    event_shape_urban = event_shape.copy()
    event_shape_croplands = event_shape.copy()
    event_shape_natural = event_shape.copy()
    
       
    if lc_pixels[1].empty:
        event_shape_urban = _gpd.GeoDataFrame(columns=["geometry","attribute"], crs="EPSG:4326", geometry="geometry")
    else:
        event_shape_urban["geometry"] = lc_pixels[1]["geometry"].values
    
    if lc_pixels[2].empty:
        event_shape_croplands = _gpd.GeoDataFrame(columns=["geometry","attribute"], crs="EPSG:4326", geometry="geometry")
    else:
        event_shape_croplands["geometry"] = lc_pixels[2]["geometry"].values
    
    if lc_pixels[3].empty:
        event_shape_natural = _gpd.GeoDataFrame(columns=["geometry","attribute"], crs="EPSG:4326", geometry="geometry")
    else:
        event_shape_natural["geometry"] = lc_pixels[3]["geometry"].values
    
    return(event_shape_urban, event_shape_croplands, event_shape_natural)


#Big countries aggregation

def get_events_country(iso_code):
    iso_events = gaul_events[gaul_events.iso == iso_code]
    iso_map = world_map[world_map.index == iso_code]
    
    xmin, ymin, xmax, ymax = iso_map.bounds.loc[iso_code]
    latitude_weights_country = lat_array.sel(longitude=slice(xmin, xmax), latitude=slice(ymax, ymin))
    
    return(iso_events, iso_map,latitude_weights_country)


def create_empty_event(columns, t1, t2,data_var):
    vars_object = {element:('poly_idx',[_np.nan]) for element in list(columns.values[0:len(gaul_events.columns)-1])}
    vars_object[data_var] = (('time','poly_idx'),(_np.repeat(_np.nan, 2191)).reshape((2191,1)))
    empty_array = _xr.Dataset(
    vars_object,
        coords={"poly_idx": [0],
               'time':_np.arange(_np.datetime64(t1), _np.datetime64(t2)+1)},)
    return(empty_array)


def pad_ts(x):
    return _np.append(x,  _np.repeat(_np.nan,2191 - len(x)))


def reshape_event_variable_landcover(aggregated,unit):
    
    data_var = list(aggregated.data_vars)[-1]
    data = aggregated[[data_var]].to_array().data

    data = _np.apply_along_axis(pad_ts, 3, data)
    
    reshaped_event = _xr.Dataset(
        data_vars = {data_var : (['units','landcover','event','time'],data)},
        
        coords = { 'units' : (['units'],[unit]),
            'landcover': (['landcover'],["all","urban","croplands","natural"]),
            'event' : (['event'], [aggregated.disaster_number_country.values[0][0]]),
            'time' : _np.arange(-1095,1096)}
    )
    
    return(reshaped_event)


def get_clim_data(iso_code,units_str,i):
    iso_map = world_map[world_map.index == iso_code]
    xmin, ymin, xmax, ymax = iso_map.bounds.loc[iso_code]
    clim_data_country = clim_data.sel(longitude=slice(xmin, xmax), latitude=slice(ymax, ymin)).sel(units=units_str, drop=True)
    latitude_weights_country = lat_array.sel(longitude=slice(xmin, xmax), latitude=slice(ymax, ymin))
    clim_vars = list(clim_data_country.data_vars)
    clim_data_country_var = clim_data_country[[clim_vars[i]]]
    clim_data_country_mem = clim_data_country_var.compute()
    return(clim_data_country_mem)


def aggregate_units_parallel(iso_code):
    
    test_event, test_country,latitude_weights_country = get_events_country(iso_code)
    
    clim_data_cube_mem = get_clim_data(iso_code,'original',0)
    clim_data_cube_mem = clim_data_cube_mem.isel(time=0)
    
    def calculate_px_overlap(event_geom):
        try:
            px_ov_all = _xa.pixel_overlaps(clim_data_cube_mem, event_geom,weights=latitude_weights_country,subset_bbox=False)
        except:
            px_ov_all = None
        return(px_ov_all)
    
    
    def parallel_px_overlap(x):
        from multiprocess import Pool
        with Pool(30) as p:
            out = p.map(calculate_px_overlap, x)
        return(out)
    
    from multiprocessing.pool import ThreadPool
    with ThreadPool(10) as p:
        out = p.map(get_land_cover_data,test_event.index)
    #from multiprocess import Pool
    #with Pool(30) as p:
    #    out = p.map(get_land_cover_data, test_event.index)
    
    out_all = []
    out_urb = []
    out_crp = []
    out_nat = []
    
    for i in _np.arange(len(out)):
        out_all.append(test_event.iloc[[i]])
        out_urb.append(out[i][0])
        out_crp.append(out[i][1])
        out_nat.append(out[i][2])
        
    px_ov_all = parallel_px_overlap(out_all)
    px_ov_urb = parallel_px_overlap(out_urb)
    px_ov_crp = parallel_px_overlap(out_crp)
    px_ov_nat = parallel_px_overlap(out_nat)
    
    for i in _np.arange(len(px_ov_all)):
        px_ov_all[i].agg = px_ov_all[i].agg.iloc[:, :-1]
    
    for i in _np.arange(len(px_ov_urb)):
        try:
            px_ov_urb[i].agg = px_ov_urb[i].agg.iloc[:, :-1]
        except:
            pass
    
    for i in _np.arange(len(px_ov_crp)):
        try:
            px_ov_crp[i].agg = px_ov_crp[i].agg.iloc[:, :-1]
        except:
            pass
    
    for i in _np.arange(len(px_ov_nat)):
        try:
            px_ov_nat[i].agg = px_ov_nat[i].agg.iloc[:, :-1]
        except:
            pass
        
    del clim_data_cube_mem
    print('##########################################')
    print('##########################################')
    print('THIS PART SHOULD NOT BE REPEATED')
    print('##########################################')
    print('##########################################')
        
    dims_names = ['original', 'anomalies', 'zscored']
        
    def aggregate_parallel(iso_code,units_str):
        events = []
        for v in _np.arange(len(clim_data.data_vars)):

            aggregated_all = []
            aggregated_urb = []
            aggregated_crp = []
            aggregated_nat = []

            px_all = copy.deepcopy(px_ov_all)
            px_urb = copy.deepcopy(px_ov_urb)
            px_crp = copy.deepcopy(px_ov_crp)
            px_nat = copy.deepcopy(px_ov_nat)

            px_all = [pair[0] if pair[1] == None else pair[1] for pair in enumerate(px_all)]
            px_urb = [pair[0] if pair[1] == None else pair[1] for pair in enumerate(px_urb)]
            px_crp = [pair[0] if pair[1] == None else pair[1] for pair in enumerate(px_crp)]
            px_nat = [pair[0] if pair[1] == None else pair[1] for pair in enumerate(px_nat)]

            clim_data_cube_mem = get_clim_data(iso_code,units_str,v)

            def aggregate_variable_parallel(px_ov_object):

                if isinstance(px_ov_object, int):
                    ccolumns = px_all[px_ov_object].agg.columns
                    t1 = px_all[px_ov_object].agg.start_aggregation[0]
                    t2 = px_all[px_ov_object].agg.end_aggregation[0]

                    empty_event = create_empty_event(ccolumns,t1,t2, list(clim_data_cube_mem.data_vars)[0])

                    px_agg_out = _xr.zeros_like(empty_event).where(_xr.zeros_like(empty_event) != 0.)
                else:
                    px_agg_out = _xa.aggregate(clim_data_cube_mem.sel(time = slice(px_ov_object.agg.start_aggregation[0], px_ov_object.agg.end_aggregation[0])), px_ov_object).to_dataset()
                return(px_agg_out)


            def parallel_agg(x):
                #from multiprocess import Pool
                #with Pool(6) as p:
                #    out = p.map(aggregate_variable_parallel, x)
                from multiprocessing.pool import ThreadPool
                with ThreadPool(10) as p:
                    out = p.map(aggregate_variable_parallel,x)
                return(out)

            aggregated_all = parallel_agg(px_all)
            aggregated_urb = parallel_agg(px_urb)
            aggregated_crp = parallel_agg(px_crp)
            aggregated_nat = parallel_agg(px_nat)
            
            # events.append([aggregated_all, aggregated_urb, aggregated_crp, aggregated_nat])


            #event_out = []
            for e in _np.arange(len(px_all)):
                event_out = []
                event_out.append(reshape_event_variable_landcover(_xr.concat([aggregated_all[e],aggregated_urb[e],
                               aggregated_crp[e], aggregated_nat[e]],dim='landcover'),units_str))

                event_out = _xr.combine_by_coords(event_out)
            #event_out.append(event_out)
                events.append(event_out)
            events.append(event_out)
        return(events)

    def aggregate_country(units_str):
        aggregated = aggregate_parallel(iso_code,units_str)
        return(aggregated)


    from multiprocess import Pool
    #with Pool(9) as p:
    #    res = p.map(aggregate_country, dims_names)
    from multiprocessing.pool import ThreadPool
    with ThreadPool(6) as p:
        res = p.map(aggregate_country,dims_names)
    
    country_events = _xr.combine_by_coords([_xr.combine_by_coords(res[0]),_xr.combine_by_coords(res[1]),_xr.combine_by_coords(res[2])])
    # Apply reset_coords to ensure poly_idx is only in coords
    if 'poly_idx' in country_events.data_vars:
        country_events = country_events.reset_coords('poly_idx', drop=True)

    
    country_events = country_events.sortby("units",ascending=False)
    return(country_events)

def reshape_small_variable_iso(iso_code, unit_str):
    
    iso_events = gaul_events[gaul_events.iso == iso_code]
    
    event_list = iso_events.index
    
    event_array = []
    for event_id in event_list:
        
        event_shapex = gaul_events.loc[[event_id]]
        event_shapex = event_shapex

        aggregated = clim_data.sel(longitude=event_shapex['geometry'].centroid.x,
                                                 latitude = event_shapex['geometry'].centroid.y,
                                                 method='nearest').sel(time=slice(event_shapex.start_aggregation[0],
                                                                                  event_shapex.end_aggregation[0]),units=unit_str).drop(['longitude','latitude'])
        event_variable = []
        for i in _np.arange(len(clim_data.data_vars)):
            data_var = list(aggregated.data_vars)[i]

            def pad_ts(x):
                return _np.append(x,  _np.repeat(_np.nan,2191 - len(x)))

            data = aggregated[[data_var]].to_array().data
            data = _np.apply_along_axis(pad_ts, 1, data)


            empty_array = _np.empty(data.shape)
            empty_array[:] = _np.NaN

            data = _np.concatenate([data, empty_array,empty_array,empty_array],axis=2)

            reshaped_event = _xr.Dataset(
                    data_vars = {data_var : (['event','time','landcover','units'],data)},

                    coords = { 'units' : (['units'],[unit_str]),
                        'landcover': (['landcover'],["all","urban","croplands","natural"]),
                        'event' : (['event'], [event_id]),
                        'time' : _np.arange(-1095,1096),
                        }
                )
            event_variable.append(reshaped_event)
        
        event_array.append(event_variable)
    
    
    return(event_array)

def reshape_small_iso_units(iso_code):
    dims_names = ['original', 'anomalies', 'zscored']
    iso_events = []
    for unit_string in dims_names:
        events_units_dim = []
        events_dim = reshape_small_variable_iso(iso_code, unit_string)
        array_merged = []
        for i in _np.arange(len(events_dim)):
            array_merged.append(_xr.merge(events_dim[i]))
        iso_events.append(array_merged)
    events = _xr.combine_by_coords([_xr.combine_by_coords(iso_events[0]),_xr.combine_by_coords(iso_events[1]),_xr.combine_by_coords(iso_events[2])])
    return(events)