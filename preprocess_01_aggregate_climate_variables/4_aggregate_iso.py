### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This code is for the aggregation of the climate data for every event, to extract time series of the different variables 3 years before and 3 years after the reported start of the event.
### Input data is read using the module src.read_data
### The functions necessary for the aggregation are read from the module src.aggregation_functions
### the output are zarr files containing the aggregated time series of the climate variables (original units, anomalies and zscored anomalies), for the whole geometry of the event and per land cover type (urban, croplands and water), per country.

import src.read_data as data
import src.aggregation_functions as aggf
import pandas as _pd
import numpy as _np
import os

gaul_events = data.disaster_gaul_data()
list_countries = _np.unique(gaul_events["iso"])

output_dir = '../data/intermediate_data/clim_iso/'

for country in list_countries:
    # Main file for `aggregate_units_parallel`
    output_file = os.path.join(output_dir, country + '_events.zarr')
    
    # File with "_small" suffix for `reshape_small_iso_units`
    output_file_small = os.path.join(output_dir, country + '_events_small.zarr')
    
    # Check if the Zarr file for the main method already exists
    if os.path.exists(output_file) or os.path.exists(output_file_small):
        print(f'{country} already processed. Skipping...')
        continue
    
    try:
        print('######')
        print(country)
        print('######')
        countries_events = aggf.aggregate_units_parallel(country)
        print(f'aggregation done')
        countries_events.to_zarr(output_file)
        print(f'written to disk successfully !')
    except:
        print('######')
        print(country)
        print('######')
        countries_events = aggf.reshape_small_iso_units(country)
        print(f'aggregation done')
        
        # Save with "_small" suffix
        countries_events.to_zarr(output_file_small)
        print(f'written to disk successfully with small suffix!')