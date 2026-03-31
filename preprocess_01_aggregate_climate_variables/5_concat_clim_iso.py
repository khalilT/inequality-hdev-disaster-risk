### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script reads and concatenates the climate time series estimated with script (4) in one zarr object, and adds the corresponding metadata.

import xarray as xr
import numpy as np
import pandas as pd

import glob
file_paths = glob.glob("../data/intermediate_data/clim_iso/*")

data_events = []

for i in np.arange(len(file_paths)):
    data_events.append(xr.open_zarr(file_paths[i]))



data_events = [xr.open_zarr(f) for f in file_paths]



combined_dataset = xr.concat(data_events, dim='event')

combined_dataset = combined_dataset.sortby("event")

combined_dataset = combined_dataset.chunk(chunks=dict(event=100, landcover=4, units=3, time=2191)).astype('float32')

combined_dataset = combined_dataset.rename_vars({"NDVI":"ndvi","SMs":"smsurf"})

combined_dataset["t2m"].attrs = {'name':'t2m',
                           'long_name': 'mean daily 2m air temperature',
                           'units':'°C',
                           'source':'ERA5'}
combined_dataset["t2mmax"].attrs = {'name':'t2mmax',
                           'long_name': 'maximum daily 2m air temperature',
                           'units':'°C',
                           'source':'ERA5'}
combined_dataset["t2mmin"].attrs = {'name':'t2mmin',
                           'long_name': 'minimum daily 2m air temperature',
                           'units':'°C',
                           'source':'ERA5'}
combined_dataset["tp"].attrs = {'name':'tp',
                           'long_name': 'total precipitation',
                           'units':'mm d^-1',
                           'source':'ERA5'}
combined_dataset["smsurf"].attrs = {'name':'smsurf',
                           'long_name': 'surface soil moisture',
                           'units':'mm d^-1',
                           'source':'GLEAM v3.6a'}
combined_dataset["rh"].attrs = {'name':'rh',
                           'long_name': 'relative humidity',
                           'units':'%',
                           'source':'calculated from ERA5 temperature and dew point temperature'}
combined_dataset["tro"].attrs = {'name':'tro',
                           'long_name': 'runoff',
                           'units':'mm d^-1',
                           'source':'ERA5'}
combined_dataset["windgustmax"].attrs = {'name':'windgustmax',
                           'long_name': 'maximum daily 10m wind gust since previous post-processing',
                           'units':'km/h',
                           'source':'aggregated from ERA5'}
combined_dataset["windgustmean"].attrs = {'name':'windgustmean',
                           'long_name': 'mean daily 10m wind gust since previous post-processing',
                           'units':'km/h',
                           'source':'aggregated from ERA5'}

combined_dataset

#General dataset metadta
combined_dataset.attrs = {
    'acknowledgment':['https://www.emdat.be/',
                      'https://data.apps.fao.org/catalog/dataset?tags=Gaul',
                     'https://www.ecmwf.int/',
                     'https://www.gleam.eu/',
                     'https://lpdaac.usgs.gov/products/mcd43c4v006/',
                     'https://www.esa-landcover-cci.org/'],
    'contributor_name':'Remote Sensing Centre for Earth System Research (RSC4Earth) - University of Leipzig',
    'contributor_url': 'https://rsc4earth.de/',
    'creator_name':'Remote Sensing Centre for Earth System Research (RSC4Earth) - University of Leipzig',
    'creator_url': 'https://rsc4earth.de/',
    'authors': ['Khalil Teber', 'Miguel Mahecha'],
    'email':['khalil.teber@uni-leipzig.de','miguel.mahecha@uni-leipzig.de'],
    'date_creation' : 'Sep 2024',
    'institution':'Remote Sensing Centre for Earth System Research (RSC4Earth) - University of Leipzig',
    'license':'Terms and conditions of the used databases in acknowledgements apply',
    'processing_steps':'Database documentation D3.1 report',
    'project': 'XAIDA - eXtreme events: Artificial Intelligence for Detection and Attribution',
    'publisher_name':'Remote Sensing Centre for Earth System Research (RSC4Earth) - University of Leipzig',
    'publisher_url': 'https://rsc4earth.de/',
    'version':'4.0',
    'major_updates':'Event Georeferencing based on GAUL (FAO) geometries in accordance with EM-DAT reporting, adding of NDVI variable',
    'title' :'Database of extreme climate events and associated impacts on society (D3.1)'
}

combined_dataset.to_zarr('../data/intermediate_data/climate_data.zarr')

