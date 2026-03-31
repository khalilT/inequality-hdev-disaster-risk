## Aggregation of climate variables to EM-DAT events

This repository contains a series of Python scripts for aggregating grid-cell climate data (in our case ERA5 and GLEAM products) to geocoded EM-DAT events  for the period 1990-2020 in order to obtain daily time series.

### Requirements

To run these scripts, you need the following very specific Python libraries installed. We recommend installing and using this conda environment:

conda create -n agg_env1 python=3.7.12 numpy=1.21.6
conda activate agg_env1
conda install xagg=0.3.0.2
conda install ipykernel=6.15.0 rioxarray=0.9.1 zarr=2.11.3 multiprocess=0.70.14
pip install dea-tools==0.2.7

python -m ipykernel install --user --name 'agg_env1' --display-name "agg python=3.7"

### Overview

#### 1_process_monthly_data.py

This script is for processing the ERA5 global (0.25deg) data downloaded with an hourly temporal resolution, and aggregate them to daily.

Inputs: hourly ERA5 data
Outputs: daily ERA5 data

#### 2_process_surface_moisture_data.py

This script is for resampling GLEAM 4.1a surface moisture variable to the same spatial resolution as used ERA5.

Inputs: daily surface mositure data (0.1deg)
Outputs: daily surface mositure data (0.25deg)

#### 3_extract_anomalies.py

This script is for processing the climate data (ERA5 / GLEAM) (0.25deg), and calculating the anomalies in the origianl units by removing the mean seasonal cycle of the whole climatology (1985 - 2023), and then calculating the normalized anomalies per pixel by dividing the anomnalies by the standard deviation of the whole climatology.

Inputs: ERA5 data in original units
Outputs: Anomalies of ERA5 variablaes in original units and Zscored 

#### 4_aggregate_iso.py

This script is for the aggregation of the climate data for every event, to extract time series of the different variables 3 years before and 3 years after the reported start of the event.

Inputs: Geolocated EM-DAT events, climate data, landcover data,
Outputs: Zarr objects with aggregated time series of climate variables per event

#### 5_concat_clim_iso.py

In this script, we concatenate all objects calculated in 4_aggregate_iso.py and add metadata

Inputs: Zarr objects with aggregated time series of climate variables per event
Outputs: Concatenated time series of all geocoded events

#### src.read_data.py
This script allows reading the data needed for the aggregation of the climate variables.

#### src.aggregation_functions.py
This script contains all the functions needed for the aggregation of the climate variables.

### How to Run the Scripts

Ensure that the required input files (geolocated events, climate data, landcover data) are available in the specified directories.
Run each script in the appropriate order indicated in the names.

### Notes

The demands of data storage, RAM and processing power are very intense for a few days (5 days in our case) to run 4_aggregate_iso.py.

1.3 Tb is needed for the storage of the climate data in original unists, calculated anomalies, and zascored anomalies
To minimize computation time, the aggregation is performed as follows: For every country, one variable is loaded into memory at a time, then aggregated to all events, then the next variables is used, until all are processed. For big countries, this means up to 150Gb in RAM.
We rely heavily on muti-processing and multi-threading. The script can use up to 30 CPU cores.

We used land cover data to aggregate the climate variables to land cover types for each event. However, this is relevant only in the case of large areas, additionally, land cover was not used in this analysis, therefore, we don't provide any scripts for the preproessing or use of this data. Further information could be provided when needed.