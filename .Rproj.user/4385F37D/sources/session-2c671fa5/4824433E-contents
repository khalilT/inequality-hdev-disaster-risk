## Aggregation of climate variables to EM-DAT events

This repository contains a series of Python scripts for joining the geocoded event locations with the EM-DAT event and impact information necessary for the analysis.

### Requirements
pandas, geopandas, xarray, numpy, re, shapely, rioxarray, multiprocessing


### Overview

#### 1_correct_geolocations.py

This script corrects some errors in the database: missing date informations in some events, and removes some events with inaccurate geolocations.

Inputs: disaster_events_national_1990_2020
Outputs: disaster_events_national_1990_2020

#### 2_process_surface_moisture_data.py

This script is for extracting population and GDP in every geometry corresponding to disaster event

Inputs: disaster_events_national_1990_2020, population GHS count, GDP count
Outputs: dataframe with population count per event, dataframe with GDP count per event

#### 3_calculate_sHDI_popweight.py

This script is for the extraction of subnational Human Development Index (sHDI) from the global data lab (GDL) for every event location.

Inputs: GDL sHDI data, disaster_events_national_1990_2020, disaster_event_locations_1990_2020
Outputs: dataframe with pop weighted sHDI per event

#### 4_normalize_impacts_create_db.py

This script is for the normalization of the reported event impacts. For every event, human losses (people affected and fatalities) are normalized by the total population of the region impacted by disaster (extracted in 3). Similarly, economic damage figures are adjusted to 2011 dollars, then normalized by the total GDP of the reported region.

Inputs: disaster_events_national_1990_2020, dataframe with population count per event, dataframe with GDP count per event
Outputs: disaster_events_1990_2020 normalized impacts

#### 5_extract_climate_variables.py

In this script, we extract the climate variables corresponding to the disaster hazards at their peak. We extract the maximum, minimum, and mean values of the variables in 3 different dimensions: original values, anomalies in original units and zscored anomalies.

Inputs: disaster_events_national_1990_2020, aggregated climate variables
Outputs: dataframe with extracted climate variables

#### 6_create_dataframe.py

This script is for creation of the dataframe that will be used throught the analysis. 

Inputs: dataframe with extracted climate variables, disaster_events_1990_2020 normalized impacts, pop weighted sHDI
Outputs: analysis ready data

#### create_gdp_data.py
This script preprocesses the gridded GDP data and prepares it for use in order to estimate the exposed GDP in every geocoded event location.

#### create_population_data.py
This script preprocesses the GHS population data and prepares it for use in order to estimate the exposed population in every geocoded event location.

#### get_gdl_popweights.py
This script is used to calculate the population weights for every event location, in order to calculate the sHDI weighted average for the whole event area.

### How to Run the Scripts

Ensure that the required input files are available in the specified directories.
Run the scripts in the src folder to prepare the needed intermediate data.
Run each script in the appropriate order indicated in the names.

### Notes

