### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script corrects some errors in the database: missing date informations in some events, and removes some events with inaccurate geolocations.

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd


disaster_locations_1990_2020 = gpd.read_file("../data/intermediate_data/disaster_events_national_1990_2020.gpkg")


#1 event attribute correction
#######################################

#correct detected wrong dates

disaster_locations_1990_2020.loc["2006-0480-BDI","end_date"] = "2006-09-03"
disaster_locations_1990_2020.loc["2006-0480-BDI","end_day"] = 8
disaster_locations_1990_2020.loc["2006-0480-BDI","event_duration"] = 8

disaster_locations_1990_2020.loc["2007-0610-IDN","end_date"] = "2008-01-02"
disaster_locations_1990_2020.loc["2007-0610-IDN","end_day"] = 8
disaster_locations_1990_2020.loc["2007-0610-IDN","event_duration"] = 8

disaster_locations_1990_2020.loc["2010-0633-BIH","end_date"] = "2010-12-30"
disaster_locations_1990_2020.loc["2010-0633-BIH","end_day"] = 11
disaster_locations_1990_2020.loc["2010-0633-BIH","event_duration"] = 11

disaster_locations_1990_2020.loc["2010-0633-SRB","end_date"] = "2010-12-30"
disaster_locations_1990_2020.loc["2010-0633-SRB","end_day"] = 11
disaster_locations_1990_2020.loc["2010-0633-SRB","event_duration"] = 11

#drop inaccurate events

disaster_locations_1990_2020 = disaster_locations_1990_2020.set_index("DisNo.").drop(["1993-0585-IRN", "1999-0298-USA", "2022-0863-USA"]).reset_index().sort_values("DisNo.").reset_index().drop(columns="index")



disaster_locations_1990_2020.to_file("../data/intermediate_data/disaster_events_national_1990_2020.gpkg", driver="GPKG")