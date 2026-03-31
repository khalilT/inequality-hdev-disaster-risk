### (c) Khalil Teber 2024 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script is for the extraction of subnational Human Development Index (sHDI) from the global data lab (GDL) (https://globaldatalab.org/) for each event location, and calculating the weighted population average.

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd
import rioxarray


### read disaster data
disasters = gpd.read_file('../data/intermediate_data/geolocated_climate_events_1990-2023_simplified_clean.gpkg')
disasters['year'] = disasters['DisNo.'].str[:4]
disasters['year'] = disasters['year'].astype(int)
disasters["occ_number"] = (disasters.groupby("DisNo.").cumcount()+1).astype(str)
disasters['event_poly_id'] = disasters[['DisNo.','occ_number']].agg('-'.join, axis=1)
disasters = disasters.sort_values("event_poly_id").drop(columns={"occ_number"})
disasters = disasters[disasters["year"] < 2021]

### read location pop weight data
disasters_merge_data = pd.read_csv("../data/intermediate_data/to_merge_popweight_subnational.csv")
disasters_merge_data_tojoin = disasters_merge_data.drop(columns={"Unnamed: 0","DisNo."})
disasters_merge_data_tojoin = disasters_merge_data_tojoin.set_index("event_poly_id")


### read GDL data
gdl_shapes = gpd.read_file('../data/raw_data/global_data_lab/GDL_Shapefiles_V6/shdi2022_World_large.shp')
gdl_shapes = gdl_shapes.to_crs('epsg:4326')

##needed correction to GDL map

#Bahrain
gdl_shapes.loc[148,'continent'] = 'Asia/Pacific'
gdl_shapes.loc[148,'iso_code'] = 'BHR'
#Taiwan
gdl_shapes.loc[304,'continent'] = 'Asia/Pacific'
gdl_shapes.loc[304,'iso_code'] = 'TWN'
#Cuba
gdl_shapes.loc[400,'continent'] = 'America'
gdl_shapes.loc[400,'iso_code'] = 'CUB'
gdl_shapes.loc[401,'continent'] = 'America'
gdl_shapes.loc[401,'iso_code'] = 'CUB'
gdl_shapes.loc[402,'continent'] = 'America'
gdl_shapes.loc[402,'iso_code'] = 'CUB'
gdl_shapes.loc[403,'continent'] = 'America'
gdl_shapes.loc[403,'iso_code'] = 'CUB'
gdl_shapes.loc[404,'continent'] = 'America'
gdl_shapes.loc[404,'iso_code'] = 'CUB'
gdl_shapes.loc[405,'continent'] = 'America'
gdl_shapes.loc[405,'iso_code'] = 'CUB'
gdl_shapes.loc[406,'continent'] = 'America'
gdl_shapes.loc[406,'iso_code'] = 'CUB'
gdl_shapes.loc[407,'continent'] = 'America'
gdl_shapes.loc[407,'iso_code'] = 'CUB'
gdl_shapes.loc[408,'continent'] = 'America'
gdl_shapes.loc[408,'iso_code'] = 'CUB'
gdl_shapes.loc[409,'continent'] = 'America'
gdl_shapes.loc[409,'iso_code'] = 'CUB'
gdl_shapes.loc[410,'continent'] = 'America'
gdl_shapes.loc[410,'iso_code'] = 'CUB'
#greenland
gdl_shapes.loc[640,'continent'] = 'America'
gdl_shapes.loc[640,'iso_code'] = 'GRL'
#Malta
gdl_shapes.loc[1090,'continent'] = 'Europe'
gdl_shapes.loc[1090,'iso_code'] = 'MLT'
#Puerto Rico
gdl_shapes.loc[1349,'continent'] = 'Europe'
gdl_shapes.loc[1349,'iso_code'] = 'MLT'
gdl_shapes.loc[1350,'continent'] = 'Europe'
gdl_shapes.loc[1350,'iso_code'] = 'MLT'
gdl_shapes.loc[1351,'continent'] = 'Europe'
gdl_shapes.loc[1351,'iso_code'] = 'MLT'
gdl_shapes.loc[1352,'continent'] = 'Europe'
gdl_shapes.loc[1352,'iso_code'] = 'MLT'
gdl_shapes.loc[1353,'continent'] = 'Europe'
gdl_shapes.loc[1353,'iso_code'] = 'MLT'
gdl_shapes.loc[1354,'continent'] = 'Europe'
gdl_shapes.loc[1354,'iso_code'] = 'MLT'
#North Korea
gdl_shapes.loc[1355,'continent'] = 'Asia/Pacific'
gdl_shapes.loc[1355,'iso_code'] = 'PRK'
gdl_shapes = gdl_shapes.rename(columns={'continent':'continent_gdl'})


#1 extract GDL data
#######################################

disasters = disasters.reset_index()
event_shapes = disasters.set_index('DisNo.')
event_shapes = event_shapes.rename(columns={"ISO":"iso"})

def find_gdl_region(event_index):    

    left_gdf = event_shapes.loc[[event_index]]
    left_gdf["geometry"] = left_gdf["geometry"].buffer(0)

    right_gdf = gdl_shapes[gdl_shapes.iso_code == left_gdf.iso[0]]
    
    if len(right_gdf) > 0:

        event_region_intersect = gpd.tools.sjoin(left_gdf, right_gdf, how="left", predicate="intersects")
        event_region_intersect = event_region_intersect[~pd.isna(event_region_intersect.index_right)]

        event_region_intersect['intersection'] = [a.intersection(right_gdf[right_gdf.gdlcode == b].geometry.values[0]).area for a, b in zip(event_region_intersect.geometry.values, event_region_intersect.gdlcode)]

        event_region_intersect = event_region_intersect.sort_values(by=['event_poly_id','intersection'])
        event_region_intersect = event_region_intersect.drop(columns='geometry')

        if len(event_region_intersect) > 1:
            result = event_region_intersect.drop_duplicates(subset=['event_poly_id'],keep='last')
        else:
            result = event_region_intersect

        return(result)
    else:
        return(left_gdf.drop(columns='geometry'))
    
    
indices_events = np.unique(event_shapes.index)\

import multiprocessing

%%time
with  multiprocessing.Pool(30) as pool:
    res  = pool.map(find_gdl_region,indices_events)
    
    
disasters_gdl = pd.concat(res)

disasters_gdl_codes = disasters_gdl[["year","gdlcode","event_poly_id","index_right","continent_gdl","iso_code"]].reset_index()
disasters_gdl_codes = disasters_gdl_codes.set_index("event_poly_id").join(disasters_merge_data_tojoin).reset_index()

#2 Calculate sHDI and extract national HDI data
#######################################

gdl_data = pd.read_csv("../data/raw_data/global_data_lab/gdl/global_download/SHDI-SGDI-Total7.0.csv")


### subnational sHDI (and subindices for education, income and health)
gdl_data_subnational = gdl_data[gdl_data["level"] == "Subnat"]
gdl_data_subnational = gdl_data_subnational[["iso_code","GDLCODE",'year',"shdi","healthindex","incindex","edindex"]]
gdl_data_subnational = gdl_data_subnational[~(gdl_data_subnational["shdi"] == ' ')]
gdl_data_subnational["shdi"] = gdl_data_subnational["shdi"].astype(float)
gdl_data_subnational = gdl_data_subnational.reset_index().drop(columns="index")
gdl_data_subnational = gdl_data_subnational[["year","GDLCODE","shdi","healthindex","incindex","edindex"]]
gdl_data_subnational = gdl_data_subnational.reset_index()
gdl_data_subnational = gdl_data_subnational.rename(columns={"GDLCODE":"gdlcode"}).set_index(["year","gdlcode"])
subnational_gdl_joined = disasters_gdl_codes.join(gdl_data_subnational)

weight_column = 'pop_weight'  
columns_to_average = ['shdi', 'healthindex', 'incindex', 'edindex']
def calculate_weighted_average(group):
    if weight_column not in group.columns:
        print(f"Warning: '{weight_column}' not in group, returning NaNs")
        return pd.Series({col: np.nan for col in columns_to_average})

    series = {}
    for column in columns_to_average:
        if column in group.columns:
            group[column] = pd.to_numeric(group[column], errors='coerce')
            if group[column].isnull().any():
                print(f"Warning: Column '{column}' contains NaN values in group, results may be affected")
            series[column] = np.average(group[column], weights=group[weight_column])
        else:
            print(f"Warning: Column '{column}' not found in group, returning NaN")
            series[column] = np.nan
    return pd.Series(series)

subnational_gdl_joined = subnational_gdl_joined.groupby('DisNo.').apply(calculate_weighted_average)
subnational_gdl_joined = subnational_gdl_joined.reset_index().rename(columns={"shdi":"sHDI"})
iso_data = disasters_gdl_codes.reset_index()[["DisNo.","iso_code"]].drop_duplicates().set_index("DisNo.")
subnational_gdl_joined = subnational_gdl_joined.set_index("DisNo.").join(iso_data)
subnational_gdl_joined_clean = subnational_gdl_joined.dropna()
events_shdi = subnational_gdl_joined_clean.reset_index()
events_shdi.to_csv("../data/intermediate_data/event_shdi.csv")


### national HDI
gdl_data_national = gdl_data[gdl_data["level"] == "National"]
gdl_data_national = gdl_data_national[["iso_code",'year',"shdi"]]
gdl_data_national = gdl_data_national[~(gdl_data_national["shdi"] == ' ')]
gdl_data_national["shdi"] = gdl_data_national["shdi"].astype(float)
gdl_data_national = gdl_data_national.reset_index().drop(columns="index")
gdl_data_national.to_csv("../data/intermediate_data/national_hdi.csv")
