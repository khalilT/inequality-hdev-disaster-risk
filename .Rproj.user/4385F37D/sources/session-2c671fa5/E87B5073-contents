### (c) Khalil Teber 2024-2025 ###
### Code file for PAPER "Inequality in human development amplifies climate-related disaster risk"

### This script is for the calculation of the climate fingerprints, using the Superposed Epoch analysis (also called composite analysis).

import xarray as xr
import numpy as np
import geopandas as gpd
import pandas as pd
import multiprocessing


disasters_df = gpd.read_file("../data/input/disaster_information.csv")
disasters_df = disasters_df.set_index('Dis No')

clim_data = xr.open_zarr('../data/intermediate_data/climate_data.zarr')
clim_data = clim_data.sel(event = disasters_df.index)

variable_list = ["t2mmax","t2mmin","rh","tp","tro","smsurf","windgustmax"]
clim_data = clim_data[variable_list]

#1 Fingerprints per disaster type
#######################################

###functions necessary for the fingerprint calculation
def shape_data_sea(disaster_type, variable_units):
    disaster_events = disasters_df[disasters_df.disaster_type == disaster_type]
    clim_data_disaster = clim_data.sel(event = disaster_events.index, units=variable_units, landcover='all')
    clim_data_disaster = clim_data_disaster[variable_list]
    clim_data_disaster_df = clim_data_disaster.to_dataframe()
    clim_data_disaster_df = clim_data_disaster_df.reset_index()
    clim_data_disaster_df = clim_data_disaster_df.drop(columns={'landcover','units'})
    return(clim_data_disaster_df)

def sample_ci(x):
    return np.nanmean(np.random.choice(df[var],n_events))

def sea_analysis_data(df, variable, window, ci):
    n_events = len(np.unique(df['event']))
    df_filtered = df.groupby('event').apply(lambda x : x.iloc[1095-window:1095+window,:])
    pool = multiprocessing.Pool(20)
    random_samples = pool.map(sample_ci,np.arange(100))#1000
    cinterval = np.quantile(random_samples, q =(ci[0], ci[1]))
    sea_mean = df_filtered[['time',variable]].groupby(['time']).mean()
    return sea_mean, cinterval

###caluclate fingerprints per disaster type

sea_results = {}
for disaster_type in np.unique(disasters_df.disaster_type):

    df = shape_data_sea(disaster_type, 'zscored')
    n_events = len(np.unique(df['event']))
    disaster_var = []
    for var in df.columns[2:]:
        disaster_var.append(sea_analysis_data(df, var,1095, (0.05, 0.95)))
    sea_results[disaster_type] = disaster_var

#organize results in dictionary
result_dict = {}
for dis_type in np.unique(disasters_df.disaster_type):
    df = pd.DataFrame()
    for i in np.arange(len(variable_list)):
        var_data = sea_results[dis_type][i][0]
        df = df.join(var_data,how="outer")
    result_dict[dis_type] = df
    
    
distypes_list = ['Flood','Storm','Landslide','Cold wave','Heat wave','Drought','Wildfire']
#correct climate variable names
for disaster_type in distypes_list:
    result_dict[disaster_type] = result_dict[disaster_type].rename(columns={"t2mmax":"max t2m","t2mmin":"min t2m","rh":"rel. humidity","tp":"total precip.",
                                                    "tro":"total runoff","smsurf":"surf. moisture","windgustmax":"max windgust","spei_30":"SPEI 30",
                                                    "tccmean":"cloud cover"})
    
#save dataframe with clim data for plotting in R
all_dfs = []
for disaster_type in distypes_list:
    disaster_df = result_dict[disaster_type].loc[(-365,-180,-90,-60,-45,-30,-15,-7,-1,0,1,7,15,30,45,60,90,180,365),:]
    disaster_df["disaster_type"] = disaster_type
    disaster_df = disaster_df.reset_index()
    all_dfs.append(disaster_df)
    
all_dfs = pd.concat(all_dfs)
all_dfs.to_csv("../data/input/disaster_types_sd_climatology.csv")

#2 Fingerprints per disaster type and sHDI group
#######################################

### function to calculate fingerprint per disaster type and sHDI
def shape_data_hdi_sea(disaster_type, variable_units,hdi_idx):
    disasters_hdi = disasters_df.loc[hdi_idx]
    disaster_events = disasters_hdi[disasters_hdi.disaster_type == disaster_type]
    clim_data_disaster = clim_data.sel(event = disaster_events.index,
                                    units=variable_units, landcover='all')
    clim_data_disaster = clim_data_disaster[variable_list]
    clim_data_disaster_df = clim_data_disaster.to_dataframe()
    clim_data_disaster_df = clim_data_disaster_df.reset_index()
    clim_data_disaster_df = clim_data_disaster_df.drop(columns={'landcover','units'})
    return(clim_data_disaster_df)

### indices of sHDI group
low_hdi_idx = disasters_df[disasters_df["sHDI_category"] == "Low sHDI"].index.values
medium_hdi_idx = disasters_df[disasters_df["sHDI_category"] == "Medium sHDI"].index.values
high_hdi_idx = disasters_df[disasters_df["sHDI_category"] == "High sHDI"].index.values
vhigh_hdi_idx = disasters_df[disasters_df["sHDI_category"] == "Very high sHDI"].index.values


### Low sHDI
sea_results_low_hdi = {}
for disaster_type in np.unique(disasters_df.disaster_type):

    df = shape_data_hdi_sea(disaster_type, 'zscored',low_hdi_idx)
    n_events = len(np.unique(df['event']))
    
    disaster_var = []
    for var in df.columns[2:]:
        disaster_var.append(sea_analysis_data(df, var,1095, (0.05, 0.95)))
    sea_results_low_hdi[disaster_type] = disaster_var
    
result_dict_low_hdi = {}
for dis_type in np.unique(disasters_df.disaster_type):
    df = pd.DataFrame()
    for i in np.arange(len(variable_list)):
        test = sea_results_low_hdi[dis_type][i][0]
        df = df.join(test,how="outer")
    result_dict_low_hdi[dis_type] = df
    
### Medium sHDI
sea_results_medium_hdi = {}
for disaster_type in np.unique(disasters_df.disaster_type):

    df = shape_data_hdi_sea(disaster_type, 'zscored',medium_hdi_idx)
    n_events = len(np.unique(df['event']))
    
    disaster_var = []
    for var in df.columns[2:]:
        disaster_var.append(sea_analysis_data(df, var,1095, (0.05, 0.95)))
    sea_results_medium_hdi[disaster_type] = disaster_var
    
result_dict_medium_hdi = {}
for dis_type in np.unique(disasters_df.disaster_type):
    df = pd.DataFrame()
    for i in np.arange(len(variable_list)):
        test = sea_results_medium_hdi[dis_type][i][0]
        df = df.join(test,how="outer")
    result_dict_medium_hdi[dis_type] = df

### High sHDI

sea_results_high_hdi = {}
for disaster_type in np.unique(disasters_df.disaster_type):

    df = shape_data_hdi_sea(disaster_type, 'zscored',high_hdi_idx)
    n_events = len(np.unique(df['event']))
    
    disaster_var = []
    for var in df.columns[2:]:
        disaster_var.append(sea_analysis_data(df, var,1095, (0.05, 0.95)))
    sea_results_high_hdi[disaster_type] = disaster_var
    
result_dict_high_hdi = {}
for dis_type in np.unique(disasters_df.disaster_type):
    df = pd.DataFrame()
    for i in np.arange(len(variable_list)):
        test = sea_results_high_hdi[dis_type][i][0]
        df = df.join(test,how="outer")
    result_dict_high_hdi[dis_type] = df


### Very high sHDI
sea_results_vhigh_hdi = {}
for disaster_type in np.unique(disasters_df.disaster_type):

    df = shape_data_hdi_sea(disaster_type, 'zscored',vhigh_hdi_idx)
    n_events = len(np.unique(df['event']))
    
    disaster_var = []
    for var in df.columns[2:]:
        disaster_var.append(sea_analysis_data(df, var,1095, (0.05, 0.95)))
    sea_results_vhigh_hdi[disaster_type] = disaster_var
    
result_dict_vhigh_hdi = {}
for dis_type in np.unique(disasters_df.disaster_type):
    df = pd.DataFrame()
    for i in np.arange(len(variable_list)):
        test = sea_results_vhigh_hdi[dis_type][i][0]
        df = df.join(test,how="outer")
    result_dict_vhigh_hdi[dis_type] = df

    
###Extract the fingerprints for every disaster type from all sHDI groups

flood = {"Low sHDI":result_dict_low_hdi['Flood'],
"Medium sHDI":result_dict_medium_hdi['Flood'],
"High sHDI":result_dict_high_hdi['Flood'],
"Very high sHDI":result_dict_vhigh_hdi['Flood']}

strom = {"Low sHDI":result_dict_low_hdi['Storm'],
"Medium sHDI":result_dict_medium_hdi['Storm'],
"High sHDI":result_dict_high_hdi['Storm'],
"Very high sHDI":result_dict_vhigh_hdi['Storm']}

landslide = {"Low sHDI":result_dict_low_hdi['Landslide'],
"Medium sHDI":result_dict_medium_hdi['Landslide'],
"High sHDI":result_dict_high_hdi['Landslide'],
"Very high sHDI":result_dict_vhigh_hdi['Landslide']}

cold_wave = {"Low sHDI":result_dict_low_hdi['Cold wave'],
"Medium sHDI":result_dict_medium_hdi['Cold wave'],
"High sHDI":result_dict_high_hdi['Cold wave'],
"Very high sHDI":result_dict_vhigh_hdi['Cold wave']}

heat_wave = {"Low sHDI":result_dict_low_hdi['Heat wave'],
"Medium sHDI":result_dict_medium_hdi['Heat wave'],
"High sHDI":result_dict_high_hdi['Heat wave'],
"Very high sHDI":result_dict_vhigh_hdi['Heat wave']}

drought = {"Low sHDI":result_dict_low_hdi['Drought'],
"Medium sHDI":result_dict_medium_hdi['Drought'],
"High sHDI":result_dict_high_hdi['Drought'],
"Very high sHDI":result_dict_vhigh_hdi['Drought']}

wildfire = {"Low sHDI":result_dict_low_hdi['Wildfire'],
"Medium sHDI":result_dict_medium_hdi['Wildfire'],
"High sHDI":result_dict_high_hdi['Wildfire'],
"Very high sHDI":result_dict_vhigh_hdi['Wildfire']}

hdi_cats = ['Low sHDI', 'Medium sHDI','High sHDI','Very high sHDI']

### correct variable names for each disaster type

for hdi_category in hdi_cats:
    flood[hdi_category] = flood[hdi_category].rename(columns={"t2mmax":"max t2m","t2mmin":"min t2m","rh":"rel. humidity","tp":"total precip.",
                                                    "tro":"total runoff","smsurf":"surf. moisture","windgustmax":"max windgust"})
for hdi_category in hdi_cats:
    strom[hdi_category] = strom[hdi_category].rename(columns={"t2mmax":"max t2m","t2mmin":"min t2m","rh":"rel. humidity","tp":"total precip.",
                                                    "tro":"total runoff","smsurf":"surf. moisture","windgustmax":"max windgust"})
for hdi_category in hdi_cats:
    landslide[hdi_category] = landslide[hdi_category].rename(columns={"t2mmax":"max t2m","t2mmin":"min t2m","rh":"rel. humidity","tp":"total precip.",
                                                    "tro":"total runoff","smsurf":"surf. moisture","windgustmax":"max windgust"})
for hdi_category in hdi_cats:
    cold_wave[hdi_category] = cold_wave[hdi_category].rename(columns={"t2mmax":"max t2m","t2mmin":"min t2m","rh":"rel. humidity","tp":"total precip.",
                                                    "tro":"total runoff","smsurf":"surf. moisture","windgustmax":"max windgust"})
for hdi_category in hdi_cats:
    heat_wave[hdi_category] = heat_wave[hdi_category].rename(columns={"t2mmax":"max t2m","t2mmin":"min t2m","rh":"rel. humidity","tp":"total precip.",
                                                    "tro":"total runoff","smsurf":"surf. moisture","windgustmax":"max windgust"})
for hdi_category in hdi_cats:
    wildfire[hdi_category] = wildfire[hdi_category].rename(columns={"t2mmax":"max t2m","t2mmin":"min t2m","rh":"rel. humidity","tp":"total precip.",
                                                    "tro":"total runoff","smsurf":"surf. moisture","windgustmax":"max windgust"})
for hdi_category in hdi_cats:
    drought[hdi_category] = drought[hdi_category].rename(columns={"t2mmax":"max t2m","t2mmin":"min t2m","rh":"rel. humidity","tp":"total precip.",
                                                    "tro":"total runoff","smsurf":"surf. moisture","windgustmax":"max windgust"})
    
disasters_hdi_dict = {"Flood":flood,"Storm":strom,"Landslide":landslide,"Cold wave":cold_wave, "Heat wave":heat_wave,"Drought":drought,"Wildfire":wildfire}

#save dataframe with clim data for plotting in R
all_dfs = []
for disaster_type in distypes_list:
    for hdi_cat in hdi_cats:
        disaster_df = disasters_hdi_dict[disaster_type][hdi_cat].loc[(-365,-180,-90,-60,-45,-30,-15,-7,-1,0,1,7,15,30,45,60,90,180,365),:]
        disaster_df["disaster_type"] = disaster_type
        disaster_df["hdi_category"] = hdi_cat
        disaster_df = disaster_df.reset_index()
        all_dfs.append(disaster_df)
        
all_dfs = pd.concat(all_dfs)

all_dfs.to_csv("../data/input/disaster_types_hdi_sd_climatology.csv")
