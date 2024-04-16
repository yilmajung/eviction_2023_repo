# Import the requisite packages
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import fiona
fiona.supported_drivers

# Load eviction filing report data
data_path = r'/Users/wooyongjung/WJ_Projects/eviction_2023_repo/data/EvictionRecords_Master.csv'
df_eviction = pd.read_csv(data_path)

# Convert the pandas dataframe to geopandas dataframe
df_eviction_gpd = gpd.GeoDataFrame(df_eviction, geometry=gpd.points_from_xy(df_eviction.X, df_eviction.Y, crs=4326))

# Select Dallas County only (lat_min=32.545214, lon_min=-97.038685, lat_max=32.989691, lon_max=-96.51687)
df_eviction_gpd = df_eviction_gpd.loc[(df_eviction_gpd['X']>-97.038685)&(df_eviction_gpd['X']<-96.51687)&(df_eviction_gpd['Y']>32.545214)&(df_eviction_gpd['Y']<32.989691)]

# Dallas County Census Block Group shapefile (2019, prepandemic)
data_path = '/Users/wooyongjung/WJ_Projects/eviction_2023_repo/data/tl_2019_48_bg/tl_2019_48_bg.shp'
census_bg = gpd.read_file(data_path)
census_bg = census_bg.to_crs(4326)
census_bg = census_bg[census_bg['GEOID'].str.startswith('48113')]

# Aggregate the eviction locations to the Dallas County census BG
eviction_bg = gpd.sjoin(census_bg, df_eviction_gpd, how='left', op='contains')

# Save the aggregated data
eviction_bg.to_file('data/eviction_count_bg_2019.geojson', driver="GeoJSON")