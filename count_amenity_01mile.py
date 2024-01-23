import utm
import numpy as np
import pandas as pd
import time


# Define a function creating smaller bounding boxes
def create_bbox(w, lat_min, lon_min, lat_max, lon_max):
    '''
    Create a dataframe where each column represents latitude and longitude of
    southern, western, northern, and eastern edge points of an inner bounding box
    as well as the center point of the bounding boxes

    Parameters
    ----------
    w: width (miles) of a bounding box (inner square)
    lat_min: minimum latitude of the whole boundary
    lon_min: minimum longitude of the whole boundary
    lat_max: maximum latitude of the whole boundary
    lon_max: maximum longitude of the whole boundary

    '''

    # Convert latitude and longitude to UTM coordinate
    easting_init, northing_init, zone_number_init, zone_letter_init = utm.from_latlon(lat_min, lon_min)
    easting_end, northing_end, zone_number_end, zone_letter_end = utm.from_latlon(lat_max, lon_max)

    # Convert the unit of bbox width and height to meters
    bbox_width_meter, bbox_height_meter = 1609.344 * w, 1609.344 * w

    if (zone_number_init == zone_number_end) and (zone_letter_init == zone_letter_end):

        # Find the bbox's latitude and longitude
        # for southern, western, northern, and eastern edge
        south = [northing_init]
        west = [easting_init]

        bbox_south = northing_init
        bbox_west = easting_init

        while bbox_south <= northing_end:
            bbox_south += bbox_height_meter
            south.append(bbox_south)

        while bbox_west <= easting_end:
            bbox_west += bbox_width_meter
            west.append(bbox_west)

        south_edge = []
        west_edge = []
        north_edge = []
        east_edge = []
        center_point_x = []
        center_point_y = []
        row_num = []
        col_num = []

        for i in range(len(south) - 1):

            for j in range(len(west) - 1):
                row_num.append(i)
                col_num.append(j)
                south_edge.append(south[i])
                west_edge.append(west[j])
                north_edge.append(south[i + 1])
                east_edge.append(west[j + 1])
                center_point_x.append((west[j] + west[j + 1]) / 2)
                center_point_y.append((south[i] + south[i + 1]) / 2)

        df_bbox = pd.DataFrame({'row_num': row_num,
                                'col_num': col_num,
                                'south_edge': south_edge,
                                'west_edge': west_edge,
                                'north_edge': north_edge,
                                'east_edge': east_edge,
                                'center_x': center_point_x,
                                'center_y': center_point_y, })

    else:
        print("error: zone_number and zone_letter are not consistent")

    # Convert utm back to latitude and longitude
    df_bbox['south_west_lat_long'] = df_bbox[['south_edge', 'west_edge']].apply(lambda df_bbox:
                                                                                utm.to_latlon(df_bbox['west_edge'],
                                                                                              df_bbox['south_edge'],
                                                                                              zone_number_init,
                                                                                              zone_letter_init), axis=1)
    df_bbox['north_east_lat_long'] = df_bbox[['north_edge', 'east_edge']].apply(lambda df_bbox:
                                                                                utm.to_latlon(df_bbox['east_edge'],
                                                                                              df_bbox['north_edge'],
                                                                                              zone_number_init,
                                                                                              zone_letter_init), axis=1)
    df_bbox['center_latlon'] = df_bbox[['center_x', 'center_y']].apply(lambda df_bbox:
                                                                       utm.to_latlon(df_bbox['center_x'],
                                                                                     df_bbox['center_y'],
                                                                                     zone_number_init,
                                                                                     zone_letter_init), axis=1)

    # Return the final df format
    df_bbox['swne_edges'] = df_bbox['south_west_lat_long'] + df_bbox['north_east_lat_long']
    columns = ['row_num', 'col_num', 'swne_edges', 'center_latlon']
    return df_bbox[columns]

# Create inner bounding boxes for Dallas County
df_bbox = create_bbox(w=.1, lat_min=32.545214, lon_min=-97.038685, lat_max=32.989691, lon_max=-96.51687)

# Load amenity list
df_amenity = pd.read_csv('D:/Projects/EvictionStudy/amenity.csv')

non_existing_amenities = ['language_school', 'toy_library', 'training', 'boat_sharing', 'car_sharing',
                          'vehicle_inspection', 'ferry_terminal', 'grit_bin', 'motorcycle_parking',
                          'baby_hatch', 'brothel', 'love_hotel', 'stripclub', 'swingerclub', 'post_depot',
                          'ranger_station', 'dog_toilet', 'dressing_room', 'give_box', 'mailroom', 'parcel_locker',
                          'shower', 'telephone', 'watering_place', 'sanitary_dump_station', 'animal_breeding',
                          'baking_oven', 'crematorium', 'dive_centre', 'funeral_hall', 'hunting_stand', 'internet_cafe',
                          'kitchen', 'kneipp_water_cure', 'lounger', 'monastery', 'place_of_mourning', 'public_bath',
                          'refugee_site']

needed_amenities = ['cafe', 'fast_food', 'restaurant', 'college', 'school', 'university', 'fuel', 'bank',
                    'theatre', 'fire_station', 'police', 'shelter', 'childcare', 'grave_yard', 'place_of_worship', 'fountain']

# Filter out the non-existing amenities
df_amenity = df_amenity[df_amenity['amenity'].isin(needed_amenities)]
        
# Count amenities in each bounding box with 2021 data
import requests
import json
from tqdm import tqdm

overpass_url = "http://overpass-api.de/api/interpreter"
amenities = df_amenity['amenity'].values
amenity_list = dict()

for idx, am in enumerate(amenities):
    print(f'{idx} / {len(df_amenity)}: {am}')
    amenity_list[am] = []

    for i in tqdm(range(len(df_bbox))):

        overpass_query = f"""
            [out:json][date:"2021-12-31T15:00:00Z"];
            nwr["amenity"='{am}']{df_bbox['swne_edges'][i]};
            out count;
            """

        try:
            response = requests.get(overpass_url,
                                    params={'data': overpass_query})
            data = response.json()
            amenity_list[am].append(data['elements'][0]['tags']['total'])

        except ValueError:
            amenity_list[am].append(np.nan)

        except:
            time.sleep(900)
            print('30min time break')

    temp_df = pd.DataFrame(amenity_list)
    temp_df.to_csv('D:/Projects/EvictionStudy/amenity_01bbox/df_amenity_2021_lab_01bbox_2.csv')



