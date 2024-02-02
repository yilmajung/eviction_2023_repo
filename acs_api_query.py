# Import requisite packages
import numpy as np
import pandas as pd
import requests

# Create a function for querying data from Census ACS API
def query_census_api_at_block_group(api_key, year, var_dict):
    '''
    Create a dataframe with Census Block Group level data for a given year and variable dictionary.
    api_key = Census API key
    year = Census year
    var_dict = Dictionary of variables to query from Census API
    '''
    
    # Create a basic data frame with Census Block Group level info
    url = f'https://api.census.gov/data/{year}/acs/acs5?get=NAME,B01001_001E&for=block%20group:*&in=state:48%20county:113&key={api_key}'
    response = requests.request("GET", url)
    df = pd.DataFrame(response.json()[1:], columns=response.json()[0])[['NAME','state','county','tract','block group']]
    
    # Query Census Block Group Level Dataset
    for key, item in var_dict.items():
        url = f'https://api.census.gov/data/{year}/acs/acs5?get=NAME,{key}&for=block%20group:*&in=state:48%20county:113&key={api_key}'
        response = requests.request("GET", url)
        df[item] = pd.DataFrame(response.json()[1:], columns=response.json()[0])[key]

    return df    

# Set census API key, year, and variable dictionary
census_api_key = 'b32d020a89fe8d18a9aad261a0b94e3725748774'
year = '2021'
var_dict = {
    # Demographic variables
    'B01001_001E':'total_population',
    'B01001_026E':'female_population',
    'B01002_001E':'median_age',
    'B02001_002E':'race_white',
    'B02001_003E':'race_black',
    'B02001_004E':'race_amerindian',
    'B02001_005E':'race_asian',
    'B03002_012E':'race_hispanic',
    'B09002_001E':'children_total',
    'B09002_002E':'children_w_married_couple',
    'B09002_009E':'children_w_male_hh',
    'B09002_015E':'children_w_female_hh',
    'B11001_001E':'hh_total',
    'B11001_007E':'hh_nonfamily',
    'B11001_008E':'hh_living_alone',
    'B15003_002E':'edu_att_over25_no_schooling_completed',
    'B15003_017E':'edu_att_over25_highschool_graduate',
    'B15003_021E':'edu_att_over25_associate_degree',
    'B15003_022E':'edu_att_over25_bachelor_degree',
    'B15003_023E':'edu_att_over25_master_degree',
    'B15003_024E':'edu_att_over25_professional_degree',
    'B15003_025E':'edu_att_over25_doctorate_degree',    
    
    # Built environment variables
    'B08303_001E':'time_to_work_total',
    'B08303_002E':'time_to_work_lt5',
    'B08303_003E':'time_to_work_5to9',
    'B08303_004E':'time_to_work_10to14',
    'B08303_005E':'time_to_work_15to19',
    'B08303_006E':'time_to_work_20to24',
    'B08303_007E':'time_to_work_25to29',
    'B08303_008E':'time_to_work_30to34',
    'B08303_009E':'time_to_work_35to39',
    'B08303_010E':'time_to_work_40to44',
    'B08303_011E':'time_to_work_45to59',
    'B08303_012E':'time_to_work_60to89',
    'B08303_013E':'time_to_work_mt90',
    'B28002_013E':'no_internet_access',
    
    # Housing variables"medrent_samples"
    'B25001_001E':'housing_unit',
    'B25002_003E':'housing_vacant',
    'B25003_002E':'tenure_owner_occupied',
    'B25003_003E':'tenure_renter_occupied',
    'B25004_002E':'vacancy_for_rent',
    'B25010_003E':'hh_average_size_renter_occupied',
    'B25024_002E':'1unit_detached_structure',
    'B25024_003E':'1unit_attached_structure',
    'B25024_008E':'units_in_structure_20_49',
    'B25024_009E':'units_in_structure_50ormore',
    'B25035_001E':'median_year_built',
    'B25070_001E':'gross_rent_percent_hhincome_total',
    'B25070_002E':'gross_rent_percent_hhincome_lt10',
    'B25070_003E':'gross_rent_percent_hhincome_10to15',
    'B25070_004E':'gross_rent_percent_hhincome_15to20',
    'B25070_005E':'gross_rent_percent_hhincome_20to25',
    'B25070_006E':'gross_rent_percent_hhincome_25to29',
    'B25070_007E':'gross_rent_percent_hhincome_30to35',
    'B25070_008E':'gross_rent_percent_hhincome_35to39',
    'B25070_009E':'gross_rent_percent_hhincome_40to50',
    'B25070_010E':'gross_rent_percent_hhincome_50ormore',
    'B25070_011E':'gross_rent_percent_hhincome_notcomputed',
    'B25071_001E':'gross_rent_percent_hhincome_median',
    'B25077_001E':'housing_median_value',
    'B25081_001E':'mortgage_status_total',
    'B25081_002E':'mortgage_status_w_mortgage',
    'B25081_009E':'mortgage_status_wo_mortgage',
    'B25088_001E':'median_monthly_owner_costs',
    'B25088_002E':'median_monthly_owner_costs_w_mortgage',
    'B25088_003E':'median_monthly_owner_costs_wo_mortgage',

    # Economic variables
    'B23025_002E':'ind_labor_force',
    'B23025_005E':'ind_labor_force_civilian_unemployed',
    'B17021_002E':'ind_poverty',
    'B19013_001E':'hh_median_income',
    'B19056_002E':'hh_w_ssi', #number of households with supplemental security income (SSI)
    'B19057_002E':'hh_w_pai', #number of households with public assistance income (PAI)
    'B19058_002E':'hh_w_foodstamp_SNAP', #number of households with PAI and Food Stamps / SNAP
    'B25058_001E':'median_contract_rent',
    'B25064_001E':'median_gross_rent', # contract rent plus monthly cost of utilities
}

df = query_census_api_at_block_group(census_api_key, year, var_dict)

# Save dataframe as csv
df.to_csv('data/acs_data_5y_2021_revised.csv', index=False)


# Query the change in median gross rent between 2016 and 2021
# Set census API key, year, and variable dictionary
year_2016 = '2016'
var_dict_2016 = {'B25064_001E':'median_gross_rent',
                 'B25077_001E':'housing_median_value'}
df_2016 = query_census_api_at_block_group(census_api_key, year_2016, var_dict_2016)
df_2016.to_csv('data/acs_data_5y_2016.csv', index=False)