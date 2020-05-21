#This script converts the time zone
#import libraries
import pandas as pd
import numpy as np
import datetime as dt
from geopy.geocoders import Nominatim
from geopy.extra.rate_limiter import RateLimiter
from timezonefinder import TimezoneFinder
import pytz
from tqdm import tqdm
import os


#grabs the time zone of origin and destination
def get_timezone_origin(city):
    geolocator = Nominatim(user_agent='CATSR',timeout=10)
    locfinder = RateLimiter(geolocator.geocode, min_delay_seconds=0)
    location = locfinder(city['OriginCityName'])
    #location = geolocator.geocode(city['CITY_NAME'])
    if location != None:
        tf = TimezoneFinder()
        time_zone = tf.timezone_at(lng=location.longitude, lat=location.latitude)
    else:
        time_zone = np.nan
    return time_zone

def get_timezone_dest(city):
    geolocator = Nominatim(user_agent='CATSR',timeout=10)
    locfinder = RateLimiter(geolocator.geocode, min_delay_seconds=0)
    location = locfinder(city['DestCityName'])
    #location = geolocator.geocode(city['CITY_NAME'])
    if location != None:
        tf = TimezoneFinder()
        time_zone = tf.timezone_at(lng=location.longitude, lat=location.latitude)
    else:
        time_zone = np.nan
    return time_zone

def time_change_crs_dep(data):
    #change time to standard format first
    hour = str(int(data['CRSDepTime']//100))
    if len(hour) < 2:
        hour = str(0) + hour
    mint = str(int(data['CRSDepTime']%100))
    if len(mint) < 2:
        mint = str(0) + mint
    naive_datetime = dt.datetime.strptime(data['FlightDate'] + ' ' + hour + mint, "%Y-%m-%d %H%M") #"%Y-%m-%d %H%M"
    #change the local times (CRS_DEP_TIME) to UTC
    local_time = pytz.timezone(data['ORIGIN_CITY_TIMEZONE'])
    local_datetime = local_time.localize(naive_datetime, is_dst=True)
    eastern = pytz.timezone('US/Eastern')
    eastern_datetime = local_datetime.astimezone(eastern)
    return eastern_datetime

def time_change_act_dep(data):
    #change time to standard format first
    hour = str(int(data['DepTime']//100))
    if len(hour) < 2:
        hour = str(0) + hour
    if hour == '24':
        hour = '23'
        mint = '59'
    else:
        mint = str(int(data['DepTime']%100))
        if len(mint) < 2:
            mint = str(0) + mint
    if (data['DepTime'] >= data['CRSDepTime'] and data['DepDelayMinutes'] <= 1440):
        naive_datetime = dt.datetime.strptime(data['FlightDate'] + ' ' + hour + mint, "%Y-%m-%d %H%M") #"%Y-%m-%d %H%M"
    elif (data['DepTime'] < data['CRSDepTime'] and data['DepDelayMinutes'] == 0):
        naive_datetime = dt.datetime.strptime(data['FlightDate'] + ' ' + hour + mint, "%Y-%m-%d %H%M")
    else:
        naive_datetime = dt.datetime.strptime(data['FlightDate'] + ' ' + hour + mint, "%Y-%m-%d %H%M") + dt.timedelta(days=1)
    #change the local times (CRS_DEP_TIME) to UTC
    local_time = pytz.timezone(data['ORIGIN_CITY_TIMEZONE'])
    local_datetime = local_time.localize(naive_datetime, is_dst=True)
    eastern = pytz.timezone('US/Eastern')
    eastern_datetime = local_datetime.astimezone(eastern)
    return eastern_datetime

def time_change_crs_arr(data):
    #change time to standard format first
    hour = str(int(data['CRSArrTime']//100))
    if len(hour) < 2:
        hour = str(0) + hour
    if hour == '24':
        hour = '23'
        mint = '59'
    else:
        mint = str(int(data['CRSArrTime']%100))
        if len(mint) < 2:
            mint = str(0) + mint
    naive_datetime = dt.datetime.strptime(data['FlightDate'] + ' ' + hour + mint, "%Y-%m-%d %H%M")
    #change the local times (CRS_DEP_TIME) to UTC
    local_time = pytz.timezone(data['DEST_CITY_TIMEZONE'])
    local_datetime = local_time.localize(naive_datetime, is_dst=True)
    eastern = pytz.timezone('US/Eastern')
    eastern_datetime = local_datetime.astimezone(eastern)
    if (eastern_datetime < data['CRS_DEP_DATETIME_EST']):
        eastern_datetime = eastern_datetime + dt.timedelta(days=1)
    return eastern_datetime

def time_change_act_arr(data):
    #change time to standard format first
    hour = str(int(data['ArrTime']//100))
    if len(hour) < 2:
        hour = str(0) + hour
    if hour == '24':
        hour = '23'
        mint = '59'
    else:
        mint = str(int(data['ArrTime']%100))
        if len(mint) < 2:
            mint = str(0) + mint
    naive_datetime = dt.datetime.strptime(data['FlightDate'] + ' ' + hour + mint, "%Y-%m-%d %H%M")
    #change the local times (CRS_DEP_TIME) to UTC
    local_time = pytz.timezone(data['DEST_CITY_TIMEZONE'])
    local_datetime = local_time.localize(naive_datetime, is_dst=True)
    eastern = pytz.timezone('US/Eastern')
    eastern_datetime = local_datetime.astimezone(eastern)
    if (eastern_datetime < data['CRS_ARR_DATETIME_EST'] and data['ArrDelayMinutes'] > 0):
        eastern_datetime = eastern_datetime + dt.timedelta(days=1)
    return eastern_datetime

# loading data
data = pd.read_csv('data.csv')

# cleaning data
data = data[(data['TaxiIn'] <= 200) & (data['TaxiOut'] <= 200) &
            ((data['WheelsOn'] - data['WheelsOff']) -
             (data['CRSArrTime'] - data['CRSDepTime'])<= 100)]

# remove nan values
data = data.dropna(subset=['DepDelayMinutes','ArrDelayMinutes','CRSDepTime','DepTime','CRSArrTime','ArrTime','FlightDate','OriginCityName'])

# changing local times
tqdm.pandas()
origin_city_timezone = pd.DataFrame(data['OriginCityName'].unique(),columns=['OriginCityName'])
origin_city_timezone['ORIGIN_CITY_TIMEZONE'] = origin_city_timezone.progress_apply(get_timezone_origin,axis=1)
dest_city_timezone = pd.DataFrame(data['DestCityName'].unique(),columns=['DestCityName'])
dest_city_timezone['DEST_CITY_TIMEZONE'] = dest_city_timezone.progress_apply(get_timezone_dest,axis=1)

# combine ORIGIN_CITY_TIMEZONE and DEST_CITY_TIMEZONE and data
data = pd.merge(data,origin_city_timezone, on = ['OriginCityName'], how='left')
data = pd.merge(data,dest_city_timezone, on = ['DestCityName'], how='left')

# drop na values in ORIGIN_CITY_NAME and DEST_CITY_TIMEZONE
data = data.dropna(subset=['ORIGIN_CITY_TIMEZONE','DEST_CITY_TIMEZONE'])

data['DepTime'] = data['DepTime'].astype('int64')

# chage data and time to Eastern Time
tqdm.pandas()
data['CRS_DEP_DATETIME_EST'] = data.progress_apply(time_change_crs_dep,axis=1)
data['ACT_DEP_DATETIME_EST'] = data.progress_apply(time_change_act_dep,axis=1)
data['CRS_ARR_DATETIME_EST'] = data.progress_apply(time_change_crs_arr,axis=1)
data['ACT_ARR_DATETIME_EST'] = data.progress_apply(time_change_act_arr,axis=1)



# EoDTD
# add the CRS departure time of the flights to the data (0: CRS_DEP_TIME is 00:00-00:59,..., 23:CRS_DEP_TIME is 23:00-23:59)
def get_hour_crs_dep(data):
    return data['CRS_DEP_DATETIME_EST'].hour

def get_hour_act_dep(data):
    return data['ACT_DEP_DATETIME_EST'].hour

def get_hour_crs_arr(data):
    return data['CRS_ARR_DATETIME_EST'].hour

def get_hour_act_arr(data):
    return data['ACT_ARR_DATETIME_EST'].hour

data['CRS_DEP_HOUR'] = data.progress_apply(get_hour_crs_dep,axis=1)
data['ACT_DEP_HOUR'] = data.progress_apply(get_hour_act_dep,axis=1)
data['CRS_ARR_HOUR'] = data.progress_apply(get_hour_crs_arr,axis=1)
data['ACT_ARR_HOUR'] = data.progress_apply(get_hour_act_arr,axis=1)

def get_date_crs_dep(data):
    return data['CRS_DEP_DATETIME_EST'].strftime("%Y-%m-%d")

def get_date_act_dep(data):
    return data['ACT_DEP_DATETIME_EST'].strftime("%Y-%m-%d")

def get_date_crs_arr(data):
    return data['CRS_ARR_DATETIME_EST'].strftime("%Y-%m-%d")

def get_date_act_arr(data):
    return data['ACT_ARR_DATETIME_EST'].strftime("%Y-%m-%d")

data['CRS_DEP_DATE'] = data.progress_apply(get_date_crs_dep,axis=1)
data['ACT_DEP_DATE'] = data.progress_apply(get_date_act_dep,axis=1)
data['CRS_ARR_DATE'] = data.progress_apply(get_date_crs_arr,axis=1)
data['ACT_ARR_DATE'] = data.progress_apply(get_date_act_arr,axis=1)

#select features
data = data[['CRS_DEP_DATETIME_EST','CRS_DEP_DATE','CRS_DEP_HOUR','ACT_DEP_DATETIME_EST','ACT_DEP_DATE','ACT_DEP_HOUR','CRS_ARR_DATETIME_EST','CRS_ARR_DATE','CRS_ARR_HOUR','ACT_ARR_DATETIME_EST','ACT_ARR_DATE','ACT_ARR_HOUR','Marketing_Airline_Network','Origin','OriginCityName','Dest','DestCityName','ArrDelayMinutes','DepDelayMinutes']]
data = data.dropna()
data.to_csv('data_cleaned.csv')

crs_dep = data[['CRS_DEP_DATETIME_EST','CRS_DEP_DATE','CRS_DEP_HOUR','Marketing_Airline_Network','Origin','OriginCityName','Dest','DestCityName','ArrDelayMinutes','DepDelayMinutes']]
act_dep = data[['ACT_DEP_DATETIME_EST','ACT_DEP_DATE','ACT_DEP_HOUR','Marketing_Airline_Network','Origin','OriginCityName','Dest','DestCityName','ArrDelayMinutes','DepDelayMinutes']]
crs_arr = data[['CRS_ARR_DATETIME_EST','CRS_ARR_DATE','CRS_ARR_HOUR','Marketing_Airline_Network','Origin','OriginCityName','Dest','DestCityName','ArrDelayMinutes','DepDelayMinutes']]
act_arr = data[['ACT_ARR_DATETIME_EST','ACT_ARR_DATE','ACT_ARR_HOUR','Marketing_Airline_Network','Origin','OriginCityName','Dest','DestCityName','ArrDelayMinutes','DepDelayMinutes']]

#makes csv of each feature
crs_dep.to_csv('crs_dep.csv')
act_dep.to_csv('act_dep.csv')
crs_arr.to_csv('crs_arr.csv')
act_arr.to_csv('act_arr.csv')
