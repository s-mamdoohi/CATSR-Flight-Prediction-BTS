#This is the FIRST script to run
#import proper libraries
import pandas as pd
import zipfile
import os

#opens zip
zf = zipfile.ZipFile('Datasets 690.zip')
df = {}
i = 1
#adds all the different datasets from 2018 to 2020 into one csv file
for year in range(2018,2020):
    for month in range(1,13):
        df[i] = pd.read_csv(zf.open('On_Time_Marketing_Carrier_On_Time_Performance_(Beginning_January_2018)_%s_%s.csv'%(year,month)))
        i += 1
frames = [df[i] for i in range(1,25)]
df = pd.concat(frames,ignore_index=True)
#dataframe into a csv
df.to_csv('data.csv')
