#import proper libraries
import pandas as pd
import zipfile
import os


zf = zipfile.ZipFile('Datasets 690.zip')
df = {}
i = 1
for year in range(2018,2020):
    for month in range(1,13):
        df[i] = pd.read_csv(zf.open('On_Time_Marketing_Carrier_On_Time_Performance_(Beginning_January_2018)_%s_%s.csv'%(year,month)))
        i += 1
frames = [df[i] for i in range(1,25)]
df = pd.concat(frames,ignore_index=True)
df.to_csv('data.csv')
