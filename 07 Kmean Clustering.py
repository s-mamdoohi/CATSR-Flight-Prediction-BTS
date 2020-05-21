#This file is to cluster based on the end of the day delays
#import libraries
from __future__ import absolute_import, division, print_function, unicode_literals
import tensorflow as tf
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
from sklearn import cluster, datasets
#%matplotlib qt
#%matplotlib inline

# load data
data = pd.read_csv('final_data.csv')

# total delay for all airlines
data1 = data.groupby(['ACT_ARR_DATE','ACT_ARR_HOUR','Weekday'])['Arr_Delay'].sum().unstack(1,0).stack().reset_index(name='Arr_Delay')
data2 = data.groupby(['ACT_ARR_DATE','ACT_ARR_HOUR','Weekday'])['Dep_Delay'].sum().unstack(1,0).stack().reset_index(name='Dep_Delay')
df = pd.merge(data1,data2,on=['ACT_ARR_DATE','ACT_ARR_HOUR','Weekday'])
df = df[df['ACT_ARR_HOUR'] == 23]
df = df['Arr_Delay']
X = pd.Series.to_numpy(df)
df = pd.DataFrame(df)

# select an airline (for example: AA)
df = data[data['Marketing_Airline_Network'] == 'WN']
df = df[df['ACT_ARR_HOUR'] == 23]
df = df['Arr_Delay']
X = pd.Series.to_numpy(df)
df = pd.DataFrame(df)


res = []
for k in range(4,5):
    k_means = cluster.KMeans(n_clusters=k)
    k_means.fit(df)
    labels = k_means.predict(df)
    res.append(k_means.inertia_)

plt.plot(res)
plt.xticks(range(0,9),range(1,10))
plt.xlabel('Number of Clusters (K)')
plt.ylabel('Sum of squared distances\n of samples to their\n closest cluster center')

#scatterplot
plt.scatter(X,np.zeros_like(X)-0.0000001,marker='.',c=labels.astype(np.float))
plt.xlabel('Cumulative End of the Day Delay (min)')
plt.yticks(range(0,0),range(0,0))


#histogram
plt.hist(X,density=True,bins=20)
plt.xlabel('Cumulative End of the Day Delay (min)')
plt.ylabel('density')
plt.yticks([],[])
