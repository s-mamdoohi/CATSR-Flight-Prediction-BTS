#Similar to the SIXTH SCRIPT that ran
#This is for categorical
#import libraries
from __future__ import absolute_import, division, print_function, unicode_literals
import tensorflow as tf
from keras.utils.np_utils import to_categorical
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import holidays
import datetime as dt
#%matplotlib qt
#%matplotlib inline

# parameters of LSTM
TRAIN_SPLIT = 364*24#14016 #don't cahnge this number
BATCH_SIZE = 8 #256
BUFFER_SIZE = 10000
EVALUATION_INTERVAL = 364/BATCH_SIZE #200
EPOCHS = 70

# past history valid options: 24-future_target,2(24)-future_target,3(24)-future_target,...
past_history = 15

# future target valid options: 12,11,10,...
future_target = 24 - past_history
# note that 'past history' + 'future target' must be a multiple of 24, i.e. 24, 48, 72, ...

# this is not relevant in our problem, it is always 1
STEP = 1 

# load data
data = pd.read_csv('final_data.csv')


# total delay for all airlines
data1 = data.groupby(['ACT_ARR_DATE','ACT_ARR_HOUR','Weekday'])['Arr_Delay'].sum().unstack(1,0).stack().reset_index(name='Arr_Delay')
data2 = data.groupby(['ACT_ARR_DATE','ACT_ARR_HOUR','Weekday'])['Dep_Delay'].sum().unstack(1,0).stack().reset_index(name='Dep_Delay')
df = pd.merge(data1,data2,on=['ACT_ARR_DATE','ACT_ARR_HOUR','Weekday'])

df['season'] = pd.to_datetime(df['ACT_ARR_DATE']).dt.quarter
dates = df['ACT_ARR_DATE'].values
holiday = np.empty(dates.shape[0])
for i in range(0,dates.shape[0]):
    if dt.datetime.strptime(dates[i],'%Y-%m-%d') in holidays.US():
        holiday[i] = 1
    else:
        holiday[i] = 0
df['holiday'] = holiday

df = df[df['ACT_ARR_DATE'] != '2018-03-11']
df = df[df['ACT_ARR_DATE'] != '2019-03-10']


# select an airline (for example: AA)
#df = data[data['Marketing_Airline_Network'] == 'AA']


# don't cahnge the seed so that we can compare the results with each other
tf.random.set_seed(13)

#definition for creating time steps
def create_time_steps(length):
  return list(range(-length, 0))

#definition for displaying plot
def show_plot(plot_data, delta, title):
  labels = ['History', 'True Future', 'Model Prediction']
  marker = ['.-', 'rx', 'go']
  time_steps = create_time_steps(plot_data[0].shape[0])
  if delta:
    future = delta
  else:
    future = 0

  plt.title(title)
  for i, x in enumerate(plot_data):
    if i:
      plt.plot(future, plot_data[i], marker[i], markersize=10,
               label=labels[i])
    else:
      plt.plot(time_steps, plot_data[i].flatten(), marker[i], label=labels[i])
  plt.legend()
  plt.xlim([time_steps[0], (future+5)*2])
  plt.ylabel('Total Delay (min)')
  plt.xlabel('Time-Step')
  return plt

#definition for baseline
def baseline(history):
  return np.mean(history)


######## multivariate
features_considered = ['Arr_Delay','Dep_Delay','Weekday','season','holiday']
features = df[features_considered]
features.index = df[['ACT_ARR_DATE','ACT_ARR_HOUR']]
features.head()

dataset = features.values

#definition for multivariate data
def multivariate_data(dataset, target, start_index, end_index, history_size,
                      target_size, step, single_step=False):
    data = []
    labels = []

    start_index = start_index + history_size
    if end_index is None:
         end_index = len(dataset)
         print(end_index)

    for i in range(start_index, end_index, 24):
        indices = range(i-history_size, i, step)
        data.append(dataset[indices])

        if single_step:
            if target[i+target_size-1] < 337884.3:
                Type = 0
            elif (target[i+target_size-1] >= 337884.3 and target[i+target_size-1] < 543553):
                Type = 1
            elif (target[i+target_size-1] >= 543553):
                Type = 2

            labels.append(Type) #added -1
        else:
            labels.append(target[i:i+target_size])

    return np.array(data), np.array(labels)

x_train_single, y_train_single = multivariate_data(dataset, dataset[:, 0], 0,
                                                   TRAIN_SPLIT, past_history,
                                                   future_target, STEP,
                                                   single_step=True)
x_val_single, y_val_single = multivariate_data(dataset, dataset[:, 0],
                                               TRAIN_SPLIT, None, past_history,
                                               future_target, STEP,
                                               single_step=True)

data_mean = x_train_single.mean(axis=0)
data_std = dataset.std(axis=0)

for i in range(0,2):
    for j in range(0,x_train_single.shape[0]):
        x_train_single[j,:,i] = (x_train_single[j,:,i]-data_mean[:,i])/data_std[i]
    for j in range(0,x_val_single.shape[0]):
        x_val_single[j,:,i] = (x_val_single[j,:,i]-data_mean[:,i])/data_std[i]

y_train_single = to_categorical(y_train_single)
y_val_single = to_categorical(y_val_single)


print ('Single window of past history : {}'.format(x_train_single[0].shape))

train_data_single = tf.data.Dataset.from_tensor_slices((x_train_single, y_train_single))
train_data_single = train_data_single.cache().shuffle(BUFFER_SIZE).batch(BATCH_SIZE).repeat()

val_data_single = tf.data.Dataset.from_tensor_slices((x_val_single, y_val_single))
val_data_single = val_data_single.batch(BATCH_SIZE).repeat()

single_step_model = tf.keras.models.Sequential()
single_step_model.add(tf.keras.layers.LSTM(8,
                                           input_shape=x_train_single.shape[-2:]))
#single_step_model.add(tf.keras.layers.LSTM(8))
#single_step_model.add(tf.keras.layers.Dense(4))
single_step_model.add(tf.keras.layers.Dense(3, activation='softmax'))

single_step_model.compile(optimizer='rmsprop', loss='categorical_crossentropy',
                          metrics=['accuracy'])

for x, y in val_data_single.take(1):
  print(single_step_model.predict(x).shape)
  
single_step_history = single_step_model.fit(train_data_single, epochs=EPOCHS,
                                            steps_per_epoch=EVALUATION_INTERVAL,
                                            validation_data=val_data_single,
                                            validation_steps=364/BATCH_SIZE)
#definition for plotting train history
def plot_train_history(history, title):
  loss = history.history['loss']
  val_loss = history.history['val_loss']

  epochs = range(len(loss))

  plt.figure()

  plt.plot(epochs, loss, 'b', label='Training loss')
  plt.plot(epochs, val_loss, 'r', label='Validation loss')
  plt.xlabel('Epoch')
  plt.ylabel('Mean Absolute Error')
  plt.title(title)
  plt.legend()

  plt.show()

plot_train_history(single_step_history,
                   'Single Step Training and validation loss')

#confusion matrix
#end-of-the-day-cumulative-delay prediction accuracy
print(single_step_model.evaluate(x_val_single,y_val_single))

pred = np.argmax(single_step_model(x_val_single),axis=1)
act = np.argmax(y_val_single,axis=1)

nn,ns,na=0,0,0
sn,ss,sa=0,0,0
an,as2,aa=0,0,0
for i in range(0,pred.shape[0]):
    if act[i] == 0:
        if pred[i] == 0:
            nn += 1
        elif pred[i] == 1:
            ns += 1
        elif pred[i] == 2:
            na += 1
    elif act[i] == 1:
        if pred[i] == 0:
            sn += 1
        elif pred[i] == 1:
            ss += 1
        elif pred[i] == 2:
            sa += 1
    elif act[i] == 2:
        if pred[i] == 0:
            an += 1
        elif pred[i] == 1:
            as2 += 1
        elif pred[i] == 2:
            aa += 1

print(nn,ns,na)
print(sn,ss,sa)
print(an,as2,aa)
