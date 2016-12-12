#Initialization
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from keras.models import Sequential
from keras.layers.core import Dense
from keras.layers.core import Activation
from keras.layers.core import Dropout
from keras.layers.recurrent import LSTM
from keras.preprocessing import sequence
from keras.layers.embeddings import Embedding
import os
from keras.callbacks import ModelCheckpoint, EarlyStopping
from keras.models import model_from_json
from itertools import chain
from keras.optimizers import SGD
import theano
theano.config.openmp = True
OMP_NUM_THREADS=12

#Change working directory
os.chdir('D:\Gili\LSTM Python')

#Functions
def _load_data(data, n_prev = 100):
    """
    data should be pd.DataFrame()
    """
    docX, docY = [], []
    for i in range(len(data)-n_prev):
        docX.append(data.iloc[i:i+n_prev,range(np.shape(data)[1]-1)].as_matrix())
        docY.append(data.iloc[i+n_prev,np.shape(data)[1]-1])
    alsX = np.array(docX)
    alsY = np.array(docY)
    return alsX, alsY

def train_test_split(df,test_size=0.1,n_prev=100):
    """
    This just splits data to training and testing parts
    """
    ntrn = round(len(df) * (1 - test_size))
    X_train, y_train = _load_data(df.iloc[0:ntrn],n_prev)
    X_test, y_test = _load_data(df.iloc[ntrn:],n_prev)
    return (X_train, y_train), (X_test, y_test)

#Read Data
dataset = pd.DataFrame.from_csv("D:\Gili\DataSet7\data_15_03_16_extended_no_rise_python.csv")
validset = pd.DataFrame.from_csv("D:\Gili\DataSet7\data_15_03_16_extended_validation_python.csv")
patinet_list = sorted(np.unique(dataset['Patient_ID']))
concatenated = range(2,11)+range(18,24)+range(26,83) #list of all column indexes to take to train
#concatenated = range(2,3)+range(18,19)+range(30,34)+range(59,62)+range(26,28) #list of all HR columns stuff indexes to take to train
#concatenated = range(26,28) #list of HR column indexes to take to train
#concatenated = range(5,7)+range(26,29) #basic 4 columns :HR, VASO, ACC, SKIN.RES
#concatenated = (2  ,3  ,4  ,5 ,18 ,19 ,26 ,27 ,33 ,37 ,45 ,51 ,52 ,58 ,59 ,61 ,63 ,64 ,66 ,67 ,68 ,69 ,70 ,80) #selected best 24 features from xgboost
#concatenated = (2  ,4,18 ,19 ,26 ,61,64,70) #selected best 7 features from xgboost

#A model for all patinets with loo night
ID_list = sorted(np.unique(dataset['ID']))

#Model Settings
in_neurons = len(concatenated)-1
out_neurons = 1 
hidden_neurons = 10 #should be low as possible to deny OF
n_prev = 5 #memory (max is ID 10 with only 118 samples - should be 128 as well)
epochs= 100 #max epochs
mini_batches = 1 #sets the time for each epoch, smaller means longer epoch
valid_patience = 5 #early stopping, if used
dropout = 1 #for regularization (1 - no droput)

#Optimizer (SGD) - not using
sgd = SGD(lr=0.1, decay=1e-6, momentum=0.9, nesterov=True)
    
#Make the dataset with LOO
for id_loo in ID_list:
    dataset_loo = dataset[dataset.ID != id_loo]  #everything but that night  
    ID_list_loo = sorted(np.unique(dataset_loo['ID'])) #those are the nights to do LOO

    #Build data for all nigths but one
    for id_tmp in ID_list_loo:
        data = dataset[dataset.ID == id_tmp]  #the data of a night
        data = data.iloc[:,concatenated]
        cols = list(data) # get a list of columns
        cols.insert(len(concatenated),cols.pop(cols.index('Hypo'))) # move the column to head of list using index, pop and insert
        data = data.ix[:, cols] # use ix to reorder
        (X_train_tmp, y_train_tmp), (X_test_tmp, y_test_tmp) = train_test_split(data,test_size=0,n_prev=n_prev)  # retrieve data for LSTM
        if  id_tmp==ID_list_loo[0]:
            X_train = X_train_tmp
            y_train = y_train_tmp
        else:
            X_train = np.concatenate((X_train,X_train_tmp),axis=0)
            y_train = np.concatenate((y_train,y_train_tmp),axis=0)
    max_features = len(X_train)
    
    #Build data for the night out (validation)
    data = validset[validset.ID == id_loo]  #the data of a night
    data = data.iloc[:,concatenated]
    cols = list(data) # get a list of columns
    cols.insert(len(concatenated),cols.pop(cols.index('Hypo'))) # move the column to head of list using index, pop and insert
    data = data.ix[:, cols] # use ix to reorder
    (X_test, y_test), (X_test_tmp, y_test_tmp) = train_test_split(data,test_size=0,n_prev=n_prev)  # retrieve data for LSTM
     
    #Initialize & Build Model
    model = Sequential()
    #model.add(Embedding(max_features, hidden_neurons, input_shape=(in_neurons,n_prev)))
    model.add(LSTM(output_dim=hidden_neurons,input_shape=(n_prev,in_neurons),return_sequences=True,activation='sigmoid', inner_activation='hard_sigmoid'))
    model.add(LSTM(output_dim=hidden_neurons,return_sequences=False,activation='sigmoid', inner_activation='hard_sigmoid'))
    #model.add(LSTM(output_dim=hidden_neurons,return_sequences=False,activation='sigmoid', inner_activation='hard_sigmoid'))    
    #model.add(LSTM(output_dim=hidden_neurons,input_dim=in_neurons, return_sequences=False))
    #model.add(LSTM(output_dim=hidden_neurons,activation='sigmoid', inner_activation='hard_sigmoid'))
    #model.add(LSTM(hidden_neurons, return_sequences=False))
    #model.add(Dense(output_dim=hidden_neurons,input_dim=hidden_neurons,activation='relu', init='uniform'))
    #model.add(Dropout(dropout))
    #model.add(Dense(hidden_neurons, activation='relu'))
    model.add(Dense(output_dim=1,activation='sigmoid')) #no softmax!!!
    
    model.compile(loss="binary_crossentropy", optimizer="adam",class_mode='binary') #rmsprop
    
    #Or.. load existing model
    #model = model_from_json(open('architecture.json').read())
    #model.load_weights('weights.h5')

    #Create callbacks
    checkpointer = ModelCheckpoint(filepath="weights_%d.hdf5"  % id_loo, verbose=1)
    earlyStopping = EarlyStopping(monitor='val_loss', patience=valid_patience, verbose=1, mode='auto')

    #train model                              
    #hist = model.fit(X_train, y_train, batch_size=mini_batches, nb_epoch=epochs,verbose=1,show_accuracy=True,shuffle=True, callbacks=[earlyStopping],validation_data=(X_test,y_test))
    hist = model.fit(X_train, y_train, batch_size=mini_batches, nb_epoch=epochs,verbose=1,show_accuracy=True,shuffle=True,validation_data=(X_test,y_test)) #w/o callbacks faster to convergence
  
    #predicted_fit = model.predict_proba(X_train) #fit results
    predicted_test = model.predict_proba(X_test) #Test on the night out
    
    #plot results
    print('Ploting Results')
    #plt.subplot(2, 1, 1)
    #plt.ylim([-0.1,1.1])
    #plt.plot(y_train)
    #plt.plot(predicted_fit,'r')
    #plt.title('Fitting')
    #plt.subplot(2, 1, 2)    
    plt.ylim([-0.1,1.1])    
    plt.plot(y_test)
    plt.plot(predicted_test,'r')
    plt.title('Night: %d' % id_loo)
    plt.savefig('%d.png' % id_loo)
    plt.show()
    
    #Prediction concatenate
    if  id_loo==2:
        predictions_all = predicted_test
    else:
        predictions_all = np.concatenate((predictions_all,predicted_test),axis=0)
 
    #save model by id_loo name
    json_string = model.to_json()
    open('architecture_%d.json'  % id_loo, 'w').write(json_string)    
    model.save_weights('weights_%d.hdf5'  % id_loo,overwrite=True)
                
    # and save it as csv
    pd.DataFrame(predicted_test).to_csv("predictions_%d.csv" % id_loo)
    #pd.DataFrame(predicted_fit).to_csv("predicted_fit.csv")
    pd.DataFrame(predictions_all).to_csv("predictions_all.csv")