 #!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Jan 31 12:39:54 2018

@author: bennyng_211
"""
import numpy as np
import pylab as pl
import pandas as pd
import torch
from torch.autograd import Variable
import torch.nn as nn
import torchvision.datasets as dsets
import torchvision.transforms as transforms
import torch.utils.data as data
import random
import time

def getData():
    # load data from csv files
    train = pl.loadtxt('/data_3class.csv')
    Xtrain = train[:400,0:2]
    Ytrain = train[:400,2:3]
    Xtest = train[400:,0:2]
    Ytest = train[400:,2:3]
    return Xtrain, Ytrain, Xtest,Ytest

df_1= []

with open('/cust_data.csv','rb') as f:
    for line in f:
        line = line.rstrip()
        df_1.append(line)
df_1 = [a.split("\r") for a in df_1]
df_1[0][1].split(",")
len(df_1[0])
df_2 = [a.split(",") for a in df_1[0]]
float(df_2[1][2])
np.array(int(df_2[1][0]))

t = np.array(df_2)
s = t[1:,3]
df_2[2][3]
df_3 = []
for i in range(len(df_2)-1):
#for i in range(3-1):
    x = []
    x.append(float(df_2[i+1][0]))
    x.append(float(df_2[i+1][1]))
    x.append(float(df_2[i+1][2]))
    x.append(float(df_2[i+1][4]))
    x.append(float(df_2[i+1][5]))
    x.append(float(df_2[i+1][6]))
    x.append(float(df_2[i+1][7]))
    x.append(float(df_2[i+1][9]))
    df_3.append(x)

df_3 = np.array(df_3)
    
X = df_3[:,0:7]
Y = df_3[:,-1]
Y = Y.reshape(-1,1)

def Oneshot(y):
    ynew = []
    #ynew = np.array(ynew)
    for i in range(len(y)):
        if y[i] == 0:
           ynew.append(np.identity(2)[0:1][0])
           #np.append(ynew,np.identity(3)[0:1])
            
        elif y[i] == 1:
            
            ynew.append(np.identity(2)[1:2][0])
            #np.append(ynew,np.identity(3)[1:2])
        else:
            ynew.append(np.identity(2)[2:3][0])
            #np.append(ynew,np.identity(3)[2:3])
    ynew = np.array(ynew)
    return ynew

Yt = Oneshot(Y)

def OneshotChan(y): 
    ynew = []
    #ynew = np.array(ynew)
    for i in range(len(y)):
        if y[i] == 'lmkebamcaaclubfxadlmueccxoimlema':
           ynew.append(np.identity(8)[0:1][0])
           #np.append(ynew,np.identity(3)[0:1])

        elif y[i] == 'foosdfpfkusacimwkcsosbicdxkicaua':
            ynew.append(np.identity(8)[1:2][0])
        elif y[i] == '':
            ynew.append(np.identity(8)[2:3][0])            
        elif y[i] == 'usilxuppasemubllopkaafesmlibmsdf':
            ynew.append(np.identity(8)[3:4][0])
        elif y[i] == 'ewpakwlliwisiwduibdlfmalxowmwpci':
            ynew.append(np.identity(8)[4:5][0])
        elif y[i] == 'epumfxlbckeskwekxbiuasklxalciiuu':
            ynew.append(np.identity(8)[5:6][0])
        elif y[i] == 'sddiedcslfslkckwlfkdpoeeailfpeds':
            ynew.append(np.identity(8)[6:7][0])
        elif y[i] == 'fixdbufsefwooaasfcxdxadsiekoceaa':
            ynew.append(np.identity(8)[7:8][0])
         
    ynew = np.array(ynew)
    return ynew

t[1:4,3]
s
Chan_oneshot = OneshotChan(s)

Xt = []
for i in range(len(Chan_oneshot)):
    Xt.append(np.append(X[i],Chan_oneshot[i]))

Xt = np.array(Xt)


def Splitdata(x,y,split):
    trainlist = random.sample(range(x.shape[0]), int(round(x.shape[0]*split)))
    vallist = list(set(range(x.shape[0]))- set(trainlist))
   
    trainX, valX = np.array([x[i] for i in trainlist]), np.array([x[i] for i in vallist])
    trainY, valY = np.array([y[i] for i in trainlist]), np.array([y[i] for i in vallist])#np.expand_dims(np.array([dataY[i] for i in trainlist]), axis = 1), np.expand_dims(np.array([dataY[i] for i in vallist]), axis = 1)
    return trainX, trainY, valX, valY


trainX, trainY, valX, valY = Splitdata(Xt,Yt,0.7)   

valX, valY, testX, testY = Splitdata(valX,valY,0.5)        

dtype = torch.FloatTensor
nn_input_dim = len(trainX[0])
nn_hidden_dim = 200
nn_output_dim = 2
batch_size = 3
max_epoch = 20

#x = Variable(torch.FloatTensor(trainX), requires_grad=False)
#y = Variable(torch.FloatTensor(trainY), requires_grad=False)

#w1 = Variable(torch.randn(nn_input_dim, nn_hidden_dim).type(dtype), requires_grad=True)
#w2 = Variable(torch.randn(nn_output_dim, nn_output_dim).type(dtype), requires_grad=True)

learning_rate = 0.001
class Net(nn.Module):
    def __init__(self,nn_input_dim,nn_hidden_dim,nn_output_dim):
        super(Net,self).__init__()
        self.fc1 = nn.Linear(nn_input_dim,nn_hidden_dim)
        self.relu = nn.ReLU()
        self.fc2 = nn.Linear(nn_hidden_dim,nn_output_dim)
        self.SM = nn.Softmax()
        
    def forward(self,x):
        out = self.fc1(x)
        out = self.relu(out)
        out = self.fc2(out)
        out = self.SM(out)
        return out

net = Net(nn_input_dim,nn_hidden_dim,nn_output_dim)

criterion = nn.L1Loss() #can try nn.CrossEntropyLoss()
optimizer = torch.optim.Adam(net.parameters(),lr = learning_rate)

trainTensor = data.TensorDataset(torch.FloatTensor(trainX), torch.FloatTensor(trainY))
trainLoader = data.DataLoader(trainTensor, batch_size = batch_size, shuffle = True) #, num_workers = 0, drop_last = True)
valTensor = data.TensorDataset(torch.FloatTensor(valX), torch.FloatTensor(valY))
valLoader = data.DataLoader(valTensor, batch_size = batch_size, shuffle = True) #, num_workers = 0, drop_last = True)
total_trainloss = []
total_valloss = []


"""Train Model"""
for epoch in range(max_epoch):
    losses = []
    vallosses = []
    starttime = time.time()
#    inp_grad = []
    
    for batch in trainLoader:
        net.train()
        net.zero_grad()
        x,y = Variable(batch[0].type(dtype), requires_grad=True), Variable(batch[1].type(dtype))
        
        optimizer.zero_grad()
        output = net(x)
        loss = criterion(output,y)
        loss.backward()
#        inp_grad.append(np.mean(x.grad.data.numpy(),0))
        optimizer.step()
        losses.append(loss.data[0])
    
    total_trainloss.append(np.mean(losses))
    print 'Epoch %i:\nAverage Training Loss: %f\nTime Taken: %f seconds'%(epoch, total_trainloss[-1], time.time()-starttime)
    
    
    Prediction = []
    Test = []
    for valbatch in valLoader:
        net.eval()
        x,y = Variable(valbatch[0].type(dtype), requires_grad=True), Variable(valbatch[1].type(dtype))
        output = net(x)
        valloss = criterion(output,y)
        vallosses.append(valloss.data[0])
#        print vallosses
        Prediction.append(output.data.numpy())
        Test.append(y.data.numpy())
        print output.data.numpy()
        print y.data.numpy()
        
len(Prediction)
len(Test)

correct = 0
total = 0
predright = []
predWrong = []
Actual = []
for i in range(len(Prediction)):
    for j in range(len(Prediction[0])):
        if np.argmax(Prediction[i][j]) == np.argmax(Test[i][j]):
            correct += 1
            total += 1
            predright.append(np.argmax(Prediction[i][j]))
        else:
            predWrong.append(np.argmax(Prediction[i][j]))
            Actual.append(np.argmax(Test[i][j]))
            total += 1
            
accuracy = float(correct)/total        
np.argmax(Prediction[1197][1])
np.argmax(Test[1197][0])


torch.max(output.data,1) == torch.max(y.data,1)

np.argmax(y.data.numpy()[0])





