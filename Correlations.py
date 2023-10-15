
# coding: utf-8

# # library



import pandas as pd
import seaborn as sns
import numpy as np
import math
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.cross_validation import train_test_split


# # import data



pubg_train = pd.read_csv('/Users/wells_wang/Desktop/UBCO-courses/Data501-data analytics/Final_Project/data/train_V2.csv')
pubg_train.head()


# # clean the data

# ## delete nan



pubg_train.dropna(how='any',inplace=True)
nan_rows = pubg_train[pubg_train['winPlacePerc'].isnull()]
print(nan_rows)


# ## checking for type of data
# object means the variable is categorical data; int means integer; float means decimal number



print(pubg_train.dtypes)


# ## remove variables
# Drop the Id, groupId and matchId columns, which is meanless in our model


pubg_train = pubg_train.drop(['Id','groupId','matchId'], axis = 1)
pubg_train.head()


# ## convert data type

# ### change "matchType" into nominal variables



sns.countplot(y = 'matchType',data = pubg_train)




matchType_class = pubg_train['matchType'].values
winpp = []
for i in range(len(matchType_class)):
    if matchType_class[i] == 'squad-fpp':
        winpp.append(0)
    if matchType_class[i] == 'duo':
        winpp.append(1)
    if matchType_class[i] == 'solo-fpp':
        winpp.append(2)
    if matchType_class[i] == 'squad':
        winpp.append(3)
    if matchType_class[i] == 'duo-fpp':
        winpp.append(4)
    if matchType_class[i] == 'solo':
        winpp.append(5)  
    if matchType_class[i] == 'normal-squad-fpp':
        winpp.append(6)
    if matchType_class[i] == 'crashfpp':
        winpp.append(7)
    if matchType_class[i] == 'flaretpp':
        winpp.append(8)
    if matchType_class[i] == 'normal-solo-fpp':
        winpp.append(9)
    if matchType_class[i] == 'flarefpp':
        winpp.append(10)
    if matchType_class[i] == 'normal-duo-fpp':
        winpp.append(11)
    if matchType_class[i] == 'normal-duo':
        winpp.append(12)
    if matchType_class[i] == 'normal-squad':
        winpp.append(13)
    if matchType_class[i] == 'crashtpp':
        winpp.append(14)
    if matchType_class[i] == 'normal-solo':
        winpp.append(15)
pubg_train['matchType_class'] = pd.Series(winpp)
pubg_train.head()




pubg_train.reset_index(drop=True, inplace=True)




lis = []
print('The length of matchType_class',len(pubg_train))
for i in range(len(pubg_train)):
    if pubg_train['matchType_class'][i] >= 6 and pubg_train['matchType_class'][i] <= 15:
        lis.append(i)
print('The length of matchType_class to be dropped',len(lis))        




pubg_train = pubg_train.drop(lis, axis = 0)
pubg_train.reset_index(drop=True, inplace=True)
pubg_train.head()



print(pubg_train.dtypes)



pubg_train.dropna(how='any',inplace=True)
nan_rows = pubg_train[pubg_train['winPlacePerc'].isnull()]
print(nan_rows)


# # correlation


# correlations between all variables
pubg_train.corr()

# corelations between winPlacePerc
pubg_train.corr()['winPlacePerc'].sort_values(ascending = False)


# correlation matrix
cols = pubg_train.corr().nlargest(7, 'winPlacePerc')['winPlacePerc'].index
data = pubg_train[cols]

corr = data.corr()
fig = plt.figure()
ax = fig.add_subplot(111)
cax = ax.matshow(corr,cmap='coolwarm', vmin=-1, vmax=1)
fig.colorbar(cax)
ticks = np.arange(0,len(data.columns),1)
ax.set_xticks(ticks)
plt.xticks(rotation=90)
ax.set_yticks(ticks)
ax.set_xticklabels(data.columns)
ax.set_yticklabels(data.columns)
plt.show()

