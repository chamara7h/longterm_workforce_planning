#ML models for ENG  



#Load libraries 

import pandas as pd
import numpy as np 

import matplotlib as inline
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib import rcParams

from sklearn.metrics import mean_squared_log_error
from sklearn.ensemble import RandomForestRegressor
from lightgbm import LGBMRegressor
import lightgbm as lgb
from lightgbm.sklearn import LGBMClassifier
from sklearn.linear_model import LinearRegression
from sklearn import tree
from sklearn.model_selection import train_test_split, cross_val_score, cross_val_predict
from xgboost import XGBRegressor
from sklearn import metrics
from sklearn.metrics import  r2_score

from  datetime import datetime, timedelta
import gc



## load data and remove unneccessary columns

nurse_df = pd.read_csv("data/nurse_eng_ml.csv")

nurse_tidy = nurse_df.drop(['Unnamed: 0', "org_code", "nhse_region_code", "ics_code"], axis = 1)

nurse_tidy.head()



## correlation plot

plt.figure(figsize=(15, 10))

heatmap = sns.heatmap(nurse_tidy.corr(), vmin=-1, vmax=1, annot=True, cmap="Blues", annot_kws = {"fontsize":10})

heatmap.set_title('Correlation Matrix for Features', fontdict={'fontsize':12}, pad=12)



## #Train and Test data set

# I use 2018 Jun - 2022 Dec as the training set

train = nurse_tidy[nurse_tidy['year'] <= 2022]

val = nurse_tidy[nurse_tidy['year'] > 2022]

pred = val


## drop forecasting variable

X_train = train.drop(['hc'], axis = 1)
y_train = train["hc"]

X_test = val.drop(['hc'], axis = 1)
y_test = val["hc"]


######################## ML Modelling ####################################


#LINEAR REGRESSION

lm = LinearRegression()

model = lm.fit(X_train, y_train)

cross_val_score(model, X_train, y_train, cv = 3, scoring = "r2").mean()

lm_pred = lm.predict(X_test) ## prediction

lm_pr = pd.DataFrame({'LM_PRED': lm_pred}, index = y_test.index) ## creating a dataframe

pred['LM_PR'] = lm_pr['LM_PRED'] ## combine LM prediction to original dataframe



#XGBoost

xgb_model = XGBRegressor().fit(X_train, y_train)

xgb_pred = xgb_model.predict(X_test)

xgb_pr = pd.DataFrame({'XGB_PRED': xgb_pred}, index = y_test.index)

pred['XGB_PR'] = xgb_pr['XGB_PRED']



#Light GBM

lgbm = LGBMClassifier()

lgbm_model = lgbm.fit(X_train, y_train)

lgbm_pred = lgbm_model.predict(X_test)

lgbm_pred = lgbm_model.predict(X_test)

lgbm_pr = pd.DataFrame({'LGBM_PRED': lgbm_pred}, index = y_test.index)

pred['LGBM_PR'] = lgbm_pr['LGBM_PRED']



#Random forest

clf = RandomForestRegressor(n_estimators=100, max_samples = 0.4)

rf_model = clf.fit(X_train, y_train)

rf_pred = rf_model.predict(X_test)

rf_pr = pd.DataFrame({'RF_PRED': rf_pred}, index = y_test.index)

pred['RF_PR'] = rf_pr['RF_PRED']



#Features Importance

sns.set_style("whitegrid")
plt.style.use("fivethirtyeight")

def feature_imp(df, model):
    fi = pd.DataFrame()
    fi["feature"] = df.columns
    fi["importance"] = model.feature_importances_
    return fi.sort_values(by="importance", ascending=False)



## save forecasted values

pred.to_csv("D:\\SSRM_Dissertation\\SSRM\\ML_pred_eng.csv", encoding='utf-8', header='true')
