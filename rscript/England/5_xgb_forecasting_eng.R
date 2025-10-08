##XGB model for eng data

#Load libraries

library(tidyverse)
library(xgboost)



#Load data

nurse_df <- read_rds("data/eng_workforce_master.rds")



##build XGBOOST model

#first we need to remove the target variable from the dataset. Here, our target variable is headcount

nurse_xgb <- nurse_df %>% 
  
  select(!c("hc_lag1", "hc_lag2", "hc_lag3", "hc_lag4", "org_name", "nhse_region_name", "ics_name"))



# remove target variable

nurse_xgb_hc_removed <- nurse_xgb %>% select(!hc)



##create target variable

target_variable <- nurse_xgb %>% ungroup() %>%
  
  select(hc)



##select only the numerical columns to make sure that our dataframe is all numeric

nurse_xgb_numeric <- nurse_xgb_hc_removed %>% select_if(is.numeric)



#convert categorical variables to numerical variables

org <- model.matrix(~ org_code -1, nurse_xgb_hc_removed) ## # convert org code into one-hot encoding

region <- model.matrix(~ nhse_region_code -1, nurse_xgb_hc_removed) ## # convert region code into one-hot encoding

ics <- model.matrix(~ ics_code -1, nurse_xgb_hc_removed) ## # convert ics code into one-hot encoding



#prepare one dataframe and convert it to matrix

nurse_xgb <- cbind(nurse_xgb, org, ics, region) ## for ml models

write.csv(nurse_xgb %>% select(!date), "data/nurse_eng_ml.csv")

nurse_xgb_numeric <- cbind(nurse_xgb_numeric, org, ics, region)

nurse_xgb_matrix <- data.matrix(nurse_xgb_numeric)



##Split dataset into testing and training subsets

#here i assume training set is my avaiable dataset (2018 Mar - 2022 Dec)

numberOfTrainingSamples <- 2430



# training data
train_data <- nurse_xgb_matrix[1:numberOfTrainingSamples,]
train_labels <- target_variable$hc[1:numberOfTrainingSamples]



# testing data
test_data <- nurse_xgb_matrix[-(1:numberOfTrainingSamples),]
test_labels <- target_variable$hc[-(1:numberOfTrainingSamples)]



# Convert the cleaned dataframe to a dmatrix

# put our testing & training data into two seperates Dmatrixs objects

dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)



# train a model using our training data

xgb_model <- xgboost(data = dtrain, # the data   
                 max.depth = 15,
                 nround = 50, # max number of boosting iterations
                 early_stopping_rounds = 10) # if we dont see an improvement in this many rounds, stop



# generate predictions for our held-out testing data

xgb_pred <- predict(xgb_model, dtest)



#prepare prediction tibble

xgb_accuracy <- tibble(actual = test_labels,
                       pred = xgb_pred)

nurses_xgb_test <- subset(nurse_xgb_hc_removed[(numberOfTrainingSamples + 1) : nrow(nurse_xgb_hc_removed),])

nurses_xgb_test <- cbind(nurses_xgb_test, xgb_accuracy)



#ploting actual vs predicted

nurses_xgb_test <- nurses_xgb_test %>% as_tsibble(index = date, key = c("org_code",
                                                            "nhse_region_code",
                                                            "ics_code")) 


nurses_xgb_test %>% 
  
  group_by(org_code) %>% ggplot(aes(x = date)) +
  
  geom_line(aes(y = actual), colour = "black") +
  
  geom_line(aes(y = pred), colour = "red") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



# get information on how important each feature is

importance_matrix <- xgb.importance(names(nurse_xgb_matrix), model = xgb_model)



# and plot it!
xgb.plot.importance(importance_matrix)
  