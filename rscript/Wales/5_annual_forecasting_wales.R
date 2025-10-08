##multivariate methods

#Load libraries

library(fpp3)
library(xgboost)


#Load data

nurse_df <- read_rds("data/wales_workforce_tidy.rds")


#create the tisbble

nurse_tsibble <- nurse_df %>% 
  
  select(!c("local_health_board_code", "local_health_board_name")) %>%
  
  select(date, alt_code, headcount, everything()) %>%
  
  as_tsibble(key = c("alt_code", "grade", grade_code, grade_hierarchy), index = date) 


##create scatterplot matrix with target variable and predictors to see the correlation

nurse_tsibble %>% GGally::ggpairs(columns = 3:14) ##with patient data

nurse_tsibble %>% GGally::ggpairs(columns = c(3, 18, 29:31)) ##nurse related data

nurse_tsibble %>% GGally::ggpairs(columns = c(3, 19:22)) ##population data

nurse_tsibble %>% GGally::ggpairs(columns = c(3, 23:28)) ##with expenditure data


##fitting regression model

nurse_reduced <- nurse_tsibble %>%
  
                 select(c("date",
                          "alt_code" ,
                          "headcount",
                          "number_of_referrals",
                          "number_of_lpmhss_assessments",
                          "number_of_therapeutic_assessments",
                          "aged_0_to_15",
                          "aged_16_to_24",
                          "aged_25_to_44",
                          "aged_45_to_64",
                          "expenditure_000_prim_child_adolescent_mental_health_services",
                          "expenditure_000_prim_general_mental_illness",
                          "SFTN",
                          "IFTN"))

##create scatterplot matrix with target variable and selected predictors to see the correlation

nurse_reduced %>% GGally::ggpairs(columns = 3:14) ##with patient data    


##build XGBOOST model

#first we need to remove the target variable from the dataset. Here, our target variable is headcount also add month feature.

nurse_xgb_hc_removed <- nurse_reduced %>% filter_index(. ~ "2021 Dec") %>%
  
  mutate(month = month(date)) %>%
  
  arrange(date) %>%
  
  select(!c("headcount", "date"))


##create target variable

target_variable <- nurse_reduced %>% 
  
  filter_index(. ~ "2021 Dec") %>%
  
  arrange(date) %>%
  
  select(headcount)


##select only the numerical columns to make sure that our dataframe is all numeric

nurse_xgb_numeric <- as_tibble(nurse_xgb_hc_removed) %>% select_if(is.numeric)                                      


#convert categorical variables to numerical variables

alt <- model.matrix(~ alt_code -1, nurse_xgb_hc_removed) ## # convert alt code into one-hot encoding  

grade_code <- model.matrix(~ grade_code -1, nurse_xgb_hc_removed) ## # convert grade code into one-hot encoding 


#prepare one dataframe and convert it to matrix

nurse_xgb_numeric <- cbind(nurse_xgb_numeric, alt, grade_code)

nurse_xgb_matrix <- data.matrix(nurse_xgb_numeric)


##Split dataset into testing and training subsets

#For now, I'm going to use 80% of our data for training and the other 20% for testing.

# get the numb 80/20 training test split

numberOfTrainingSamples <- round((nrow(target_variable) * .8))

# training data
train_data <- nurse_xgb_matrix[1:numberOfTrainingSamples,]
train_labels <- target_variable$headcount[1:numberOfTrainingSamples]

# testing data
test_data <- nurse_xgb_matrix[-(1:numberOfTrainingSamples),]
test_labels <- target_variable$headcount[-(1:numberOfTrainingSamples)]


# Convert the cleaned dataframe to a dmatrix

# put our testing & training data into two seperates Dmatrixs objects

dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data

xgb_model <- xgboost(data = dtrain, # the data   
                     max.depth = 15,
                     nround = 25, # max number of boosting iterations
                     early_stopping_rounds = 10) # if we dont see an improvement in this many rounds, stop


# generate predictions for our held-out testing data

xgb_pred <- predict(xgb_model, dtest)


#prepare prediction tibble

xgb_accuracy <- tibble(actual = test_labels,
                       pred = xgb_pred)

nurses_xgb_test <- subset(as_tibble(nurse_xgb_hc_removed)[(numberOfTrainingSamples + 1) : nrow(nurse_xgb_hc_removed),])

nurses_xgb_test <- cbind(nurses_xgb_test, xgb_accuracy)


#ploting actual vs predicted

nurses_xgb_test <- nurses_xgb_test %>% as_tsibble(index = date, key = c("alt_code", "grade_code"))

nurses_xgb_test %>% 
  
  group_by(alt_code) %>% 
  
  summarise(actual = sum(actual), pred = sum(pred)) %>%
  
  ggplot(aes(x = date)) +
  
  geom_line(aes(y = actual), colour = "black") +
  
  geom_line(aes(y = pred), colour = "red") +
  
  facet_wrap(vars(alt_code), scales = "free_y", ncol = 2)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# get information on how important each feature is
importance_matrix <- xgb.importance(names(nurse_xgb_matrix), model = xgb_model)

# and plot it!
xgb.plot.importance(importance_matrix)
  
 