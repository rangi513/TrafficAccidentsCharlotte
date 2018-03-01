##################################
## Hackathon - Traffic Datasets ##
##################################

## Library packages
library(tidyverse)
library(stringr)
library(dummies)
library(devtools) 
library(xgboost)
install_github("AppliedDataSciencePartners/xgboostExplainer")
library(xgboostExplainer)
library(caret)
library(lubridate)

## Read In Traffic Data
traffic_data <- read_csv('~/Downloads/dataset/Charlotte Traffic Incidents/traffic.csv')
three11_data <- read_csv('~/Downloads/RequestsforService.csv')

# Create Features from 311 Data
three11traffic_clean <- three11_data %>% 
  mutate(TITLE = str_to_lower(TITLE),
         potholes = ifelse(grepl('pothole', TITLE), 1, 0),
         road = ifelse(grepl('road', TITLE), 1, 0),
         bball = ifelse(grepl('basketball', TITLE), 1, 0),
         tree = ifelse(grepl('tree', TITLE), 1, 0), 
         rightOfWay = ifelse(grepl('right of way', TITLE), 1, 0),
         streetSign = ifelse(grepl('street sign', TITLE), 1, 0),
         traffic = ifelse(grepl('traffic', TITLE), 1, 0),
         streetLight = ifelse(grepl('streetlight', TITLE), 1, 0),
         Latitude = round(Latitude, 2),
         Longitude = round(Longitude, 2)) %>% 
  select(Latitude, Longitude, potholes, road, bball, tree, rightOfWay, streetSign, traffic, streetLight) %>% 
  group_by(Latitude, Longitude) %>% 
  summarise_all(funs(sum)) 

traffic_data_clean <- traffic_data %>% 
        filter(!is.na(CRSH_LEVL)) %>% 
        mutate(date_val = mdy_hm(DATE_VAL),
               crash = ifelse(CRSH_LEVL < 4, 1, 0),
               Latitude = round(LATITUDE, 2),
               Longitude = round(LONGITUDE, 2),
               ToD = gsub('.{2}$', '', MILT_TIME),
               ToD = ifelse(ToD == '', 0, ToD)) %>% 
        rename(DoW = DAY_OF_WEEK_DESC) %>% 
        select(Latitude, Longitude, ToD, DoW, CRASH_TYPE, LIT, WTHR, PRIMARY_CAUSE, NUM_LNS, RD_COND, RD_SURF, TRFC_CTRL, crash) %>% 
        as.data.frame() %>% 
        mutate(CRASH_TYPE = gsub(' |,', '_',CRASH_TYPE),
               LIT = gsub(' |,', '_',LIT),
               WTHR = gsub(' |,', '_',WTHR),
               PRIMARY_CAUSE = gsub(' |,', '_',PRIMARY_CAUSE),
               RD_COND = gsub(' |,', '_',RD_COND),
               RD_SURF = gsub(' |,', '_',RD_SURF),
               TRFC_CTRL = gsub(' |,', '_',TRFC_CTRL)) %>% 
        select(-ToD, -DoW, -CRASH_TYPE, -PRIMARY_CAUSE, -WTHR) %>% 
        dummy.data.frame(names = c( 'LIT', 'RD_COND', 'RD_SURF', 'TRFC_CTRL'), drop = TRUE) %>% 
        select(-TRFC_CTRLOther, -RD_SURFOther, -LITUnknown) %>% # Remove variables for singularity
  # -PRIMARY_CAUSEUnknown, -CRASH_TYPEUnknown, -DoWSaturday, -ToD0, -WTHROther
        group_by(Latitude, Longitude) %>% 
        summarise_all(funs(mean)) %>% 
        ungroup() %>% 
        mutate(crash = ifelse(crash > 0, 1, 0))
        

all_combined <- traffic_data_clean %>% 
  left_join(three11traffic_clean, by = c('Longitude', 'Latitude')) %>% 
  mutate(potholes = ifelse(is.na(potholes), 0, potholes),
         road = ifelse(is.na(road), 0, road),
         bball = ifelse(is.na(bball), 0, bball),
         tree_down = ifelse(is.na(tree), 0, tree),
         rightOfWay = ifelse(is.na(rightOfWay), 0, rightOfWay),
         streetSign = ifelse(is.na(streetSign), 0, streetSign),
         traffic = ifelse(is.na(traffic), 0, traffic),
         streetLight = ifelse(is.na(streetLight), 0, streetLight)) %>% 
  select(-tree) %>% 
  as.data.frame()



  
## Build Model ##
keepRows <- sample(1:nrow(all_combined), .8 * nrow(all_combined))
df_train <- all_combined[keepRows,] 
df_test <- all_combined[-keepRows,]

df_train_matrix <- as.matrix(df_train %>% dplyr::select(-crash, -Latitude, -Longitude))
df_train_Dmatrix <- xgb.DMatrix(df_train_matrix, label = df_train$crash)


xgb_params_1 = list(
        objective = "binary:logistic",                                               # linear classification
        eta = 0.1,                                                                  # learning rate
        gamma = 0,
        min_child_weight = 1,
        colsample_bylevel =1,
        max.depth = 2,                                                               # max tree depth
        eval_metric = "auc",                                                          # evaluation/loss metric
        colsample_bytree = .2 #setting lower decreases gap between training/testing, setting higher reduce training error - increase variance
)

print('XGboost model build')
# fit the model with the arbitrary parameters specified above
allFeatures <- colnames(df_train[-length(df_train)])
print(head(allFeatures))

xgb_1 = xgboost(data = df_train_Dmatrix,
                params = xgb_params_1,
                nrounds = 400,                                                 # max number of trees to build
                verbose = TRUE,                                         
                print_every_n = 100,
                early_stop_round = 50                                          # stop if no improvement within 50 trees
)

boostpred <- predict(xgb_1, newdata = as.matrix(df_test %>% select(-crash)))
boostpred2 <- ifelse(boostpred < 0.5, 0, 1)
confusionMatrix(boostpred2, df_test$crash )

## 82% accuracy out of sample
# 83% in sample accuracy

# Entire dataset

df_full_matrix <- as.matrix(all_combined %>% dplyr::select(-crash, -Latitude, -Longitude))
df_full_Dmatrix <- xgb.DMatrix(df_full_matrix, label = all_combined$crash)


preds_all <- predict(xgb_1, newdata = as.matrix(all_combined %>% select(-crash)))

preds_DF <- data.frame(preds = preds_all, all_combined %>% select(-crash))

explainer = buildExplainer(xgb.model = xgb_1, trainingData = df_train_Dmatrix, type="binary", base_score = 0.5, n_first_tree = xgb_1$best_ntreelimit - 1)
# pred.breakdown = explainPredictions(xgb_1, explainer, train2)



showWaterfall(xgb_1, explainer, df_full_Dmatrix, df_full_matrix,  10, type = "binary")

saveRDS(preds_DF, file = 'preds_DF.RDS')
saveRDS(xgb_1, file = 'xgb_1.RDS')
saveRDS(explainer, file = 'explainer.RDS')
saveRDS(all_combined$crash, file = 'all_combined_crash.RDS')
saveRDS(df_full_matrix, file = 'df_full_matrix.RDS')

