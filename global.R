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
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(leaflet)


# Change your wd to where ever the RDS files are saved
setwd('~/Projects/QueenCityOutliers/')


# Read in data from build
xgb_1 <- readRDS(file = 'xgb_1.RDS')
explainer <- readRDS(file = 'explainer.RDS')
df_full_matrix<- readRDS(file = 'df_full_matrix.RDS')
preds_DF <- readRDS(file = 'preds_DF.RDS')
allzips <- readRDS("preds_DF.RDS")
all_combined_crash <- readRDS("all_combined_crash.RDS")
df_full_Dmatrix <- xgb.DMatrix(df_full_matrix, label = all_combined_crash)
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
index <- 1
cleantable <- allzips %>%
  select(
    Preds = preds,
    Lat = Latitude,
    Long = Longitude
  )
