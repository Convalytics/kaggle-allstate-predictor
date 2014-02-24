######################################################
#  Kaggle Allstate Purchase Prediction Challenge
#  Jason Green
#  February, 24th 2014
#  https://github.com/Convalytics/kaggle-allstate-predictor
#  Last Updated: 2/24/2014
######################################################

# Load Packages
library(plyr)

# Set Working Directory
setwd("~/GitHub/kaggle-allstate-predictor")

# Import Data
sampleSubmission <- read.csv("~/GitHub/kaggle-allstate-predictor/sampleSubmission.csv")
test <- read.csv("~/GitHub/kaggle-allstate-predictor/test.csv")
train <- read.csv("~/GitHub/kaggle-allstate-predictor/train.csv")

head(sampleSubmission, n=5)
head(test, n=5)
head(train, n=5)

