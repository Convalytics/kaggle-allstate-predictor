######################################################
#  Kaggle Allstate Purchase Prediction Challenge
#  Jason Green
#  February, 24th 2014
#  https://github.com/Convalytics/kaggle-allstate-predictor
#  Last Updated: 2/24/2014
######################################################

# Load Packages
library(plyr)
library(ggplot2)
library(gridExtra)

# Set Working Directory
setwd("~/GitHub/kaggle-allstate-predictor")

# Import Data
sampleSubmission <- read.csv("~/GitHub/kaggle-allstate-predictor/sampleSubmission.csv")
test <- read.csv("~/GitHub/kaggle-allstate-predictor/test_v2.csv")
#test <- read.csv("~/GitHub/kaggle-allstate-predictor/test.csv")
train <- read.csv("~/GitHub/kaggle-allstate-predictor/train.csv")

head(sampleSubmission, n=5)
head(test, n=5)
head(train, n=5)

train.quotes <- subset(train, train$record_type == 0)
train.selection <- subset(train, train$record_type == 1)
  


# Aplot <- qplot(A,A,data=train.selection, geom="violin", na.rm=T)
# Bplot <- qplot(B,B,data=train.selection, geom="violin", na.rm=T)
# Cplot <- qplot(C,C,data=train.selection, geom="violin", na.rm=T)
# Dplot <- qplot(D,D,data=train.selection, geom="violin", na.rm=T)
# Eplot <- qplot(E,E,data=train.selection, geom="violin", na.rm=T)
# Fplot <- qplot(F,F,data=train.selection, geom="violin", na.rm=T)
# Gplot <- qplot(G,G,data=train.selection, geom="violin", na.rm=T)


Aplot <- qplot(A,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Bplot <- qplot(B,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Cplot <- qplot(C,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Dplot <- qplot(D,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Eplot <- qplot(E,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Fplot <- qplot(F,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)
Gplot <- qplot(G,data=train.selection, binwidth = 1, geom="histogram", na.rm=T)

grid.arrange(Aplot, Bplot,Cplot,Dplot,Eplot,Fplot,Gplot,ncol=4)
#ggplot(train.selection, aes(x=A, na.rm=T)) + geom_histogram(binwidth=1) 




qplot(state, data=test, geom="histogram")

training <- subset(train.selection, state == "OH")

Aplot <- qplot(A,data=training, binwidth = 1, geom="histogram", na.rm=T)
Bplot <- qplot(B,data=training, binwidth = 1, geom="histogram", na.rm=T)
Cplot <- qplot(C,data=training, binwidth = 1, geom="histogram", na.rm=T)
Dplot <- qplot(D,data=training, binwidth = 1, geom="histogram", na.rm=T)
Eplot <- qplot(E,data=training, binwidth = 1, geom="histogram", na.rm=T)
Fplot <- qplot(F,data=training, binwidth = 1, geom="histogram", na.rm=T)
Gplot <- qplot(G,data=training, binwidth = 1, geom="histogram", na.rm=T)

grid.arrange(Aplot, Bplot,Cplot,Dplot,Eplot,Fplot,Gplot,ncol=4)