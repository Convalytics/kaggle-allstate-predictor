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
test <- read.csv("~/GitHub/kaggle-allstate-predictor/test_v2.csv")
# test <- read.csv("~/GitHub/kaggle-allstate-predictor/test.csv")
train <- read.csv("~/GitHub/kaggle-allstate-predictor/train.csv")

# head(sampleSubmission, n=5)
# head(test, n=5)
# head(train, n=5)

#### Last Quote submission

#Gets the last quoted plan
sub <- test[ !duplicated( test$customer_ID, fromLast=TRUE ) , ]



#### RULES ###########

#Test
#subset(sub,state == "OH" & G < 2 )

# The F option in NY is always 0
sub$F[sub$state == "NY"] <- 0

# The G option in FL is never 1 or 2, and is 3 80% of the time.
sub$G[sub$state == "FL" & sub$G < 3] <- 3

# OH - G is never 1. Is 3 80% of the time.
# None found in the test set. :(
sub$G[sub$state == "OH" & sub$G == 1] <- 3

# GA - C is 2, never 1. ... D is 2, never 1.
sub$C[sub$state == "GA" & sub$C < 2] <- 2
sub$D[sub$state == "GA" & sub$D < 2] <- 2

# Car age > 15. 
sub$A[sub$car_age > 15 & sub$A != 0 & sub$A != 1] <- 0
sub$E[sub$car_age > 15 & sub$E != 0] <- 0
sub$F[sub$car_age > 15 & sub$F == 3] <- 0
##################################################################################

# Build submission file.
sub$plan <- paste0( sub[,18],sub[,19],sub[,20],sub[,21],sub[,22],sub[,23],sub[,24] )

lastQuote <- sub[,c]
write.csv(sub[,c(1,ncol(sub))],paste0('convalytics_allstate_9.csv'),quote=FALSE , row.names = FALSE )


#############################################


