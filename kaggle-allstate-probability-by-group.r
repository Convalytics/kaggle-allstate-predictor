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
library(qcc)    # for pareto.chart


# Set Working Directory
setwd("~/GitHub/kaggle-allstate-predictor")

# Import Data
sampleSubmission <- read.csv("~/GitHub/kaggle-allstate-predictor/sampleSubmission.csv")
#test <- read.csv("~/GitHub/giantfiles/kaggle-allstate-predictor/test_v2.csv")
#test <- read.csv("~/GitHub/kaggle-allstate-predictor/test.csv")
train <- read.csv("~/GitHub/giantfiles/kaggle-allstate-predictor/train.csv")


train.quotes <- subset(train, train$record_type == 0)
train.selection <- subset(train, train$record_type == 1)

### Subset train.quotes to get a list of "last quotes".
train.lastQuote <- train.quotes[ !duplicated( train.quotes$customer_ID, fromLast=TRUE ) , ]

### Compare to train.selection to see how often each option is changed.
### Merge. then compare.
lastQuoteWithSelection <- merge(x = train.selection, y = train.lastQuote, by.x="customer_ID", by.y="customer_ID")
lastQuoteWithSelection$Asame <- with(lastQuoteWithSelection, ifelse(A.x == A.y, 1,0))
lastQuoteWithSelection$Bsame <- with(lastQuoteWithSelection, ifelse(B.x == B.y, 1,0))
lastQuoteWithSelection$Csame <- with(lastQuoteWithSelection, ifelse(C.x == C.y, 1,0))
lastQuoteWithSelection$Dsame <- with(lastQuoteWithSelection, ifelse(D.x == D.y, 1,0))
lastQuoteWithSelection$Esame <- with(lastQuoteWithSelection, ifelse(E.x == E.y, 1,0))
lastQuoteWithSelection$Fsame <- with(lastQuoteWithSelection, ifelse(F.x == F.y, 1,0))
lastQuoteWithSelection$Gsame <- with(lastQuoteWithSelection, ifelse(G.x == G.y, 1,0))
  

lastQuote.byState <- ddply(lastQuoteWithSelection, "state.x", summarise,
                      count = length(customer_ID),
                      allStay = length(subset(customer_ID, 
                                              Asame==1 
                                              & Bsame==1
                                              & Csame==1
                                              & Dsame==1
                                              & Esame==1
                                              & Fsame==1
                                              & Gsame==1
                                              )
                                       ) / length(customer_ID),
                      aStay = length(subset(Asame,Asame==1)) / length(Asame),
                      bStay = length(subset(Bsame,Bsame==1)) / length(Bsame),
                      cStay = length(subset(Csame,Csame==1)) / length(Csame),
                      dStay = length(subset(Dsame,Dsame==1)) / length(Dsame),
                      eStay = length(subset(Esame,Esame==1)) / length(Esame),
                      fStay = length(subset(Fsame,Fsame==1)) / length(Fsame),
                      gStay = length(subset(Gsame,Gsame==1)) / length(Gsame)
                    )
  
lastQuote.byAll <- ddply(lastQuoteWithSelection, 
                           c("state.x","married_couple.x","homeowner.x"),  
                           summarise,
                           count = length(customer_ID),
                           allStay = length(subset(customer_ID, 
                                                   Asame==1 
                                                   & Bsame==1
                                                   & Csame==1
                                                   & Dsame==1
                                                   & Esame==1
                                                   & Fsame==1
                                                   & Gsame==1
                           )
                           ) / length(customer_ID),
                           aStay = length(subset(Asame,Asame==1)) / length(Asame),
                           bStay = length(subset(Bsame,Bsame==1)) / length(Bsame),
                           cStay = length(subset(Csame,Csame==1)) / length(Csame),
                           dStay = length(subset(Dsame,Dsame==1)) / length(Dsame),
                           eStay = length(subset(Esame,Esame==1)) / length(Esame),
                           fStay = length(subset(Fsame,Fsame==1)) / length(Fsame),
                           gStay = length(subset(Gsame,Gsame==1)) / length(Gsame)
)



#Get Overall Probabilities by Option ####################################################
prob.overall <- data.frame(matrix(ncol = 22, nrow = 1))
names(prob.overall) <- c("a0","a1","a2","b0","b1","c1","c2","c3","c4","d1","d2","d3","e0","e1","f0","f1","f2","f3","g1","g2","g3","g4")
rowcount <- nrow(train.selection)

# A
prob.overall$a0 <- nrow(subset(train.selection, A == 0)) / rowcount
prob.overall$a1 <- nrow(subset(train.selection, A == 1)) / rowcount
prob.overall$a2 <- nrow(subset(train.selection, A == 2)) / rowcount
# B
prob.overall$b0 <- nrow(subset(train.selection, B == 0)) / rowcount
prob.overall$b1 <- nrow(subset(train.selection, B == 1)) / rowcount
# C
prob.overall$c1 <- nrow(subset(train.selection, C == 1)) / rowcount
prob.overall$c2 <- nrow(subset(train.selection, C == 2)) / rowcount
prob.overall$c3 <- nrow(subset(train.selection, C == 3)) / rowcount
prob.overall$c4 <- nrow(subset(train.selection, C == 4)) / rowcount
# D
prob.overall$d1 <- nrow(subset(train.selection, D == 1)) / rowcount
prob.overall$d2 <- nrow(subset(train.selection, D == 2)) / rowcount
prob.overall$d3 <- nrow(subset(train.selection, D == 3)) / rowcount
# E
prob.overall$e0 <- nrow(subset(train.selection, E == 0)) / rowcount
prob.overall$e1 <- nrow(subset(train.selection, E == 1)) / rowcount
# F
prob.overall$f0 <- nrow(subset(train.selection, F == 0)) / rowcount
prob.overall$f1 <- nrow(subset(train.selection, F == 1)) / rowcount
prob.overall$f2 <- nrow(subset(train.selection, F == 2)) / rowcount
prob.overall$f3 <- nrow(subset(train.selection, F == 3)) / rowcount
# G
prob.overall$g1 <- nrow(subset(train.selection, G == 1)) / rowcount
prob.overall$g2 <- nrow(subset(train.selection, G == 2)) / rowcount
prob.overall$g3 <- nrow(subset(train.selection, G == 3)) / rowcount
prob.overall$g4 <- nrow(subset(train.selection, G == 4)) / rowcount
######################################################################
######################################################################


#########################################################################################
#Get Probabilities By State ####################################################
prob.byState <- data.frame(matrix(ncol = 24, nrow = 1))
names(prob.byState) <- c("count","state","a0","a1","a2","b0","b1","c1","c2","c3","c4","d1","d2","d3","e0","e1","f0","f1","f2","f3","g1","g2","g3","g4")


prob.byState <- ddply(train.selection, "state", summarise,
                count = length(customer_ID),
                a0 = length(subset(A,A==0)) / length(A),
                a1 = length(subset(A,A==1)) / length(A),
                a2 = length(subset(A,A==2)) / length(A),
                b0 = length(subset(B,B==0)) / length(B),
                b1 = length(subset(B,B==1)) / length(B),
                c1 = length(subset(C,C==1)) / length(C),
                c2 = length(subset(C,C==2)) / length(C),
                c3 = length(subset(C,C==3)) / length(C),
                c4 = length(subset(C,C==4)) / length(C),
                d1 = length(subset(D,D==1)) / length(D),
                d2 = length(subset(D,D==2)) / length(D),
                d3 = length(subset(D,D==3)) / length(D),
                e0 = length(subset(E,E==0)) / length(E),
                e1 = length(subset(E,E==1)) / length(E),
                f0 = length(subset(F,F==0)) / length(F),
                f1 = length(subset(F,F==1)) / length(F),
                f2 = length(subset(F,F==2)) / length(F),
                f3 = length(subset(F,F==3)) / length(F),
                g1 = length(subset(G,G==1)) / length(G),
                g2 = length(subset(G,G==2)) / length(G),
                g3 = length(subset(G,G==3)) / length(G),
                g4 = length(subset(G,G==4)) / length(G)
                )

#########################################################################################
#Get Probabilities By Everything ####################################################
prob.byAll <- data.frame(matrix(ncol = 26, nrow = 1))
names(prob.byAll) <- c("count",
                       "state", 
                       "married_couple",
                       "homeowner",
                       #"car_value",
                       #"risk_factor",
                       "a0","a1","a2","b0","b1","c1","c2","c3","c4","d1","d2","d3","e0","e1","f0","f1","f2","f3","g1","g2","g3","g4")

prob.byAll <- ddply(train.selection, 
                      c("state","married_couple","homeowner"), 
                      summarise,
                      count = length(customer_ID),
                      a0 = length(subset(A,A==0)) / length(A),
                      a1 = length(subset(A,A==1)) / length(A),
                      a2 = length(subset(A,A==2)) / length(A),
                      b0 = length(subset(B,B==0)) / length(B),
                      b1 = length(subset(B,B==1)) / length(B),
                      c1 = length(subset(C,C==1)) / length(C),
                      c2 = length(subset(C,C==2)) / length(C),
                      c3 = length(subset(C,C==3)) / length(C),
                      c4 = length(subset(C,C==4)) / length(C),
                      d1 = length(subset(D,D==1)) / length(D),
                      d2 = length(subset(D,D==2)) / length(D),
                      d3 = length(subset(D,D==3)) / length(D),
                      e0 = length(subset(E,E==0)) / length(E),
                      e1 = length(subset(E,E==1)) / length(E),
                      f0 = length(subset(F,F==0)) / length(F),
                      f1 = length(subset(F,F==1)) / length(F),
                      f2 = length(subset(F,F==2)) / length(F),
                      f3 = length(subset(F,F==3)) / length(F),
                      g1 = length(subset(G,G==1)) / length(G),
                      g2 = length(subset(G,G==2)) / length(G),
                      g3 = length(subset(G,G==3)) / length(G),
                      g4 = length(subset(G,G==4)) / length(G)
)

#########################################################################################
#Get Probabilities By Risk Factor ####################################################
prob.byRisk <- data.frame(matrix(ncol = 24, nrow = 1))
names(prob.byRisk) <- c("count","risk_factor","a0","a1","a2","b0","b1","c1","c2","c3","c4","d1","d2","d3","e0","e1","f0","f1","f2","f3","g1","g2","g3","g4")


prob.byRisk <- ddply(train.selection, "risk_factor", summarise,
                      count = length(customer_ID),
                      a0 = length(subset(A,A==0)) / length(A),
                      a1 = length(subset(A,A==1)) / length(A),
                      a2 = length(subset(A,A==2)) / length(A),
                      b0 = length(subset(B,B==0)) / length(B),
                      b1 = length(subset(B,B==1)) / length(B),
                      c1 = length(subset(C,C==1)) / length(C),
                      c2 = length(subset(C,C==2)) / length(C),
                      c3 = length(subset(C,C==3)) / length(C),
                      c4 = length(subset(C,C==4)) / length(C),
                      d1 = length(subset(D,D==1)) / length(D),
                      d2 = length(subset(D,D==2)) / length(D),
                      d3 = length(subset(D,D==3)) / length(D),
                      e0 = length(subset(E,E==0)) / length(E),
                      e1 = length(subset(E,E==1)) / length(E),
                      f0 = length(subset(F,F==0)) / length(F),
                      f1 = length(subset(F,F==1)) / length(F),
                      f2 = length(subset(F,F==2)) / length(F),
                      f3 = length(subset(F,F==3)) / length(F),
                      g1 = length(subset(G,G==1)) / length(G),
                      g2 = length(subset(G,G==2)) / length(G),
                      g3 = length(subset(G,G==3)) / length(G),
                      g4 = length(subset(G,G==4)) / length(G)
)

#########################################################################################
#Get Probabilities By car_value ####################################################
prob.byCarValue <- data.frame(matrix(ncol = 24, nrow = 1))
names(prob.byCarValue) <- c("count","car_value","a0","a1","a2","b0","b1","c1","c2","c3","c4","d1","d2","d3","e0","e1","f0","f1","f2","f3","g1","g2","g3","g4")


prob.byCarValue <- ddply(train.selection, "car_value", summarise,
                     count = length(customer_ID),
                     a0 = length(subset(A,A==0)) / length(A),
                     a1 = length(subset(A,A==1)) / length(A),
                     a2 = length(subset(A,A==2)) / length(A),
                     b0 = length(subset(B,B==0)) / length(B),
                     b1 = length(subset(B,B==1)) / length(B),
                     c1 = length(subset(C,C==1)) / length(C),
                     c2 = length(subset(C,C==2)) / length(C),
                     c3 = length(subset(C,C==3)) / length(C),
                     c4 = length(subset(C,C==4)) / length(C),
                     d1 = length(subset(D,D==1)) / length(D),
                     d2 = length(subset(D,D==2)) / length(D),
                     d3 = length(subset(D,D==3)) / length(D),
                     e0 = length(subset(E,E==0)) / length(E),
                     e1 = length(subset(E,E==1)) / length(E),
                     f0 = length(subset(F,F==0)) / length(F),
                     f1 = length(subset(F,F==1)) / length(F),
                     f2 = length(subset(F,F==2)) / length(F),
                     f3 = length(subset(F,F==3)) / length(F),
                     g1 = length(subset(G,G==1)) / length(G),
                     g2 = length(subset(G,G==2)) / length(G),
                     g3 = length(subset(G,G==3)) / length(G),
                     g4 = length(subset(G,G==4)) / length(G)
)



###########################################################################
### Combine probabilities ###


###########################################################################
write.csv(prob.byAll, file = "probabilities_byAll.csv", row.names=F)
write.csv(prob.byState, file = "probabilities_byState.csv", row.names=F)
write.csv(prob.byRisk, file = "probabilities_byRisk.csv", row.names=F)
write.csv(prob.byCarValue, file = "probabilities_byCarValue.csv", row.names=F)

#length(levels(as.factor(train.selection$cost)))
#head(train.selection$location)

field <- 2
prob.byRisk[,paste("a", field,sep="")]


trained <- merge(x = train.lastQuote, y = lastQuote.byAll, by.x=c("state","married_couple","homeowner"), by.y=c("state.x","married_couple.x","homeowner.x"))
trained <- merge(x = trained, y = prob.byAll, by.x=c("state","married_couple","homeowner"), by.y=c("state","married_couple","homeowner"))
trained$aFinal <- with(trained, ifelse(get(paste("a", A, sep="")) > (1-aStay), A, 
                                       ifelse(a0 > a1 & a0 > a2,0,
                                              ifelse(a1 > a0 & a1 > a2,1,
                                                     ifelse(a2 > a0 & a2 > a1,2, A
                                                     )))))
trained$bFinal <- with(trained, ifelse(get(paste("b", B, sep="")) > (1-bStay), B, 
                                       ifelse(b0 > b1,0,
                                              ifelse(b1 > b0,1, B
                                              ))))

trained$cFinal <- with(trained, ifelse(get(paste("c", C, sep="")) > (1-cStay), C, 
                                       ifelse(c1 > c2 & c1 > c3 & c1 > c4,8,
                                              ifelse(c2 > c1 & c2 > c3 & c2 > c4,7,
                                                     ifelse(c3 > c1 & c3 > c2 & c3 > c4,6,
                                                            ifelse(c4 > c1 & c4 > c2 & c4 > c3,5,9
                                              ))))))
trained$cFinal <- with(trained, ifelse(get(paste("c", C, sep="")) > (1-cStay), C, which.max(c(c1,c2,c3,c4))))
#trained$cFinal <- with(trained, ifelse(get(paste("c", C, sep="")) > (1-cStay), C,9))
#subset(trained,cFinal == 9)
#cpasted <- 
#   with(trained, get(paste("c", C, sep="")))
#with(trained, max.col(c(a0,a1,a2))-1)

subset(trained, cFinal != C)

write.csv(subset(trained, cFinal != C),file="trainedSample.csv",row.names=F)
