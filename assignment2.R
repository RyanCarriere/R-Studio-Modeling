install.packages('ISLR')
install.packages('caret')
install.packages('randomForest')

library(ISLR)
library(caret)
library(randomForest)
setwd('C:/Users/grays/Desktop/ConsumerAnalytics')
dfRC <- read.csv("BostonHousing.csv")
summary(dfRC)
str(dfRC)

#turn cat..medv to factor because it is binary
dfRC$CAT..MEDV <- factor(dfRC$CAT..MEDV)
dfRC$CHAS <- factor(dfRC$CHAS)
summary(dfRC)
str(dfRC)

#delete unwanted "continuous DV" variable
cols.dont.want <- c("MEDV")
dfRC <- dfRC[, ! names(dfRC) %in% cols.dont.want, drop = F]

# Data partition with the Caret package 
set.seed(50)
trainIndexRC <- createDataPartition(dfRC$CAT..MEDV,
                                    p=0.7,
                                    list=FALSE,
                                    times=1)
df.trainRC <- dfRC[trainIndexRC,] 
df.testRC <-dfRC[-trainIndexRC,]

#-----------------------------------
#RANDOM FOREST

# default random forest model. 
rf_defaultRC <- train(CAT..MEDV~.,
                      data=df.trainRC,
                      method='rf',
                      metric='Accuracy',    
                      ntree=100)  

print(rf_defaultRC) 


tuneGridRC <- expand.grid(.mtry=c(1:12)) 



rf_mtryRC <- train(CAT..MEDV~.,
                   data=df.trainRC,
                   method='rf',
                   metric='Accuracy',
                   tuneGridRC=tuneGridRC,
                   importance=TRUE,    
                   ntree=100)
print(rf_mtryRC)

#check importance
varImp(rf_mtryRC)  

# Evaluate model performance
predictionRC <- predict(rf_mtryRC,df.testRC)
confusionMatrix(predictionRC,df.testRC$CAT..MEDV)


# SVM

library(caret)
library(ggplot2)

str(dfRC)


#10 fold validation
trControlRC <- trainControl(method='cv',
                            number=10,
                            search='grid')

# SVM Model with the linear Kernel function
# Pre-processing data with centering and scaling
svm_linearRC <- train(CAT..MEDV~.,
                      data=df.trainRC,
                      method='svmLinear',  
                      trControlRC=trControlRC, 
                      preProcess=c('center','scale')) 
print(svm_linearRC)

# Evaluate the linear SVM model performance
linear_predRC <- predict(svm_linearRC,df.testRC)
confusionMatrix(linear_predRC,df.testRC$CAT..MEDV)


# SVM Model with the Radial Kernel function
svm_radialRC <- train(CAT..MEDV~.,
                    data=df.trainRC,
                    method='svmRadial',
                    trControlRC=trControlRC,
                    preProcess=c('center','scale'))


print(svm_radialRC)

# Evaluate the radial SVM model performance
radial_predRC <- predict(svm_radialRC,df.testRC)
confusionMatrix(radial_predRC,df.testRC$CAT..MEDV)


# Additional model tuning for the radial SVM 
grid_radialRC <- expand.grid(sigma = c(0.0,0.5,0.75,1.0,1.3,1.5),
                           C = c(0,0.05, 0.25, 0.5, 0.75, 1))

svm_radial_tuneRC <- train(CAT..MEDV~.,
                         data=df.trainRC,
                         method='svmRadial',
                         trControlRC=trControlRC,
                         preProcess=c('center','scale'),
                         tuneGridRC=grid_radialRC)
print(svm_radial_tuneRC)

# Evaluate the tuned  radial SVM 
radial_tune_predRC <- predict(svm_radial_tuneRC,df.testRC)
confusionMatrix(radial_tune_predRC,df.testRC$CAT..MEDV)



