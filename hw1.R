install.packages("caret")
install.packages("e1071")
install.packages("car")
install.packages("pROC")
install.packages("dplyr") 
library(caret)
library(e1071)
library(car)
library(pROC)
library(dplyr)

setwd('C:/Users/grays/Desktop/ConsumerAnalytics')


dfRC <- read.csv("inq2015.csv",na.strings=c("NA",""))

summary(dfRC)
str(dfRC)


cols.dont.want <- c("ACADEMIC_INTEREST_1","ACADEMIC_INTEREST_2","IRSCHOOL","CONTACT_CODE1","CONTACT_DATE1")
dfRC <- dfRC[, ! names(dfRC) %in% cols.dont.want, drop = F]


#drop additional unwanted variables
#additional one that could be deleted is ethnicity
cols.dont.want <- c("CONTACT_DATE","LEVEL_YEAR","telecq","sex")
dfRC <- dfRC[, ! names(dfRC) %in% cols.dont.want, drop = F]

#comment out, before running the vif so it shows the high vif
cols.dont.want <- c("TOTAL_CONTACTS","SELF_INIT_CNTCTS","TRAVEL_INIT_CNTCTS","SOLICITED_CNTCTS","REFERRAL_CNTCTS")
dfRC <- dfRC[, ! names(dfRC) %in% cols.dont.want, drop = F]

#comment out before running everything, p value too high
cols.dont.want <- c("ETHNICITY")
dfRC <- dfRC[, ! names(dfRC) %in% cols.dont.want, drop = F]


#variables in which vif was high
#transform variables from int to factor 
dfRC$Enroll <- factor(dfRC$Enroll)
#dfRC$sex <- factor(dfRC$sex)
dfRC$premiere<-factor(dfRC$premiere)
dfRC$stucell<-factor(dfRC$stucell)
#dfRC$TRAVEL_INIT_CNTCTS<-factor(dfRC$TRAVEL_INIT_CNTCTS)
#dfRC$SOLICITED_CNTCTS<-factor(dfRC$SOLICITED_CNTCTS)
#dfRC$REFERRAL_CNTCTS<-factor(dfRC$REFERRAL_CNTCTS)
dfRC$CAMPUS_VISIT<-factor(dfRC$CAMPUS_VISIT)

str(dfRC)
summary(dfRC,na.strings=c("NA",""))


vif(glm(formula=Enroll ~ . , family = binomial(link='logit'),data = dfRC))


install.packages("Hmisc")
library(Hmisc)

#commented out, deleted due to pvalue
#dfRC$ETHNICITY <- with(dfRC,impute(ETHNICITY,max))
dfRC$TERRITORY <- with(dfRC,impute(TERRITORY,max))
dfRC$satscore <- with(dfRC,impute(satscore,mean))
dfRC$avg_income <- with(dfRC,impute(avg_income,mean))
dfRC$distance <- with(dfRC,impute(distance,mean))

summary(dfRC,na.strings=c("NA",""))
str(dfRC)



set.seed(60)
trainIndex <- createDataPartition(dfRC$Enroll,
                                    p=0.7,
                                    list=FALSE,
                                    times=1)

dfRC.train <- dfRC[trainIndex,]


dfRC.valid <-dfRC[-trainIndex,]

#Run baseline model
baseline.model <- train(Enroll~.,
                          data=dfRC.train,
                          method='glm',
                          family='binomial',
                          na.action=na.pass)


summary(baseline.model)

#Evaluation model performance using the validation dataset

#Criteria 1: the confusion matrix
predictionRC <- predict(baseline.model,newdata=dfRC.valid)


dfRC.valid.nonmissing <- na.omit(dfRC.valid)

confusionMatrix(predictionRC,dfRC.valid.nonmissing$Enroll)


predRC.probabilities <- predict(baseline.model,newdata=dfRC.valid,type='prob')

regressionRC.ROC <- roc(predictor=predRC.probabilities$`1`,
                        response=dfRC.valid.nonmissing$Enroll,
                        levels=levels(dfRC.valid.nonmissing$Enroll))
plot(regressionRC.ROC)
regressionRC.ROC$auc


# User-defined functions to calculate cumulative lift & gains
lift <- function(depvar, predcol, groups=10) {
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  
  helper <- data.frame(cbind(depvar, predcol))
  helper <- helper[order(-helper$predcol),] 
  
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}


dtRC=lift(dfRC.valid.nonmissing$Enroll,predRC.probabilities$`1`,groups=10)
print(dtRC)


plot(dtRC$bucket, dtRC$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")



#DECISION TREE MODEL
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)


# Build a decision tree model
treeRC.model <- train(Enroll~.,
                      data=dfRC.train,
                      method="rpart",
                      na.action=na.pass)


treeRC.model


prp(treeRC.model$finalModel,type=2,extra=106)




#Criteria 1: the confusion matrix

predictionRC <- predict(treeRC.model,newdata=dfRC.valid,na.action = na.pass)
confusionMatrix(predictionRC,dfRC.valid$Enroll)

#Criteria 2: the ROC curve and area under the curve
treeRC.probabilities <- predict(treeRC.model,newdata=dfRC.valid,type='prob',na.action=na.pass)

treeRC.ROC <- roc(predictor=treeRC.probabilities$`1`,
                  response=dfRC.valid$Enroll,
                  levels=levels(dfRC.valid$Enroll))

plot(treeRC.ROC)

treeRC.ROC$auc