install.packages('forecast')
install.packages("zoo")
library(forecast)

setwd('C:/Users/grays/Desktop/BusinessAnalytics')
# Read data
dataRC <- read.csv("AustralianWines.csv")
str(dataRC)


#delete missing values/NA's 
dataRC <- na.omit(dataRC)
summary(dataRC,na.strings=c("NA",""))

# Convert data into time series object in R


# start: the time of the first observation
# frequency: number of times per year
xRC <- ts(dataRC$Red, start=c(1980,1),frequency = 12)
xRC
plot(xRC)



# Model 1
red.lmRC <- tslm(xRC~trend)
summary(red.lmRC)

#12 yrs (144 months) as train, 2 yrs (24months) as validation
nTrainRC <- 156
nValidRC <- length(xRC)-nTrainRC


train.tsRC <- window(xRC,start=c(1980,1),end=c(1980,nTrainRC))
valid.tsRC <- window(xRC,start=c(1980,nTrainRC+1),end=c(1980,nTrainRC+nValidRC))

train.lmRC <- tslm(train.tsRC~trend)
summary(train.lmRC)
train.lm.predRC <- forecast(train.lmRC,h=nValidRC,level=0)


# Visualize the linear trend model
par(mfrow = c(1, 1))
plot(train.lm.predRC, ylim = c(400, 4000),  ylab = "Red", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1980,1995),main = "", flty = 2)
axis(1, at = seq(1980, 1995, 1), labels = format(seq(1980, 1995, 1)))
lines(train.lm.predRC$fitted, lwd = 2, col = "blue")
lines(valid.tsRC)

# Evaluate model performance
accuracy(train.lm.predRC,valid.tsRC)



# A model with seasonality
train.lm.seasonRC <- tslm(train.tsRC ~ season)
summary(train.lm.seasonRC)
train.lm.season.predRC <- forecast(train.lm.seasonRC, h = nValidRC, level = 0)
accuracy(train.lm.season.predRC,valid.tsRC)

# Visualize the seasonal model
par(mfrow = c(1, 1))
plot(train.lm.season.predRC, ylim = c(400, 4000),  ylab = "Red", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1980,1995),main = "", flty = 2)
axis(1, at = seq(1980, 1995, 1), labels = format(seq(1980, 1995, 1)))
lines(train.lm.season.predRC$fitted, lwd = 2, col = "blue")
lines(valid.tsRC)


# Linear trend and seasonal
train.lm.trend.seasonRC <- tslm(train.tsRC ~ trend + season)
summary(train.lm.trend.seasonRC)
train.lm.trend.season.predRC <- forecast(train.lm.trend.seasonRC, h = nValidRC, level = 0)
accuracy(train.lm.trend.season.predRC,valid.tsRC)

par(mfrow = c(1, 1))
plot(train.lm.trend.season.predRC, ylim = c(400, 4000),  ylab = "Red", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1980,1995),main = "", flty = 2)
axis(1, at = seq(1980, 1995, 1), labels = format(seq(1980, 1995, 1)))
lines(train.lm.trend.season.predRC$fitted, lwd = 2, col = "blue")
lines(valid.tsRC)



## simple exponential smRCthing model

sesRC <- ses(train.tsRC, alpha = 0.6492, h=24)
autoplot(sesRC)
accuracy(sesRC,valid.tsRC)

# Use ses function to estimate alpha
ses1RC <- ses(train.tsRC, alpha = NULL, h=24)
summary(ses1RC)
accuracy(ses1RC,valid.tsRC)
