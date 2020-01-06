## Load library packages

library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(quantmod)
library(xts)
library(TTR)
library(fGarch)
library(vars)
library(car)
library(lubridate)
library(nnet)
library(fpp2)
library(knitr)
library(kableExtra)


# Read in data
library(Quandl)
chicago_home_sales <- Quandl("ZILLOW/M3_SALES", api_key ="5uU2sssTU3c9WvE1dcUT", type="ts")
head(chicago_home_sales)
tail(chicago_home_sales)
str(chicago_home_sales)

# Create train and test sets
chs_train <-window(chicago_home_sales, end = c(2015,1))

chs_test <-window(chicago_home_sales,start= c(2015,2))


### Data Description
# The data is comprised of the monthly home sales in the greater Chicago metropolitan area from June 2008 - June 2016
# Zillow provides data on sold homes, including median sale price for various housing types and sale counts.
# I will fit four models : NNET, ARIMA, HW and ETS


# Plot the training data to get an idea of shape
autoplot(chs_train)


# There appears to be a positive trend from 2008-2014 which seems to level off around 2015. There is also a strong seasonal component. 
# Peak sales occur in summer months and are lowest in winter months


## Fit neural network model             

fit <- nnetar(chs_train , size = 3, repeats = 5, lambda = "auto")
chs_forecast <- forecast(fit, h = 17, PI = TRUE)

print(fit)

# The optimal neural network model is NNAR(5,1,3)[12]. 
# This means that the model uses 5 lags, 1 seasonal component and 3 hidden nodes 

## Plot forecasts
autoplot(chs_forecast) + ylab("# of Sales") + xlab("Time")
autoplot(chs_train, series = "Training Data") + autolayer(chs_forecast$fitted, series = "Fitted Values") + autolayer(chs_forecast$mean, series = "Point Forecasts") + autolayer(chs_test, series = "Test Data") + ylab("# of Sales")+ ggtitle("Monthly Home Sales for Chicago, IL (2008-2015) + NNAR(5,1,6) Forecasts") + ylab("# of Sales") + xlab("Time")


## Accuracy metrics
nnet_acc <- accuracy(chs_forecast, chs_test)
kable(nnet_acc)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


## Fit Holt Winters Model
fit1 <- hw(chs_train, h = 17, seasonal = "multiplicative", alpha = NULL, initial = "optimal", damped = TRUE, level = c(80,95))

## Plot forecasts
autoplot(fit1) + ylab("# of Sales") + xlab("Time")
autoplot(chs_train, series = " Training Data") + autolayer(fit1$fitted, series = "Fitted Values") + autolayer(fit1$mean, series = "Point Forecasts") + autolayer(chs_test, series = "Test Data")


## Accuracy metrics
hw_acc <- accuracy(fit1, chs_test)
kable(hw_acc)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


## Fit ARIMA model
fit2 <- auto.arima(chs_train, stationary = FALSE, seasonal = TRUE, lambda = 0)
arima_fcast <- forecast(fit2, h = 17, PI = TRUE)

print(fit2)

# auto.arima selected an ARIMA (2,0,0)(0,1,1)[12] model with and AICc of -73.8

## Plot forecasts
autoplot(arima_fcast) + ylab("# of Sales") + xlab("Time")
autoplot(chs_train, series = "Training Data") + autolayer(fit2$fitted, series = "Fitted Values") + autolayer(arima_fcast$mean, series = " Point Forecasts") + autolayer(chs_test, series = "Test Data")


## Accuracy metrics
arima_acc <- accuracy(arima_fcast, chs_test)
kable(arima_acc)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


## Fit ETS model
fit3 <- ets(chs_train, model = "ZZZ", allow.multiplicative.trend = TRUE)
ets_fcast <- forecast(fit3, h = 17, PI = TRUE)

print(fit3)

## Auto ETS selected and ETS(M,N,M) with AICc of 1486.379

## Plot forecasts
autoplot(ets_fcast) + ylab("# of Sales") + xlab("Time")
autoplot(chs_train, series = "Training Data") + autolayer(fit2$fitted, series = "Fitted Values") + autolayer(arima_fcast$mean, series = " Point Forecasts") + autolayer(chs_test, series = "Test Data")


## Accuracy metrics
ets_acc <- accuracy(ets_fcast, chs_test)
kable(ets_acc)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


## Model comparison
model_acc <- data.frame(rbind(nnet_acc, hw_acc, arima_acc, ets_acc), row.names = c("Neural Net Training Set", "Neural Net Test Set", "Holt Winter's Training Set", "Holt Winter's Test Set", "ARIMA Training Set", "ARIMA Test Set", "ETS Training Set", "ETS Test Set"))
kable(model_acc)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## Based on the MAE (Mean Absolute Error) metric, the best performing model on the training set and test sets was the Neural Network model with an MAE of 535 (training) and 1179 (test)
## The second best performing model was the ETS model
## The best performing model on the test set was the .

