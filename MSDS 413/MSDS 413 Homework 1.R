setwd("C:/Users/John/Desktop/MSDS 413")

## Load library packages
library(ggplot2)
library(fpp2)
library(gridExtra)

## Question 1: Section 2.10 Exercises,  problem 6

#Create autoplot variables
autoplot_hsales <- autoplot(hsales) 
autoplot_usdeaths <- autoplot(usdeaths) 
autoplot_bricksq <- autoplot(bricksq) 
autoplot_sunspotarea <- autoplot(sunspotarea) 
autoplot_gasoline <- autoplot(gasoline) 
            
## Plot using grid.arrange
grid.arrange(autoplot_hsales, autoplot_usdeaths, autoplot_bricksq, autoplot_sunspotarea, autoplot_gasoline, ncol = 3, nrow = 2)

## Create seasonplot variables
seasonplot_hsales <- ggseasonplot(hsales)
seasonplot_usdeaths <- ggseasonplot(usdeaths)
seasonplot_bricksq <- ggseasonplot(bricksq)
seasonplot_gasoline <- ggseasonplot(gasoline)

##Plot using grid.arrange
## Note: Sunspotarea data is not seasonal and cannot be plotted using ggseasonplot
grid.arrange(seasonplot_hsales, seasonplot_usdeaths, seasonplot_bricksq, seasonplot_gasoline, ncol = 2, nrow = 2)


## Create subseriesplot variables
subseriesplot_hsales <- ggsubseriesplot(hsales)
subseriesplot_usdeaths <- ggsubseriesplot(usdeaths)
subseriesplot_bricksq <- ggsubseriesplot(bricksq)

## Plot using grid.arrange
## Note: Sunspotarea data is seasonal and cannot be plotted
## Note: Gasoline data does not have integer data
grid.arrange(subseriesplot_hsales, subseriesplot_usdeaths, subseriesplot_bricksq, ncol = 2, nrow = 2)


## Create lagplot variables

lagplot_hsales <- gglagplot(hsales)
lagplot_usdeaths <- gglagplot(usdeaths)
lagplot_bricksq <- gglagplot(bricksq)
lagplot_sunspotarea <- gglagplot(sunspotarea)
lagplot_gasoline <- gglagplot(gasoline)

##Plot using grid.arrange
grid.arrange(lagplot_hsales, lagplot_usdeaths, lagplot_bricksq, lagplot_sunspotarea, lagplot_gasoline, ncol = 3, nrow = 2)

## Create Acf variables
acf_hsales <- Acf(hsales)
acf_usdeaths <- Acf(usdeaths)
acf_bricksq <- Acf(bricksq)
acf_sunspotarea <- Acf(sunspotarea)
acf_gasoline <- Acf(gasoline)

## Plot 
acf_hsales
acf_usdeaths
acf_bricksq
acf_sunspotarea
acf_gasoline

#Can you spot any seasonality, cyclicity and trend?
#What do you learn about the series?
 
## From the hsales data, I learned that there is some seasonality but there does not seem to be any trend to the data. 
## From the usdeaths data there is seasonality but not trend either. 
## For the bricksq data there does not seem to be much seasonality but a stron positive trend. For the sunspotarea data there is no seasonality and no trend either but some cyclicity. For the gasoline data there is a positive trend and some seasonality.

## Question 2: Section 3.7 Exercises, problems 9, 10, 11, 12

#9. 
# a) Use window() to create three training sets for visnights[,"QLDMetro"], omitting the last 1, 2 and 3 years; call these train1, train2, and train3, respectively.

train1 <- window(visnights[, "QLDMetro"], end = c(2015, 4))

train2 <- window(visnights[, "QLDMetro"], end = c(2014, 4))

train3 <- window(visnights[, "QLDMetro"], end = c(2013, 4))
                
# b) Compute one year of forecasts for each training set using the snaive() method. Call these fc1, fc2 and fc3, respectively.

fc1 <- snaive(train1, h = 4, level = c(80,95))

fc2 <- snaive(train2, h = 4, level = c(80,95))

fc3 <- snaive(train3, h = 4, level = c(80,95))

# c) Use accuracy() to compare the MAPE over the three test sets. Comment on these.

accuracy(fc1)

accuracy(fc2)

accuracy(fc3)


#10. Use the Dow Jones index (data set dowjones) to do the following:

dowjones <-ts(dowjones)

dowjones_df <- data.frame(dowjones)

summary(dowjones)



dowjones# a) Produce a time plot of the series.

dowjones_plot <- autoplot(dowjones, xlab = "Time (in days)", ylab = "Closing Price", main = "Dow Jones Index (Aug. 28 1972 - Dec 18 1972)")

dowjones_plot


# b) Produce forecasts using the drift method and plot them.

dowjones_drift <- autoplot(dowjones,xlab = "Time (in days)", ylab = "Closing Price") + autolayer(rwf(dowjones, drift = TRUE, h = 30, level = c(80,95)))

dowjones_drift

# c) Show that the forecasts are identical to extending the line drawn between the first and last observations.

dowjones_drift_line <- autoplot(dowjones,xlab = "Time (in days)", ylab = "Closing Price") + autolayer(rwf(dowjones, drift = TRUE, h = 30, level = c(80,95)))



# d) Try using some of the other benchmark functions to forecast the same data set. Which do you think is best? Why?

#Average method
dowjones_average <- autoplot(dowjones, xlab = "Time (in days)", ylab = "Closing Price") + autolayer(meanf(dowjones, h = 30, level = c(80,95)))

dowjones_average

#Naive method
dowjones_naive <- autoplot(dowjones, xlab = "Time (in days)", ylab = "Closing Price") + autolayer(naive(dowjones, h = 30, level = c(80,95)))

dowjones_naive

#Seasonal Naive method
dowjones_snaive <- autoplot(dowjones, xlab = "Time (in days)", ylab = "Closing Price") + autolayer(snaive(dowjones, h = 30, level = c(80,95)))

dowjones_snaive

Dowjones_benchmarks <- autoplot(dowjones) + 
  autolayer(meanf(dowjones, h=30), series="Average", PI=FALSE) + 
  
  autolayer(naive(dowjones, h=30),series="Naïve", PI=FALSE) +
 
  autolayer(rwf(dowjones, drift=TRUE, h=30),series="Drift", PI=FALSE) +
  
  autolayer(snaive(dowjones, h=30),series="Seasonal Naïve", PI=FALSE) +
  
  ggtitle("Daily Dow Jones Index (Aug 28 1972 - Dec 18 1972)") +
  
  xlab("Day") + ylab("Closing Price (US$)") +
  
  guides(colour=guide_legend(title="Forecast"))

  Dowjones_benchmarks
  
  ## I would say that the drift method is best. It appears that the average method is too simple for the data. 
  #Both the naive and seasonal naive methods output the same forecast because there is no seasonality in the data. 
  #Since the Naive method only takes the last observation as the basis of the forecast, it is too simple as well. 
  #These methods do not account for longer term trends in the data. 
  #The drift method is more complex by averaging changes in the data. This makes it the most suitable method. 
  

#11. Consider the daily closing IBM stock prices (data set ibmclose).


# a) Produce some plots of the data in order to become familiar with it.
ibm_plot <- autoplot(ibmclose, xlab = "Time (days)", ylab = "Price (US$)", main = "IBM Closing Stock Price")

ibm_plot

tsdisplay(ibmclose, plot.type = "scatter")


# b) Split the data into a training set of 300 observations and a test set of 69 observations.
library(rmutil)
library(rowr)

X = data.frame(ibmclose)

train_size = round(nrow(X) * 0.813)

train = X$ibmclose[1:train_size]

test = X$ibmclose[train_size:len(X)]

str(train)
str(test)

# c) Try using various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?

ibm_average <- meanf(train, h=70, level = c(80,95))

ibm_naive <- naive(train, h =70, level = c(80,95))

ibm_drift <- rwf(train, h=70, drift = TRUE, level = c(80,95))

ibm_snaive <- snaive(train, h=70, level = c(80,95))

#Compute accuracy metrics with test set

ibm_average_accuracy <- accuracy(ibm_average, test)
ibm_average_accuracy

ibm_naive_accuracy <- accuracy(ibm_naive, test)
ibm_naive_accuracy

ibm_drift_accuracy <- accuracy(ibm_drift, test)
ibm_drift_accuracy

ibm_snaive_accuracy <- accuracy(ibm_snaive, test)
ibm_snaive_accuracy

#Based on these results, the drift method is clearly the best benchmark for this data. The Naive method is a close second.

# d) Check the residuals of your preferred method. Do they resemble white noise?
drift_res <- residuals(ibm_drift)

autoplot(drift_res) + xlab("Time (in days)") + ylab("") + ggtitle("Residuals from drift method")

checkresiduals(ibm_drift)

#Based off of these plots, the mean of the residuals appears to be close to 0 and there is no significant correlation in the residual series. The residuals have constant variance.
#The residuals are normally distributed.

#12. Consider the sales of new one-family houses in the USA, Jan 1973 – Nov 1995 (data set hsales).

# a) Produce some plots of the data in order to become familiar with it.
hsales_plot <- autoplot(hsales, xlab = "Time (years)", ylab = "Price (in thousands US$)", main = "U.S Single Family Home Sales (Jan 1973 - Nov 1995)")

hsales_plot

tsdisplay(hsales, plot.type = "scatter")

# b) Split the hsales data set into a training set and a test set, where the test set is the last two years of data.
hsales
str(hsales)


hsales_train <- window(hsales, end = 1994)
str(hsales_train)

hsales_test<- window(hsales, start = 1994, end = 1996)
str(hsales_test)

# c) Try using various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?
hsales_average <- meanf(hsales_train, h = 23, level = c(80,95))

hsales_naive <- naive(hsales_train, h = 23, level = c(80,95))

hsales_snaive <- snaive(hsales_train, h = 23, level = c(80,95))

hsales_drift <- rwf(hsales_train, drift = TRUE, h = 23, level = c(80,95))


hsales_average_acc <- accuracy(hsales_average, hsales_test)
hsales_average_acc

hsales_naive_acc <- accuracy(hsales_naive, hsales_test)
hsales_naive_acc

hsales_snaive_acc <- accuracy(hsales_snaive, hsales_test)
hsales_snaive_acc

hsales_drift_acc <- accuracy(hsales_drift, hsales_test)
hsales_drift_acc

# d) Check the residuals of your preferred method. Do they resemble white noise?
checkresiduals(hsales_drift)


## Question 3: Section 5.10 Exercises, problem 6
#The gasoline series consists of weekly data for supplies of US finished motor gasoline product, from 2 February 1991 to 20 January 2017.
#The units are in “million barrels per day”. Consider only the data to the end of 2004.

gas <- window(gasoline, end = 2005)

autoplot(gas)

# a) Fit a harmonic regression with trend to the data. 
# Experiment with changing the number of Fourier terms. 
# Plot the observed gasoline and fitted values and comment on what you see.

fit.gas1 <- tslm(gas ~ trend + fourier(gas, K = 2), data = gas)

fit.gas2 <- tslm(gas ~ trend + fourier(gas, K = 4), data = gas)

fit.gas3 <- tslm(gas ~ trend + fourier(gas, K = 6), data = gas)

fit.gas4 <- tslm(gas ~ trend + fourier(gas, K = 8), data = gas)

autoplot(gas, series = "Data") + autolayer(fitted(fit.gas1), series = "Fitted (2 pairs)") + autolayer(fitted(fit.gas2), series = "Fitted (4 pairs)") + autolayer(fitted(fit.gas3), series = "Fitted (6 pairs)") + autolayer(fitted(fit.gas3), series = "Fitted (8 pairs)") + xlab("Year") + ylab("# of Barrels (in millions)") + ggtitle("Plot of Gas w/ Fitted Values")

CV(fit.gas1)
CV(fit.gas2)
CV(fit.gas3)
CV(fit.gas4)

# b) Select the appropriate number of Fourier terms to include by minimising the AICc or CV value.

## The model that minimizes the CV and AICc value is "Fit.gas1" which has two pairs of Fourier terms

# c) Check the residuals of the final model using the checkresiduals() function. 

checkresiduals(fit.gas1)

# d) To forecast using harmonic regression, you will need to generate the future values of the Fourier terms. This can be done as follows.
# Forecast the next year of data.
gas_fc <- forecast(fit.gas1, newdata = data.frame(fourier(gas, K =2, h = 52)))

# e)Plot the forecasts along with the actual data for 2005. What do you find?

gas.2005 <-window(gasoline, start = 1991, end = 2006)

autoplot(gasoline, series = "Actual Data") + autolayer(gas_fc, series = "Forecast", showgap = TRUE) + xlab("Time") + ylab("# of Barrels (in millions)") + ggtitle("2005 Gasoline Data vs. Forecast")

## Question 4: Section 6.9 Exercises, problem 6
#We will use the bricksq data (Australian quarterly clay brick production. 1956–1994) for this exercise.

autoplot(bricksq)


# a) Use an STL decomposition to calculate the trend-cycle and seasonal indices. (Experiment with having fixed or changing seasonality.)

STL_fixed <- bricksq %>%
  stl(bricksq, s.window = "periodic", robust = TRUE) %>%
  autoplot() + ggtitle("STL Decomposition w/ Fixed Seasonality")

STL_fixed

STL_changing <-  bricksq %>%
  stl(bricksq, s.window = 13) %>%
  autoplot() + ggtitle("STL Decomposition w/ Changing Seasonality")

STL_changing

# b) Compute and plot the seasonally adjusted data.

plot(bricksq, main = "Bricks Data vs Trend Data", ylab = "# of Clay Bricks")
fit <- stl(bricksq, s.window = 13, robust = FALSE)
lines(trendcycle(fit),col="red")

bricks_seas <- seasadj(fit)
autoplot(bricks_seas) + ylab("# of Bricks") + ggtitle("Seasonally Adjusted Data")


# c) Use a naïve method to produce forecasts of the seasonally adjusted data.

bricks_naive <- naive(bricks_seas, h = 4, level = c(80,95))
bricks_naive

autoplot(bricksq) + autolayer(bricks_naive) + ylab("# of Bricks") + ggtitle("Naive Forecasts for Seasonally Adjusted Data")

# d) Use stlf() to reseasonalise the results, giving forecasts for the original data.

bricks_reseas <- stlf(bricks_seas, h = 4, s.window = 13, t.window = NULL, robust = FALSE)

bricks_reseas_naive <- naive(bricks_reseas, h = 4, level = c(80,95))   

autoplot(bricksq) + autolayer(bricks_reseas_naive, showgap = FALSE) + ylab("# of Bricks") + ggtitle("Naive Forecasts for Reseasonalized Data")
# e) Do the residuals look uncorrelated?

autoplot(residuals(bricks_reseas_naive))

# f) Repeat with a robust STL decomposition. Does it make much difference?
fit1 <- stl(bricksq, s.window = 13, robust = TRUE)

bricks_seas1 <- seasadj(fit1)

bricks_naive1 <- naive(bricks_seas1, h = 4, level = c(80,95))
bricks_naive1

autoplot(bricksq) + autolayer(bricks_naive1) + ylab("# of Bricks") + ggtitle("Naive Forecasts for Seasonally Adjusted Data")


bricks_reseas1 <- stlf(bricks_seas1, h = 4, s.window = 13, t.window = NULL, robust = TRUE)

bricks_reseas_naive1 <- naive(bricks_reseas1, h = 4, level = c(80,95))   

autoplot(bricksq) + autolayer(bricks_reseas_naive1, showgap = FALSE) + ylab("# of Bricks") + ggtitle("Naive Forecasts for Reseasonalized Data")

# g) Compare forecasts from stlf() with those from snaive(), using a test set comprising the last 2 years of data. Which is better?
bricks_test <- window(bricksq, start = 1993)

bricks_train <- window(bricksq, end = 1993)

bricks_snaive <- snaive(bricks_train, h = 8, level = c(80,95))

bricks_stlf <- stlf(bricks_train, h = 8, level = c(80,95))

##Compare accuracy metrics of two models

accuracy(bricks_snaive, bricks_test)

accuracy(bricks_stlf, bricks_test)

## Based on these results, it appears that the stlf model is by far the better model

## Question 5: Section 7.8 Exercises, problem 11
# For this exercise use data set visitors, the monthly Australian short-term overseas visitors data, May 1985–April 2005.

# a) Make a time plot of your data and describe the main features of the series.

autoplot(visitors) + ylab("# of Visitors") + ggtitle("Monthly Australian Overseas Visitors (May 1985 - April 2005")

## There appears to be a strong seasonal component and positive linear trend. There appears to be an outlier around 2002 with a bigger drop in visitors.

# b) Split your data into a training set and a test set comprising the last two years of available data. Forecast the test set using Holt-Winters’ multiplicative method.

visitors_train <- window(visitors, end = 2003)

visitors_test <- window(visitors, start = 2003)

visitors_hw <- hw(visitors_train, h = 8, seasonal = "multiplicative", level = c(80,95))

autoplot(visitors_train) + autolayer(visitors_hw) + ylab("# of Visitors") + ggtitle("Multiplicative Holt Winters' Forecast")

## Compute accuracy metrics
accuracy(visitors_hw, visitors_test)


# c) Why is multiplicative seasonality necessary here?

## Multiplicative seasonality was neccesary in this case becuase the seasonal variation in the data increased as the level of the series increased. 
## The multiplicative method is better at accounting for these large increases.


# d) Forecast the two-year test set using each of the following methods:
## an ETS model; an additive ETS model applied to a Box-Cox transformed series;
## a seasonal naïve method; an STL decomposition applied to the Box-Cox transformed data 
## followed by an ETS model applied to the seasonally adjusted (transformed) data.

## ETS Model
visitors_ets <- ets(visitors_train, model = "ZZZ", damped = NULL, alpha = NULL, beta = NULL, gamma = NULL, phi = NULL)

visitors_ets_forecast <- forecast(visitors_ets, h = 8, level = c(80,05))

accuracy(visitors_ets_forecast, visitors_test)

## Additive ETS Model w/ Box Cox Transformation
visitors_test_bc <- BoxCox(visitors_test, lambda = "auto")


visitors_train_bc <- BoxCox(visitors_train, lambda = "auto")
BoxCox.lambda(visitors_train)

visitors_add_ets <- ets(visitors_train_bc, model = "ZZZ", alpha = NULL, beta = NULL, gamma = NULL, phi = NULL, damped = NULL, biasadj = TRUE, lambda = .3596984, additive.only = TRUE )
visitors_add_ets

visitors_add_ets_forecast <- forecast(visitors_add_ets, h = 8, level = c(80,95))

accuracy(visitors_add_ets_forecast, visitors_test_bc)

## Seasonal Naive Model

visitors_snaive <- snaive(visitors_train, h = 8, level = c(80,95))

autoplot(visitors_train) + autolayer(visitors_snaive)

accuracy(visitors_snaive, visitors_test)

## STL Decomposition w/ Box Cox Transformation
visitors_stl <- stl(visitors_train_bc, s.window = 13, robust = FALSE)

visitors_stl_forecast <- forecast(visitors_stl, h = 8, level = c(80,95))

autoplot(visitors_train_bc) + autolayer(visitors_stl_forecast)

accuracy(visitors_stl_forecast, visitors_test_bc)

## ETS Model w/ Seasonal Adjustment
visitors_train_decomp <- decompose(visitors_train, type = "multiplicative")
visitors_test_decomp <- decompose(visitors_test, type = "multiplicative")

visitors_seasadj <- seasadj(visitors_train_decomp)
visitors_seasadj_test <- seasadj(visitors_test_decomp)

visitors_seasadj_ets <- ets(visitors_seasadj, model = "ZZZ", damped = NULL, alpha = NULL, beta = NULL, gamma = NULL, phi = NULL, lambda = NULL)

visitors_seasadj_forecast <- forecast(visitors_seasadj_ets, h = 8, level = c(80,95))

autoplot(visitors_seasadj) + autolayer(visitors_seasadj_forecast)

accuracy(visitors_seasadj_forecast, visitors_seasadj_test)

# e) Which method gives the best forecasts? Does it pass the residual tests?

## The STL model with Box Cox transformation was the best model based on the accuracy metrics. The `stl` function removes the trend from the signal very well. The residuals look fairly random.

checkresiduals(visitors_stl_forecast)

##The residuals look like white noise and they are normally distributed with a meman around zero. They pass the residuals test.

# f) Compare the same four methods using time series cross-validation with the tsCV() function instead of using a training and test set. Do you come to the same conclusions?

tsCV(visitors, visitors_seasadj_forecast, h = 8)

tsCV(visitors, visitors_add_ets_forecast, h=8)

tsCV(visitors, visitors_ets_forecast, h =8)

tsCV(visitors, visitors_snaive)
  

