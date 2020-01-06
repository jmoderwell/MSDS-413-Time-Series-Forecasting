library(fpp2)
library(ggplot2)
library(tseries)
library(xts)
library(readxl)

setwd("C:/Users/John/Desktop/MSDS 413")

###  Chapter 8

### Question 7

# Consider wmurders, the number of women murdered each year (per 100,000 standard population) in the United States.
# By studying appropriate graphs of the series in R, find an appropriate ARIMA(p,d,q) model for these data.

ggtsdisplay(wmurders)
kpss.test(wmurders)

autoplot(wmurders)

autoplot(diff(wmurders))

ndiffs(wmurders)

# Based on these plots, the data needs to be differenced twice to make it stationary.

autoplot(diff(wmurders, differences = 2))

diff(wmurders, differences = 2) %>% ggtsdisplay()

# PACF is decaying. There are significant spikes at lag 1, and 2 in the ACF, but none beyond lag 2. 
# I'm going to model the data by ARIMA(0, 2, 2).


# Should you include a constant in the model? Explain.

# ARIMA model of the data includes twice differencing. 
# If there is a constant in the model, twice integrated contant will yield quadratic trend


# Write this model in terms of the backshift operator.

# (1 - B)^2*yt = (1 + theta1*B + theta2*B^2)*et


# Fit the model using R and examine the residuals. Is the model satisfactory?
wmurders_arima <- Arima(wmurders, order = c(0, 2, 2))

checkresiduals(wmurders_arima)

# The residuals are white noise but they are not normally distributed. 
# I would still say they are satisfactory


# Forecast three times ahead. Check your forecasts by hand to make sure that you know how they have been calculated.
wmurders_arima_forecast <- forecast(wmurders_arima, h = 3)

# Create a plot of the series with forecasts and prediction intervals for the next three periods shown.

autoplot(wmurders, series = "Data") + autolayer(wmurders_arima_forecast$mean, series = "Forecasts")

# Does auto.arima() give the same model you have chosen? If not, which model do you think is better?

wmurders_auto <- auto.arima(wmurders)

checkresiduals(wmurders_auto)

wmurders_auto_forecast <- forecast(wmurders_auto, h =3)

autoplot(wmurders, series = "Data") + autolayer(wmurders_auto_forecast$mean, series = "Forecast")

# auto.arima() gave a (1,2,1) model instead of (0,2,2)

accuracy(wmurders_arima_forecast)
accuracy(wmurders_auto_forecast)
 
# The ARIMA (1,2,1) model has a lower RMSE score so it is the preferred model.

### Question 9

# For the usgdp series:
autoplot(usgdp)
ggtsdisplay(usgdp)

# if necessary, find a suitable Box-Cox transformation for the data;

BoxCox.lambda(usgdp, method = c("loglik"))

# Lambda = .2

# fit a suitable ARIMA model to the transformed data using auto.arima();
usgdp_auto <- auto.arima(usgdp, lambda = .2)
checkresiduals(usgdp_auto)


# try some other plausible models by experimenting with the orders chosen;
usgdp_Arima1.2.1 <- Arima(usgdp, lambda = .2, order = c(1,2,1))
checkresiduals(usgdp_Arima1.2.1)

usgdp_Arima1.1.1 <- Arima(usgdp, lambda = .2, order = c(1,1,1))
checkresiduals(usgdp_Arima1.1.1)

#choose what you think is the best model and check the residual diagnostics;
final_model <- auto.arima(usgdp, lambda = .2)

checkresiduals(final_model)

# produce forecasts of your fitted model. Do the forecasts look reasonable?
fit1 <- forecast(final_model, h = 10)

autoplot(usgdp)+autolayer(fit1) + ylab("US GDP ($)")

# The forecasts look reasonable

# compare the results with what you would obtain using ets() (with no transformation).

ets_model <- ets(usgdp, model = "ZZZ")

fit2 <- forecast(ets_model, h = 10)

autoplot(usgdp) + autolayer(fit2)

accuracy(ets_model)
accuracy(final_model)

# The auto.arima() model was more accurate

###  Question 10.

# Consider austourists, the quarterly number of international tourists to Australia for the period 1999-2010.

# Describe the time plot.
ggtsdisplay(austourists)

## There is a positive trend and strong seasonality. 
## The seasonality increases over time.

# What can you learn from the ACF graph?
ggAcf(austourists)

# Autocorrelations slowly decreasing
#Very strong seasonality - 4, 8, 12, 16 seen in the components. Reducing strength of the contributions as expected.

# What can you learn from the PACF graph?
ggPacf(austourists)

#Strong seasonal components at lag = 4. Perhaps a strong non-seasonal component at lag=5.

#Produce plots of the seasonally differenced data  
#(1???B^4)Yt. What model do these graphs suggest?
ndiffs(austourists)

aust_adj <- diff(austourists, lag = 4)

ggtsdisplay(aust_adj)

# the seasonally differenced data need at least one more differencing to make it stationary.

ggtsdisplay(aust_adj1)
# For the values at the lags of multiple of 4, there are just significant spikes at lag 4. It is same for ACF and PACF plots. 
# The order of seasonal ARIMA model can be (1, 1, 0)[4] or (0, 1, 1)[4]. I'll choose (1, 1, 0)[4] order. 

aust_arima <- Arima(austourists, order = c(1,1,0))

aust_arima_forecast <- forecast(aust_arima, h = 10)


autoplot(austourists, series = "Data") + autolayer(aust_arima_forecast$mean, series = "Forecasts")

#Does auto.arima() give the same model that you chose? If not, which model do you think is better?

aust_auto <- auto.arima(austourists, stepwise = FALSE, approximation = FALSE)

aust_auto_forecast <- forecast(aust_auto, h =5)

autoplot(austourists, series = "Data") + autolayer(aust_auto_forecast, series = "Forecasts")

checkresiduals(aust_auto)

# auto.arima() gave a different model than the first model It is a (1,0,0) model and appears much more suitable for the data.

# Write the model in terms of the backshift operator, then without using the backshift operator.

## (1 - phi1 x B)(1 - PHI1 x B^4)(1 - B^4)y_t = (1 + THETA1 x B^4)e_t


#18
#Select a time series from Quandl. Then copy its short URL and import the data using
library(Quandl)
y <- Quandl("FED/RXI_US_N_A_NZ", api_key="5uU2sssTU3c9WvE1dcUT", type="ts")

#(Replace each ????? with the appropriate values.)

#Plot graphs of the data, and try to identify an appropriate ARIMA model.
ggtsdisplay(y)
ndiffs(y)

ggtsdisplay(diff(y))

y_arima <- Arima(y, order = c(0,1,1))
checkresiduals(y_arima)

y_auto <- auto.arima(y)
checkresiduals(y_auto)


#Do residual diagnostic checking of your ARIMA model. Are the residuals white noise?
checkresiduals(y_arima)
checkresiduals(y_auto)
# The residuals are like white noise.

#Use your chosen ARIMA model to forecast the next four years.
fit3 <- forecast(y_arima, h = 4)

autoplot(y, series = "Data") + autolayer(fit3, series = "Forecast")

#Now try to identify an appropriate ETS model.
y_ets <- ets(y, model = "ZZZ")

# Do residual diagnostic checking of your ETS model. Are the residuals white noise?
checkresiduals(y_ets)
# The residuals are like white noise.

# Use your chosen ETS model to forecast the next four years.
fit4 <- forecast(y_ets, h = 4)

autoplot(y) + autolayer(fit4)

#Which of the two models do you prefer?
accuracy(y_arima)
accuracy(y_ets)

# Based on the accuracy metrics, the ARIMA model is preferred.

### Section 9.7

#1
# Consider monthly sales and advertising data for an automotive parts company (data set advert).
# Plot the data using autoplot. Why is it useful to set facets=TRUE?
autoplot(advert, facets = TRUE)

# Setting Facets = TRUE allows us to visualize trends in advertising expenditure data and sales volume at the same time.

# Fit a standard regression model  
advert_tslm <- tslm(advert[,"sales"]~advert[,"advert"])

# Show that the residuals have significant autocorrelation.
checkresiduals(advert_lm)

# There is significant autocorrelation at lags 1 and 2

# What difference does it make you use the Arima function instead:
  
advert_arima <- Arima(advert[,"sales"], xreg=advert[,"advert"],
        order=c(0,0,0))

checkresiduals(advert_arima)

# Refit the model using auto.arima(). How much difference does the error model make to the estimated parameters? What ARIMA model for the errors is selected?
advert_auto <- auto.arima(advert[,"sales"], xreg = advert[,"advert"])

# Check the residuals of the fitted model.
checkresiduals(advert_auto)

autoplot(advert[, "sales"], series = "Data") +
  geom_line(color = "red", size = 1) +
  autolayer(advert_auto$fitted, size = 1, series = "Dynamic Regression fitted values") +
  autolayer(advert_lm$fitted.values, size = 1, series = "Linear Regression fitted values") +
  ylab("Sales volume")

# The residuals are like white noise

# Assuming the advertising budget for the next six months is exactly 10 units per month, produce and plot sales forecasts with prediction intervals for the next six months.

advert_fit <- forecast(advert_auto, xreg = rep(10, 6),  h = 6, level = c(80,85))

autoplot(advert_fit)

#2
# This exercise uses data set huron giving the level of Lake Huron from 1875-1972.
 
# Fit a piecewise linear trend model to the Lake Huron data with a knot at 1920 and an ARMA error structure.
# Forecast the level for the next 30 years.
x1 <- 1:length(huron)
x2 <- pmax(0, x1-44)
x3 <- pmax(0, x1-46)
t  <- seq(x1+10)

fit <- auto.arima(huron, xreg=cbind(x1,x2,x3))
fc <- forecast(fit, xreg=cbind(max(x1)+seq(10), max(x2)+seq(10), max(x3)+seq(10)), h = 30)
b0 <- coef(fit)["intercept"]
b1 <- coef(fit)["x1"]
b2 <- coef(fit)["x2"]
b3 <- coef(fit)["x3"]
trend <- ts(b0 + b1*t + b2*pmax(0,t-44) + b3*pmax(0,t-46),
            start=start(huron))

plot(fc, main="Piecewise linear trend with AR(1) errors")
lines(trend, col='red')

### 3
 
# This exercise concerns motel: the total monthly takings from accommodation and the total room nights occupied at hotels, motels, and guest houses in Victoria, Australia, between January 1980 and June 1995. Total monthly takings are in thousands of Australian dollars; total room nights occupied are in thousands.
summary(motel)
autoplot(motel, facets = TRUE)

# Use the data to calculate the average cost of a night's accommodation in Victoria each month.

avg_cost <- motel[, "Takings"] / motel[, "Roomnights"]

autoplot(avg_cost)

# Average cost of a night's accomodation in Victoria increased rapidly until 1990, and then just oscillated without increasing until 1995.


# Use cpimel to estimate the monthly CPI.
tsdisplay(cpimel)

monthly_CPI <- auto.arima(motel[,"CPI"], lambda = 0)

autoplot(motel[, "CPI"]) + autolayer(monthly_CPI$fitted) + ylab("CPI")

# Produce time series plots of both variables and explain why logarithms of both variables need to be taken before fitting any models.
autoplot(avg_cost)
autoplot(monthly_CPI$fitted)

# Log transformations can make the variations constant as time progresses. It can also be used to linearize non-linear data. 
# This allows us to build linear regression models. 
# By setting lambda = 0 in the model, this is the same as applying a log transformation. The transformations are reversed for forecasts.

# Fit an appropriate regression model with ARIMA errors. Explain your reasoning in arriving at the final model.
motel_reg <- auto.arima(avg_cost, xreg = monthly_CPI$fitted, lambda = 0, stepwise = FALSE, approximation = FALSE)

checkresiduals(motel_reg)

# The residuals are like white noise.

# Forecast the average price per room for the next twelve months using your fitted model. (Hint: You will need to produce forecasts of the CPI figures first.)
formatter1000 <- function(x){ 
  scales::dollar(x*1000)
}

CPI_forecasts <- forecast(monthly_CPI, h = 12)
 
avg_cost_forecast <- forecast(motel_reg, xreg = CPI_forecasts$mean, h = 12)


autoplot(motel_reg$fitted, series = "Data") + autolayer(avg_cost_forecast$mean, series = "Forecasts") + ylab("Average Cost (AU$)") + ggtitle("Forecasts for Average Cost of a night's accomadation") + scale_y_continuous(labels = formatter1000)

# The forecasts show a slight increase in average cost (from approx $83 at end of 1995 to approx $91 at beginning of 1996) which oscillates as the year goes on.


#4
 
# We fitted a harmonic regression model to part of the gasoline series in Exercise 6 in Section 5.10. We will now revisit this model, and extend it to include more data and ARMA errors.
str(gasoline)
head(gasoline)
gasoline
tsdisplay(gasoline)
plot(stl(gasoline, s.window = "periodic"))

autoplot(gasoline) + ylab("US Gasoline Supply (1991-2017 (millions of barrels)")

# Using tslm(), fit a harmonic regression with a piecewise linear time trend to the full gasoline series. Select the position of the knots in the trend and the appropriate number of Fourier terms to include by minimising the AICc or CV value.
gas_t <- round(time(gasoline), digits = 1)
gas_t.knot1 <- 2007.5
gas_t.knot2 <- 2012.5

gas_t.x2 <- ts(pmax(0, gas_t - gas_t.knot1), start = gas_t[1], frequency = 365.25/7)
gas_t.x3 <- ts(pmax(0, gas_t - gas_t.knot2), start = gas_t[1], frequency = 365.25/7)

AICc <- Inf
K_min_AIC <- 0

for(num in c(1:26))
{
    gas_fit <- tslm(gasoline ~ trend + gas_t.x2 + gas_t.x3 + fourier(gasoline, K = num))
    AICc_value <- CV(gas_fit)["AICc"]
    
if(AICc > AICc_value){
  AICc <- AICc_value}

else{
  K_min_AIC <- num
  break
}
    }


# The number of Fourier terms that minimizes the AIC is 11

gas_tslm <- tslm(gasoline ~ trend + gas_t.x2 + gas_t.x3 + fourier(gasoline, K = 11))

autoplot(gasoline, series = "Data") + autolayer(gas_tslm$fitted.values, series = "Fitted Values")

gas_tslm_forecast <- forecast(gas_tslm$fitted.values, h = 30)

autoplot(gasoline, series = "Data") + autolayer(gas_tslm_forecast$mean, series = "Forecasts")


Fourier_gas <- fourier(gasoline, K = 11)

xtrain <- cbind(gas_t.x2, gas_t.x3, Fourier_gas)

# Now refit the model using auto.arima() to allow for correlated errors, keeping the same predictor variables as you used with tslm().
gas_arima <- auto.arima(gasoline, xreg = xtrain)


# Check the residuals of the final model using the checkresiduals() function. Do they look sufficiently like white noise to continue? If not, try modifying your model, or removing the first few years of data.
checkresiduals(gas_arima)
# Once you have a model with white noise residuals, produce forecasts for the next year.
 
gas_arima_forecast <- forecast(gas_arima, xreg = xtrain)

#5
# What sort of ARIMA model is identified for ??t?
qplot(elecdaily[,"Temperature"], elecdaily[,"Demand"]) +
  xlab("Temperature") + ylab("Demand")

autoplot(elecdaily, facets = TRUE)

xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
checkresiduals(fit)

# An ARIMA (2,1,2) model was identified for nt

# Forecast one day ahead
forecast(fit, xreg = cbind(26, 26^2, 1))

fcast <- forecast(fit,
                  xreg = cbind(rep(26,14), rep(26^2,14),
                               c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))
autoplot(fcast) + ylab("Electicity demand (GW)")

 
#6
# For the retail time series considered in earlier chapters:
retaildata <- read_excel("retail.xlsx", skip = 1)
str(retaildata)
head(retaildata)

retail_clothing_ts <- ts(retaildata[,"A3349399C"],
                         frequency=12, start=c(1982,4))

BoxCox.lambda(retail_clothing_ts, method = "loglik")

min_AIC_retail <- Inf
K_min_AIC_retail <- 0

for(num in c(1:6))
{
  retail_tslm <- tslm(retail_clothing_ts ~ trend +  fourier(retail_clothing_ts, K = num), lambda = .3)
  
  AIC_retail <- CV(retail_tslm)["AIC"]
  
if(AIC_retail < min_AIC_retail){
    min_AIC_retail <- AIC_retail
    K_min_AIC_retail <- num
  }
}
  
# There should be 6 fourier terms

# Develop an appropriate dynamic regression model with Fourier terms for the seasonality. Use the AIC to select the number of Fourier terms to include in the model. (You will probably need to use the same Box-Cox transformation you identified previously.)
retail_tslm_1 <- tslm(
  retail_clothing_ts ~ trend + fourier(retail_clothing_ts, K = 6),
  lambda = .3)

autoplot(retail_clothing_ts, series = "Data") +
  autolayer(retail_tslm_1$fitted.values, series = "Fitted Values")


retail_tslm_forecast <- forecast(retail_tslm_1, h = 12)

  # Check the residuals of the fitted model. Does the residual series look like white noise?
checkresiduals(retail_tslm_1)

# The residuals do not look like white noise


# Build dynamic regression model
Fourier_retail <- fourier(retail_clothing_ts, K = 6)

time_retail <- time(retail_clothing_ts)

retail_auto <- auto.arima(retail_clothing_ts, lambda = .3, xreg = cbind(Fourier_retail, time_retail))

retail_auto_forecast <- forecast(retail_auto, h = 12, xreg = cbind(Fourier_retail, time_retail))

checkresiduals(retail_auto)
# Compare the forecasts with those you obtained earlier using alternative models.
 
accuracy(retail_auto_forecast)

accuracy(retail_tslm_1)

# The auto.arima() (3,0,1) model is preferred.
 