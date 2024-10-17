## Names:
# - Elios KARAM
# - David Sleiman
# - Robert David PETREL VARGAS

library(quantmod)
library(zoo)
library(tseries)
library(moments)
library(forecast)
library(aTSA)
library(MLmetrics)
library(ggplot2)
library(psych)

################################################################################
# 1 Data Import and Preliminary Analysis
################################################################################
#1.1 Import and preliminary cleaning of the Data:
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2024-08-31")

getSymbols("EURUSD=X", src = "yahoo", from = start_date, to = end_date)
getSymbols("GC=F", src = "yahoo", from = start_date, to = end_date)

eurusd_zoo <- zoo(`EURUSD=X`[, "EURUSD=X.Close"], index(`EURUSD=X`))
goldusd_zoo <- zoo(`GC=F`[, "GC=F.Close"], index(`GC=F`))

dates1 <- zoo(,seq(from = min(index(eurusd_zoo)), to =  max(index(eurusd_zoo)), by = 1))
dates2 <- zoo(,seq(from = min(index(goldusd_zoo)), to =  max(index(goldusd_zoo)), by = 1))
eurusd_zoo <- merge(dates1, eurusd_zoo)
goldusd_zoo <- merge(dates2, goldusd_zoo)

eurousd_clean <- na.locf(eurusd_zoo)
goldusd_clean <- na.locf(goldusd_zoo)

#Clean unused variables
rm(dates1,dates2,eurusd_zoo,goldusd_zoo,`EURUSD=X`,`GC=F`)

################################################################################
#1.2 Plot the Data:

## EUR/USD
#Plot 1
chartSeries(eurousd_clean, type = "auto", 
            theme = chartTheme("white"), 
            name = "Euro/USD Exchange Rate", 
            TA = NULL) 
addBBands()


title(main = "Euro/USD Exchange Rate with Bollinger Bands", col.main = "blue", font.main = 4)
mtext("Date", side = 1, line = 3) 
mtext("Exchange Rate (USD per EUR)", side = 2, line = 3) 
grid(col = "gray") 

#Plot 2
ggplot(eurousd_clean) + 
  geom_path(mapping = aes(x= Index , y = `EURUSD=X.Close`)) +
  labs(x = "Time",  
       y = "Daily close value", 
       title = "Daily closing rate for EUR/USD", 
       caption = "From Jan 1st, 2022 to Aug 31st, 2024",
       ylab("EUR/USD"))

#Plot 3 Seems more stationary but apparently different variance over time (heteroscedasticity)
ggplot(diff(eurousd_clean)) + 
  geom_path(mapping = aes(x= Index , y = `EURUSD=X.Close`)) +
  labs(x = "Time",  
       y = "First differentiation of \nDaily close value", 
       title = "Differentitation of\nDaily closing rate for EUR/USD", 
       caption = "From Jan 1st, 2022 to Aug 31st, 2024",
       ylab("EUR/USD"))


##For GOL/USD
# Plot 1
chartSeries(goldusd_clean, type = "auto", 
            theme = chartTheme("white"), 
            name = "Gold/USD Exchange Rate", 
            TA = NULL)


addBBands()


title(main = "Gold/USD Exchange Rate with Bollinger Bands", col.main = "darkgoldenrod", font.main = 4)
mtext("Date", side = 1, line = 3) # X-axis label
mtext("Exchange Rate (USD per oz of Gold)", side = 2, line = 3) # Y-axis label
grid(col = "gray") # Adding gridlines to the chart

#Plot 2
ggplot(goldusd_clean) + 
  geom_path(mapping = aes(x= Index , y = `GC=F.Close`)) +
  labs(x = "Time",  
       y = "Daily close value", 
       title = "Daily closing rate for Gold/USD", 
       caption = "From Jan 1st, 2022 to Aug 31st, 2024",
       ylab("Gold/USD"))
#Plot 3 seem stationary with heteroscedasticity 
ggplot(diff(goldusd_clean)) + 
  geom_path(mapping = aes(x= Index , y = `GC=F.Close`)) +
  labs(x = "Time",  
       y = "First differentiation of \nDaily close value", 
       title = "Differentitation of \nDaily closing rate for Gold/USD", 
       caption = "From Jan 1st, 2022 to Aug 31st, 2024",
       ylab("Gold/USD"))

################################################################################
#1.3 Descriptive Analysis:
sum_statistics_eurusd <- describe(eurousd_clean)
sum_statistics_golsdusd <- describe(goldusd_clean)

ggplot(eurousd_clean) + 
  geom_boxplot(mapping = aes(x= "EUR/USD",y = `EURUSD=X.Close`)) +
  labs(x = "EUR/USD",
       y = "Daily close value", 
       title = "Box plot Daily closing rate for EUR/USD", 
       caption = "From Jan 1st, 2022 to Aug 31st, 2024")

ggplot(diff(eurousd_clean)) + 
  geom_boxplot(mapping = aes(x= "EUR/USD", y = `EURUSD=X.Close`)) +
  labs(x = "EUR/USD",
       y = "First differentiation of \n log Daily close value", 
       title = "Box plot transformation of Daily closing rate for EUR/USD", 
       caption = "From Jan 1st, 2022 to Aug 31st, 2024")


ggplot(goldusd_clean) + 
  geom_boxplot(mapping = aes(x= "Gold/USD",y = `GC=F.Close`)) +
  labs(x = "Gold/USD",
       y = "Daily close value", 
       title = "Box plot Daily closing rate for Gold/USD", 
       caption = "From Jan 1st, 2022 to Aug 31st, 2024")

ggplot(diff(goldusd_clean)) + 
  geom_boxplot(mapping = aes(x= "Gold/USD",y = `GC=F.Close`)) +
  labs(x = "Gold/USD",
       y = "First differentiation of \n log Daily close value", 
       title = "Box plot transformation of Daily closing rate for Gold/USD", 
       caption = "From Jan 1st, 2022 to Aug 31st, 2024")
################################################################################
#1.4 Stationarity Check:

##EUR/USD
#IF p-value is less than alpha critical value, reject H_0: X is not a stationary series https://www.r-bloggers.com/2022/06/augmented-dickey-fuller-test-in-r/
# At alpha 0.05, p value of 0.3528 does not reject H_0, ie, series is not stationary
tseries::adf.test(eurousd_clean, alternative = "stationary") 

#IF p-value is less than alpha critical value, reject H_0: X is a trend stationary series https://www.statology.org/kpss-test-in-r/
# At alpha 0.05, p value of 0.01  does reject H_0, ie,  the series is not trend stationary
tseries::kpss.test(eurousd_clean, null = "T")

#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of < 2.2e-16 does reject H_0, the series is autocorrelated, appropriate to differentiate
Box.test(eurousd_clean)

acf(eurousd_clean)
pacf(eurousd_clean)

##EUR/USD first differentiation
eurousd_diff <- diff(eurousd_clean, differences = 1)
#IF p-value is less than alpha critical value, reject H_0: X is not a stationary series https://www.r-bloggers.com/2022/06/augmented-dickey-fuller-test-in-r/
# At alpha 0.05, p value of 0.01 does reject H_0, ie, series is stationary
tseries::adf.test(eurousd_diff, alternative = "stationary") 

#IF p-value is less than alpha critical value, reject H_0: X is a trend stationary series https://www.statology.org/kpss-test-in-r/
# At alpha 0.05, p value of 0.1  does not reject H_0, ie, the series is trend stationary
tseries::kpss.test(eurousd_diff, null = "T")

#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of 0.4836 does not reject H_0, the series is autocorrelated anymore, 
Box.test(eurousd_diff)

acf(eurousd_diff)
pacf(eurousd_diff)

##Gold/USD
#IF p-value is less than alpha critical value, reject H_0: X is not a stationary series https://www.r-bloggers.com/2022/06/augmented-dickey-fuller-test-in-r/
# At alpha 0.05, p value of 0.6855 does not reject H_0, ie, series is not stationary
tseries::adf.test(goldusd_clean, alternative = "stationary") 

#IF p-value is less than alpha critical value, reject H_0: X is a trend stationary series https://www.statology.org/kpss-test-in-r/
# At alpha 0.05, p value of 0.01  does reject H_0, ie, the series is not trend stationary
tseries::kpss.test(goldusd_clean, null = "T")

#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of < 2.2e-16 does reject H_0, the series is autocorrelated, appropriate to differentiate
Box.test(goldusd_clean)

acf(goldusd_clean)
pacf(goldusd_clean)

##Gold/USD first differentiation
goldusd_diff <- diff(goldusd_clean, differences = 1)
#IF p-value is less than alpha critical value, reject H_0: X is not a stationary series https://www.r-bloggers.com/2022/06/augmented-dickey-fuller-test-in-r/
# At alpha 0.05, p value of 0.01 does reject H_0, ie, series is stationary
tseries::adf.test(goldusd_diff, alternative = "stationary") 

#IF p-value is less than alpha critical value, reject H_0: X is a trend stationary series https://www.statology.org/kpss-test-in-r/
# At alpha 0.05, p value of 0.1  does not reject H_0, ie, series is trend stationary
tseries::kpss.test(goldusd_diff, null = "T")

#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of 0.0477 does reject H_0, the series is autocorrelated, appropriate to differentiate
Box.test(goldusd_diff)

acf(goldusd_diff)
pacf(goldusd_diff)

################################################################################
# 2. Modeling and Analysis
################################################################################
#2.1 Autoregressive and Moving Average Models:

par(mfrow = c(2, 2))
acf(eurousd_diff, main= "ACF of first differentiation \nof EUR/USD")
#Indicates not good to be model by an AR model
pacf(eurousd_diff, main= "PACF of first differentiation \nof EUR/USD")

acf(goldusd_diff, main= "ACF of first differentiation \nof GOLD/USD")
#Indicates not good to be model by an AR model
pacf(goldusd_diff, main= "ACF of first differentiation \nof GOLD/USD")
par(mfrow = c(1, 1))


#Not fitted by a AR model as shown by the BOX.test it is not autocorrelated avant not best sol
eurousd_ARmodel <- ar(eurousd_diff$`EURUSD=X.Close`, method = "mle")
#comptibilite
eurousd_ARmodel <- arima(eurousd_diff, order = c(1,0,0))  #If decided to use a AR(1) model

#As box.test show the data is still correlated a ARmodel of order 1 is selected by AIC (The one with minimum AIC)
goldusd_ARmodel <- ar(goldusd_diff$`GC=F.Close`, method = "mle")
#Approximatively white noise so it could be a model to explain it
acf(goldusd_ARmodel$res[-1])
goldusd_ARmodel <- arima(goldusd_diff, order = c(goldusd_ARmodel$order,0,0))

#MA models

AIC_MA_eurousd <- rep(0,30)
eurousd_MAmodel <- arima(eurousd_diff, order = c(0,0,1))
MA_order_eurousd <- 1

for (t in 1:length(AIC_MA_eurousd)) {
  fitted_model <- arima(eurousd_diff, order = c(0,0,t))
  AIC_MA_eurousd[t] <- AIC(fitted_model)
  if (AIC_MA_eurousd[t] == min(AIC_MA_eurousd[1:t])) {
    eurousd_MAmodel <- fitted_model
    MA_order_eurousd <- t
  }
}

AIC_MA_eurousd <- as.data.frame(AIC_MA_eurousd)

ggplot(AIC_MA_eurousd) +
  geom_point(aes(x= index(AIC_MA_eurousd), y = AIC_MA_eurousd))


AIC_MA_goldusd <- rep(0,30)
goldusd_MAmodel <- arima(goldusd_diff, order = c(0,0,1))
MA_order_goldusd <- 1

for (t in 1:length(AIC_MA_goldusd)) {
  fitted_model <- arima(goldusd_diff, order = c(0,0,t))
  AIC_MA_goldusd[t] <- AIC(fitted_model)
  if (AIC_MA_goldusd[t] == min(AIC_MA_goldusd[1:t])) {
    goldusd_MAmodel <- fitted_model
    MA_order_goldusd <- t
  }
}
AIC_MA_goldusd <- as.data.frame(AIC_MA_goldusd)

ggplot(AIC_MA_goldusd) +
  geom_point(aes(x= index(AIC_MA_goldusd), y = AIC_MA_goldusd))

print(MA_order_eurousd)
print(MA_order_goldusd)

  #ARMA models 
eurousd_ARMAmodel <- arima(eurousd_diff, order = c(1,0,MA_order_eurousd))
goldusd_ARMAmodel <- arima(goldusd_diff, order = c(1,0,MA_order_goldusd))

#Comparison of AR, MA, ARMA models based on AIC
AIC(eurousd_ARmodel) #Not the best model AIC -7710.801
AIC(eurousd_MAmodel) #Best model from AIC -7710.832
AIC(eurousd_ARMAmodel) # AIC -7709.116

AIC(goldusd_ARmodel) # AIC 8020.036
AIC(goldusd_MAmodel) # Best model from AIC 8019.895
AIC(goldusd_ARMAmodel) # AIC 8021.661

################################################################################
#2.2 Residual Analysis

#For EUR/USD
##AR model
ggplot(eurousd_ARmodel$residuals) +
  geom_point(aes(x = seq(1,length(eurousd_ARmodel$residuals)) ,y = eurousd_ARmodel$residuals))

acf(eurousd_ARmodel$residuals)
#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of 0.984 does not reject H_0, the series is not autocorrelated, appropriate to differentiate
Box.test(eurousd_ARmodel$residuals)

##MA model
ggplot(eurousd_MAmodel$residuals) +
  geom_point(aes(x = seq(1,length(eurousd_MAmodel$residuals)) ,y = eurousd_MAmodel$residuals))

acf(eurousd_MAmodel$residuals)

#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of 0.9821 does not reject H_0, the series is not autocorrelated, appropriate to differentiate
Box.test(eurousd_MAmodel$residuals)

##ARMA model
ggplot(eurousd_ARMAmodel$residuals) +
  geom_point(aes(x = seq(1,length(eurousd_ARMAmodel$residuals)) ,y = eurousd_ARMAmodel$residuals))

acf(eurousd_ARMAmodel$residuals)

#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of 0.8794 does not reject H_0, the series is not autocorrelated, appropriate to differentiate
Box.test(eurousd_ARMAmodel$residuals)


#For gold/USD
##AR model
ggplot(goldusd_ARmodel$residuals) +
  geom_point(aes(x = seq(1,length(goldusd_ARmodel$residuals)) ,y = goldusd_ARmodel$residuals))

acf(goldusd_ARmodel$residuals)

#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of 0.9713 does not reject H_0, the series is not autocorrelated, appropriate to differentiate
Box.test(goldusd_ARmodel$residuals)

##MA model
ggplot(goldusd_MAmodel$residuals) +
  geom_point(aes(x = seq(1,length(goldusd_MAmodel$residuals)) ,y = goldusd_MAmodel$residuals))

acf(goldusd_MAmodel$residuals)

#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of 0.9738 does not reject H_0, the series is not autocorrelated, appropriate to differentiate
Box.test(goldusd_MAmodel$residuals)

##ARMA model
ggplot(goldusd_ARMAmodel$residuals) +
  geom_point(aes(x = seq(1,length(goldusd_ARMAmodel$residuals)) ,y = goldusd_ARMAmodel$residuals))

acf(goldusd_ARMAmodel$residuals)

#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of 0.9843 does not reject H_0, the series is not autocorrelated, appropriate to differentiate
Box.test(goldusd_ARMAmodel$residuals)

################################################################################
#2.3 Heteroscedasticity Testing:

#If p-value is less than alpha critical value, reject H_0: X is homoscedastic
# At alpha 0.05 p-values less than 5.03e-04 (portmanteau), indicate that should reject homoscedasticity 
arch.test(eurousd_MAmodel)

#Show correlation in times (wave-like form)
acf((eurousd_MAmodel$residuals - mean(eurousd_ARMAmodel$residuals))^2)

#If p-value is less than alpha critical value, reject H_0: X is homoscedastic
# At alpha 0.05 p-values less than 4.81e-04 (portmanteau), indicate that should reject homoscedasticity 
arch.test(eurousd_ARMAmodel)

#Show correlation in times (wave-like form)
acf((eurousd_ARMAmodel$residuals - mean(eurousd_ARMAmodel$residuals))^2)

#If p-value is less than alpha critical value, reject H_0: X is homoscedastic
# At alpha 0.05 p-value of 0.896 does not allow to reject homoscedasticity 
arch.test(goldusd_MAmodel)
#Show correlation in times (wave-like form)
acf((goldusd_MAmodel$residuals - mean(goldusd_MAmodel$residuals))^2)

#If p-value is less than alpha critical value, reject H_0: X is homoscedastic
# At alpha 0.05 p-value of 0.904 does not allow to reject homoscedasticity 
arch.test(goldusd_ARMAmodel)
#Show correlation in times (wave-like form)
acf((goldusd_ARMAmodel$residuals - mean(goldusd_ARMAmodel$residuals))^2)

################################################################################
# 3 ARCH and GARCH Models
################################################################################
#3.1 Fitting ARCH Models:

eurousd_residuals_ARCHmodel <- garch(eurousd_ARMAmodel$residuals, order = c(0,1))
goldusd_residuals_ARCHmodel <- garch(goldusd_ARMAmodel$residuals, order = c(0,1))

t(confint(eurousd_residuals_ARCHmodel))
t(confint(goldusd_residuals_ARCHmodel)) #Confidence interval for a_1 contains 0 and as seen in the arch test doesn't seem heteroscedastic

summary(eurousd_residuals_ARCHmodel)
summary(goldusd_residuals_ARCHmodel)

################################################################################
#3.2 Fitting GARCH Models:
eurousd_residuals_GARCHmodel <- garch(eurousd_ARMAmodel$residuals, order = c(1,1))
goldusd_residuals_GARCHmodel <- garch(goldusd_ARMAmodel$residuals, order = c(1,1)) #Canno't fit it beacause not heteroscedastic

t(confint(eurousd_residuals_GARCHmodel)) #Not the best fit as the interval for b1 parameter contains 0
t(confint(goldusd_residuals_GARCHmodel)) #Not stimated as not invertible matrix for the model due to homoscedasticity

AIC(eurousd_residuals_ARCHmodel) #AIC -7713.326 Best fit
AIC(eurousd_residuals_GARCHmodel) #AIC -7711.325 /Also not good fit according the box test before

AIC(goldusd_residuals_ARCHmodel) #AIC 8009.544 Best fit, however the series is homoscedastic according to previous test
AIC(goldusd_residuals_GARCHmodel) #AIC 8011.566 

################################################################################
#3.2 Model Validation:

ggplot() +
  geom_path(aes(x = seq(1, length(eurousd_ARMAmodel$residuals)), y = eurousd_ARMAmodel$residuals)) +
  geom_path(aes(x =seq(1, length(eurousd_ARMAmodel$residuals)), y = (sign(eurousd_ARMAmodel$residuals)*(eurousd_residuals_ARCHmodel$fitted.values[1:length(eurousd_ARMAmodel$residuals)]))), color = "blue")

ggplot() +
  geom_path(aes(x = seq(1, length(eurousd_ARMAmodel$residuals)), y = eurousd_ARMAmodel$residuals)) +
  geom_path(aes(x =seq(1, length(eurousd_ARMAmodel$residuals)), y = (sign(eurousd_ARMAmodel$residuals)*(eurousd_residuals_GARCHmodel$fitted.values[1:length(eurousd_ARMAmodel$residuals)]))), color = "blue")


ggplot() +
  geom_path(aes(x = seq(1, length(goldusd_ARMAmodel$residuals)), y = goldusd_ARMAmodel$residuals)) +
  geom_path(aes(x =seq(1, length(goldusd_ARMAmodel$residuals)), y = (sign(goldusd_ARMAmodel$residuals)*(goldusd_residuals_ARCHmodel$fitted.values[1:length(goldusd_ARMAmodel$residuals)]))), color = "blue")

ggplot() +
  geom_path(aes(x = seq(1, length(goldusd_ARMAmodel$residuals)), y = goldusd_ARMAmodel$residuals)) +
  geom_path(aes(x =seq(1, length(goldusd_ARMAmodel$residuals)), y = (sign(goldusd_ARMAmodel$residuals)*(goldusd_residuals_GARCHmodel$fitted.values[1:length(goldusd_ARMAmodel$residuals)]))), color = "blue")


#If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of 0.8767 does not reject H_0, the series is not autocorrelated, e
summary(eurousd_residuals_GARCHmodel)

#BAD FIT , however we do the test as required
# If p-value is less than alpha critical value, reject H_0: X: is not autocorrelated series: https://rc2e.com/timeseriesanalysis#recipe-id104
# At alpha 0.05 p-value of 0.9739 does not reject H_0, the series is not autocorrelated, e
summary(goldusd_residuals_GARCHmodel)

################################################################################
#4. Forecast
################################################################################
#4.1 Point Forecasts
#best fit for the eurousd series: MA with ARCH model
eurousd_MA_forecast <- as.data.frame(forecast(eurousd_MAmodel,lead = 20))
set.seed(1)
alpha0 <- eurousd_residuals_ARCHmodel$coef[1]
alpha1 <- eurousd_residuals_ARCHmodel$coef[2]
beta1 <- 0
w <- rnorm(20)
a <- rep(0, 20)
h <- rep(0, 20)

for (i in 2:20) {
  h[i] <- alpha0 + alpha1 * (a[i - 1]^2) + beta1 * h[i - 1]
  a[i] <- w[i] * sqrt(h[i])
}

eurousd_MA_forecast$Forecast <- eurousd_MA_forecast$Forecast + a #now the confidence intervals are only approximated because of the sum of the volatility

eurousd_forecasted <- rep(0,20) #Real values 
eurousd_forecasted[1] <- tail(eurousd_clean$`EURUSD=X.Close`, 1) + eurousd_MA_forecast$Forecast[1]
for (i in 2:20) {
  eurousd_forecasted[i] <- eurousd_forecasted[i-1] + eurousd_MA_forecast$Forecast[i]
}

#best fit for the goldusd series:MA, No volatility needed
goldusd_MA_forecast <- as.data.frame(forecast(goldusd_MAmodel, lead = 20))

goldusd_forecasted <- rep(0,20) #Real values 
goldusd_forecasted[1] <- tail(goldusd_clean$`GC=F.Close`, 1) + goldusd_MA_forecast$Forecast[1]
for (i in 2:20) {
  goldusd_forecasted[i] <- goldusd_forecasted[i-1] + goldusd_MA_forecast$Forecast[i]
}

################################################################################
#4.2 Forecast Evaluation 1:
start_date_f<- as.Date("2024-09-01")
end_date_f <- as.Date("2024-09-21")

getSymbols("EURUSD=X", src = "yahoo", from = start_date_f, to = end_date_f)
getSymbols("GC=F", src = "yahoo", from = "2024-08-30", to = end_date_f)

eurusd_zoo_f <- zoo(`EURUSD=X`[, "EURUSD=X.Close"], index(`EURUSD=X`))
goldusd_zoo_f <- zoo(`GC=F`[, "GC=F.Close"], index(`GC=F`))

dates1_f <- zoo(,seq(from = start_date_f, to =  end_date_f, by = 1))
dates2_f <- zoo(,seq(from = start_date_f, to =  end_date_f, by = 1))
eurusd_zoo_f <- merge(dates1_f, eurusd_zoo_f)
goldusd_zoo_f <- merge(dates2_f, goldusd_zoo_f)

eurousd_clean_f <- na.locf(eurusd_zoo_f)
goldusd_clean_f <- na.locf(goldusd_zoo_f)
goldusd_clean_f <- tail(goldusd_clean_f,-1)

eurousd_diff_f <- diff(eurousd_clean_f, differences = 1)
goldusd_diff_f <- diff(goldusd_clean_f, differences = 1)

# For the differentiated series
ggplot() +
  geom_path(aes(x = index(eurousd_diff_f), y= eurousd_diff_f)) +
  geom_path(aes(x = index(eurousd_diff_f), y= eurousd_MA_forecast$Forecast), color = "blue")

ggplot() +
  geom_path(aes(x = index(goldusd_diff_f), y= goldusd_diff_f)) +
  geom_path(aes(x = index(goldusd_diff_f), y= goldusd_MA_forecast$Forecast), color = "blue")

#For the actual values
ggplot() +
  geom_path(aes(x = index(tail(eurousd_clean_f,-1)), y= tail(eurousd_clean_f,-1))) +
  geom_path(aes(x = index(tail(eurousd_clean_f,-1)), y= eurousd_forecasted), color = "blue")

ggplot() +
  geom_path(aes(x = index(tail(goldusd_clean_f,-1)), y= tail(goldusd_clean_f,-1))) +
  geom_path(aes(x = index(tail(goldusd_clean_f,-1)), y= goldusd_forecasted), color = "blue")


MAPE(eurousd_forecasted, tail(eurousd_clean_f,-1))
MAPE(goldusd_forecasted, tail(goldusd_clean_f,-1))

MSE(eurousd_forecasted, tail(eurousd_clean_f,-1))
MSE(goldusd_forecasted, tail(goldusd_clean_f,-1))

################################################################################
#4.3 Scenario Analysis:

#Only suitable for the Euro/USD series, Changing the volatility parameters:
set.seed(1)
alpha0 <- eurousd_residuals_ARCHmodel$coef[1] +0.0005
alpha1 <- eurousd_residuals_ARCHmodel$coef[2]
beta1 <- 0
w <- rnorm(20)
a <- rep(0, 20)
h <- rep(0, 20)

for (i in 2:20) {
  h[i] <- alpha0 + alpha1 * (a[i - 1]^2) + beta1 * h[i - 1]
  a[i] <- w[i] * sqrt(h[i])
}

eurousd_MA_forecast_param1 <- as.data.frame(forecast(eurousd_MAmodel,lead = 20)) + a

ggplot() +
  geom_path(aes(x = index(eurousd_MA_forecast_param1), y= eurousd_MA_forecast$Forecast)) +
  geom_path(aes(x = index(eurousd_MA_forecast_param1), y= eurousd_MA_forecast_param1$Forecast), color = "blue")

set.seed(1)
alpha0 <- eurousd_residuals_ARCHmodel$coef[1] 
alpha1 <- eurousd_residuals_ARCHmodel$coef[2] + 0.8
beta1 <- 0
w <- rnorm(20)
a <- rep(0, 20)
h <- rep(0, 20)

for (i in 2:20) {
  h[i] <- alpha0 + alpha1 * (a[i - 1]^2) + beta1 * h[i - 1]
  a[i] <- w[i] * sqrt(h[i])
}

eurousd_MA_forecast_param2 <- as.data.frame(forecast(eurousd_MAmodel,lead = 20)) + a

ggplot() +
  geom_path(aes(x = index(eurousd_MA_forecast_param1), y= eurousd_MA_forecast$Forecast)) +
  geom_path(aes(x = index(eurousd_MA_forecast_param1), y= eurousd_MA_forecast_param1$Forecast), color = "blue") +
  geom_path(aes(x = index(eurousd_MA_forecast_param1), y= eurousd_MA_forecast_param2$Forecast), color = "green")

