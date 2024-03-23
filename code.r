data<-read.csv(file = "~/desktop/monthly-milk-production-pounds-p.csv") ##read data 
data = ts(data[,2],start = c(1962,1),frequency = 12)

# plot milk production as time series
plot(data, xlab = "Years", ylab = "Milk Production")

# Step 2 Difference data to make data stationary on mean
plot(diff(data),ylab = "Differenced Milk Production")

# Step 3 log transform data to make data stationary on variance
plot(log10(data), ylab = "log(Milk Production)")

# Step 4 Difference log transform data to make data stationary on both mean and variance
plot(diff(log10(data)),ylab = "Difference Log (Milk Production)")

# Step 5 plot ACF and PACF to identify potential AR and MA model
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main = 'ACF Milk Production')
pacf(ts(diff(log10(data))), main = 'PACF Milk Production')

# Step 6 Identification of best fit ARIMA model
## Loading required package: forecast
require(forecast)
ARIMAfit = auto.arima(log10(data), approximation = FALSE, trace = FALSE)
summary(ARIMAfit)

# Step 7 Forecast production by using the best fit ARIMA model
par(mfrow = c(1,1))
pred = predict (ARIMAfit, n.ahead = 72) ##predict for 6 yrs 
pred

plot(data, type = 'l', xlim = c(1962,1981), ylim = c(1,1600), xlab = 'Year', ylab = 'Milk
Production' )
lines(10^(pred$pred),col = 'blue')
lines(10^(pred$pred+2*pred$se), col = 'orange')
lines(10^(pred$pred-2*pred$se), col = 'green')

# Step 8 Plot ACF and PACF for residuals of ARIMA model
par(mfrow = c(1,2))
acf(ts(ARIMAfit$residuals), main = 'ACF Residual')
pacf(ts(ARIMAfit$residuals), main = 'PACF Residual')

