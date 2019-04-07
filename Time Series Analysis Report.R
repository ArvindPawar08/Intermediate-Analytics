
#Part A
library(ggplot2)
install.packages("TTR")
library(TTR)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)

births_data<- scan("https://robjhyndman.com/tsdldata/data/nybirths.dat", skip=0)
births_times <- ts(births_data, frequency = 12, start = c(1946,1))
births_times

plot(births_times, main="Time series plot from 1946 to 1960")


components<- decompose(births_times)
components
plot(components)


A_births<- births_data-components$seasonal
plot(A_births, main="Time series with only Trend and Random Components")

#Part B

volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodust_times<-ts(volcanodust, start = c(1500))
volcanodust_times
plot(volcanodust_times, main="Time series for volcanoduct dataset")


pacf(volcanodust_times, main="Partial Autocorrelation plot")

acf(volcanodust_times, main="Autocorrelation plot")


Arima_Model<- arima(volcanodust_times, order = c(1,1,3))
Arima_Model


Forcast<- forecast(Arima_Model, h=15)
plot(Forcast, main="Forecast on the ARIMA model")

