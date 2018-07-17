#Time series Model on SO2 level in the air of Delhi
AirQ = read.csv("data.csv")
summary(AirQ)

#calculating missing data
AirQ$so2 = ifelse(is.na(AirQ$so2),
                  ave(AirQ$so2, FUN= function(x) mean(x, na.rm = TRUE)),
                  AirQ$so2)
AirQ$so2
DEL <- AirQ[AirQ$location == "Delhi",]
DEL$so2
summary(DEL)
# converting dataset to a time series object
DELso2 = ts(DEL$so2, frequency = 12,start = c(1987,1), end = c(2015))
plot(DELso2) #no trend and seasonality
#forecasting the time series data
SO2forecast = HoltWinters(DELso2,beta = F, gamma = F)
SO2forecast
plot(SO2forecast)
library(forecast)
SO2future=forecast::forecast(SO2forecast,h=48)
plot(SO2future)

#Forecasting using ARIMA MODEL
#testing dataset for stationarity using Augmented Dickey-Fuller Test
install.packages("tseries")
library(tseries)
adf.test(DELso2) #if p-value < 0.05 indicates the TS is stationary. (here p= 0.01)
#time series model is stationary
# this is a non seasonal data
#finding values of p, d, q values using auto.arima()
auto.arima(DELso2)
SO2fit = arima(DELso2, order = c(4,1,5))
SO2future =forecast:::forecast.Arima(SO2fit,h=48)
plot(SO2future)








