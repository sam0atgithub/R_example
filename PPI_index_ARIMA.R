library("tseries")
library("forecast")
library("tidyverse")

#load data
data1 <- read.csv("C:/Users/17786/Desktop/timeseries_ppi.csv")

ppi <- ts(data1[,"ppi"], frequency = 4)

#plot data
autoplot(ppi) + ggtitle("PPI index") + ylab("ppi")
#looks non-stationary, no need for transformation

#find the appropriate ARIMA model
model1 <- auto.arima(ppi, seasonal = TRUE, approximation = FALSE,
                     stepwise = FALSE)

summary(model1)

#check if residuals have any spikes in ACF
checkresiduals(model1)
#results show that our model looks great

#forecast based on the ARIMA model above
forecast1 <- forecast(model1, h = 20)

autoplot(forecast1)

summary(forecast1)
