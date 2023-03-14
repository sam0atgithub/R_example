library(tidyverse)
library(forecast)
library(tseries)

#Bierens, Martins (2010)

#tsDyn is a package including tseries, VAR, VECM
library(tsDyn)
data(barry)
barry <- barry[, c("cpiUSA", "cpiCAN")]
plot(barry)

#diff calculates the difference between t and t-1
dbarry <- diff(barry)
plot(dbarry)

mreg <- lineVar(dbarry, lag = 3, model = "VAR")
print(mreg)

#impulse response function and SVAR: irf can be used to test if the assumption of structure VAR is true

#Will an impulse in e1t affect both y1(t+h) and y2(t+h) over time?
irf_var = irf(mreg, impulse = "cpiUSA", response = c("cpiUSA", "cpiCAN"), boot = FALSE)
print(irf_var)
plot(irf_var)
#the plot shows that y1t affects both y1(t+h) and y2(t+h) over time (here h = 10)

#Will an impulse in e2t affect both over time?
irf_var2 = irf(mreg, impulse = "cpiCAN", response = c("cpiUSA", "cpiCAN"), boot = FALSE)
print(irf_var2)
plot(irf_var2)
#result is weak, see explanation in the paper

#Granger Test for SVAR: is y2(t-h) really correlated with y1t?
library(lmtest)
data(ChickEgg)

#By default, Granger test is based on F test
#order = 3 means we take lag1 lag2 and lag3 of eggs and chicken
grangertest(egg ~ chicken, order = 3, data = ChickEgg)
#betas are insignificant, thus uncorrelated

#test the reverse relationship
grangertest(chicken ~ egg, order = 3, data = ChickEgg)
#betas are significant, thus correlated, egg first!

#notice that we can change the order and then do more Granger tests

#We can adjust the setting to make it based on Wald test
grangertest(egg ~ chicken, order = 3, data = ChickEgg, test = "Chisq")
grangertest(chicken ~ egg, order = 3, data = ChickEgg, test = "Chisq")
#results are the same, because F test is based on Wald test

#Dicky-Fuller Test, included in the urca package
library(urca)
#simulate a dataset
n <- 100
y <- arima.sim(n = n, list(order = c(0,1,0)))
DFtest <- ur.df(y, type = "none", lags = 0)
summary(DFtest)

#notice: DFtest is a one-side test, t-statistic(tau1) usually negative
#type = none means no drift, type = "drift" means with drift, "trend" means drift + trend
#drift: phi1 refers to the joint null hypothesis mu = gamma = 0 (neither drift nor RW)
#trend: phi2 refers to neither drift nor RW, phi3 refers to none of drift, trend, RW

#DFtest is based on the assumption of no serial correlation in error terms
#Augmented DFtest handles serial correlation by taking lags
#ADFtest uses AIC or BIC to determine the number of lags

#An example
y <- arima.sim(model = list(order = c(3, 1, 1), ar = c(0.4, 0.2, 0.2), ma = 0.5), n = 1000)
df <- ur.df(y, type = "trend", lags = 10, selectlags = "AIC")
print(summary(df))

#lags = 10 means the max number of lags we use is 10

#pp test also handle serial correlation in error terms in another way
#it is more generalized than ADF test
pp <- ur.pp(y, type = "Z-tau", model = "trend", lags = "short")
summary(pp)

#notice: the options lags or use.lag in ur.pp is about the lags in the long-run
#variance, not the lag of the delta(yt) in the regression

#the null of ADF and PP tests is unit root

#A third way is KPSS test, it tests for RW + trend + drift
ur.kpss(y, type = "tau", lags = "short") %>% summary()
#lags option is again for the lrvar estimation
#type = mu means only intercept in the regression (the model assumes no trend, only drift + RW)
#type = tau means we assume trend + drift + RW

#detect an explosive bubble: beta > 1 versus RW
#H0 = Unit Root, HA = beta > 1 (explosive)
#need to specify the beginning and the end of the time series, and the size of the smallest time window
#included in a specific package psymonitor
library(psymonitor)

#first take the log form (common in playing with financial data)
psy.stat <- PSY(log_data)
plot(psy.stat, type = "l")

#though not usually used, we can use metrics to compare forecasts of models and show which one
#has lowest error. notice that we are comparing the ex-post forecasts using the test set
accuracy(forecast_result1, x = test_set)
accuracy(forecast_result2, x = test_set)

#Alternatively, we have the DM test to detect if one model dominates the other
#suppose we have a AR12 and a MA12 model
dm.test(e1 = e.ar12, e2 = e.ma12, alternative = "two.sided", h = 1, power = 1)
#the two side test is used to test if they are different, power means the power of loss function
#"greater" means HA is model2 better than model1
#"less" means H0 is m2 better than m1, HA is m1 better than m2




