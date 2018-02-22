


### Simulate a MA time series

rm(list=ls())
dd <- arima.sim(model = list(ma = 1.9), n = 100000, sd = 0.1) 
acf(dd)
sd(dd)



### Simulate a random walk

rm(list=ls())
library('tseries')
dd <- arima.sim( model = list(order=c(0, 1, 0)), n = 100000, sd = 0.1)
plot(dd)
adf.test(dd, alternative='stationary')      ### When the p-value is larger than 5%, the series is considered non-stationary




### Simulate an ARIMA time series

rm(list=ls())
library('tseries')
dd <- arima.sim( model = list(ar = 0.9, order=c(1, 1, 0)), n = 100000, sd = 0.1)
plot(dd)
adf.test(dd, alternative='stationary')      ### When the p-value is larger than 5%, the series is considered non-stationary
acf(dd)

rm(list = ls())
library('tseries')
dd <- arima.sim(model = list(ar = 0.9, ma = c(1.9, 0.3), order = c(1, 1, 2)), n=10000, sd = 0.1)
plot(dd)
ddd <- diff(dd)
acf(ddd)

a <- arima.sim(model = list(ar = c(0.7, 0.4), ma = c(1.9, 0.3), order = c(2,0,2)), n=10000, sd=0.1)
plot(a)
acf(a)
