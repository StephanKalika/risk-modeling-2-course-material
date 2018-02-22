


dd <- arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))


dd <- arima.sim(model = list(ar = -0.9), n = 100, sd = 0.1) 
acf(dd)
sd(dd)





### Reproducing Figure 3.2
ts1 <- arima.sim( model = list(ar = 0.9), n = 100000, sd = 0.1)
acf(ts1)
pacf(ts1)
autocorr1 <- acf(ts1, 100)$acf[1:101] #how large is the shock or noise


ts2 <- arima.sim( model = list(ar = 0.5),  n = 100000, sd = 0.1)
acf(ts2)
pacf(ts2)
autocorr2 <- acf(ts2, 100)$acf[2:101]

ts3 <- arima.sim( model = list(ar = 0.1),  n = 100000, sd = 0.1)
pacf(ts3)
autocorr3 <- acf(ts3, 100)$acf[1:101]

########################################################################
ts4 <- arima.sim( model = list(order=c(0, 1, 0)), n = 100000, sd = 0.1)
autocorr4 <- acf(ts4, 100)$acf[1:101]#middle parameter of order is 1 means random walk
########################################################################
ts5 <- arima.sim(model = list(ar = -0.9), n = 100000, sd = 0.1)
acf(ts5)
autocorr5 <- acf(ts5, 100)$acf[1:101]

plot(autocorr1, col='red', type='l', ylim=c(0, 1.2), lwd=5)
points(autocorr2, col='green', type='l', lwd=5)
points(autocorr3, col='blue', type='l', lwd=5)
points(autocorr4, col='black', type='l', lwd=5)
plot(autocorr5, col='yellow', type='l', lwd=3)


dd <- arima.sim(model = list(ar = c(0.5,0.4)), n = 1000, sd=1)
acf(dd, 20)
pacf(dd, 20)


