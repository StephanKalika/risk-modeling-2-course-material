hw <- read.csv('HW1Data.csv', header = T)
price <- hw$Close
date <- hw$Date
T <- length(price)

############ PROBLE 8 ####################
ret <- log(price[2:T]) - log(price[1:T-1])
ind <- which(ret!=0)
ret <- ret[ind]
date <- date[ind]
T <- length(ret)
plot(date, ret, type = 'l', col = 'black')
plot(date, price[ind], type = 'l', col = 'blue')

############ PROBLE 9 ####################
mean(ret)
sd(ret)

############ PROBLE 10 ####################
autocorr <- acf(ret, 100)$acf[1:101]
plot(autocorr, type = 'l')
abline(0,0, col='red')
acf(autocorr)

############ PROBLE 11 ####################
sqr_ret <- ret^2
autocorr_sqr <- acf(sqr_ret, 100)$acf[1:101]
plot(autocorr_sqr, type = 'l', ylim = c(0,1))
acf(sqr_ret)
abline(0,0, col='red')

############ PROBLE 12 ####################
var1 <- numeric(T)
for (i in 251:T){
  var1[i] <- -quantile(ret[(i-250):i], probs = 0.01)
}
plot(var1, col='red', type='l', ylim=c(0,0.1))

############ PROBLE 13 ####################
var5 <- numeric(T)
for (i in 251:T){
  var5[i] <- -quantile(ret[(i-250):i], probs = 0.05)
}
points(var5, col='blue', type = 'l')


############ PROBLE 14 ####################
library(tseries)
adf.test(price, alternative = 'stationary')
adf.test(ret, alternative = 'stationary')

############ PROBLE 15 ####################
xt <- arima.sim(model = list(ar = 0.9), n = 1000000, sd = 0.1)
observation <- acf(xt, 10)$acf[2:11]
x = c(1:10)
theoretical <- 0.9^x
sum(observation)
sum(theoretical)

############ PROBLE 16 ####################
yt <- arima.sim(model = list(ar = 0.5), n = 1000000, sd = 0.1)
zt <- xt + yt
ob <- acf(zt, 10)$acf[2:11]
theo <- theoretical + 0.5^x
sum(ob)
sum(theo)




