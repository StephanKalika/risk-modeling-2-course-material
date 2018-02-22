rm(list=ls())
library(quantmod)
install.packages('Ecdat')
#1a
data(SP500, package = 'Ecdat')

#1b
window <- SP500$r500[(1805-500):1804]
fit1 <- garchFit(fomula = ~garch(1,1), data=window, trace = TRUE)
# parameters:   mu = 1.273546e-03 
              # omega = 8.440923e-06  
              # alpha = 7.168575e-02
              # beta = 8.470616e-01

#1c 
sigma <- sqrt(fit1@h.t)
plot(sigma, type='l')

#1d
pred <- predict(fit1, n.ahead = 1)
meanpred = pred$meanForecast
stdpred = pred$standardDeviation
prob <- pnorm(-0.228, mean=meanpred, sd=stdpred)

#1e
win_standard <- window/sigma
plot(win_standard, type='l', main='Standardized Returns')
acf(win_standard, 20)
acf(win_stand^2, 20)
# the result shows little autocorrelation, which means the Garch(1,1) model fits quite adequately

#1f
qqnorm(win_standard)
qqline(win_standard, col='red', type='blue')
# According to the qq plot, the standardized return is not nromal distributed, it has
# fatter tail on the left and less fatter tail on the right.

#1g
fit2 <- garchFit(fomula = ~garch(1,1), data = SP500$r500, cond.dist = 'std')
predict2 <- predict(fit2, n.ahead = 1)
df <- fit2@fit$par[5]
prob <- pstd(SP500$r500[1805], mean = predict2$meanForecast, sd=predict2$standardDeviation, nu=df)



# 2a
rm(list=ls())
getSymbols('BAC', from = '2000-1-3', to = '2017-3-24')
BAC <- BAC$BAC.Adjusted
plot(BAC, type='l')

#2b
ret <- diff(log(BAC))
ret <- ret[-1,]
T <- length(ret)
var_garch <- numeric(T)

for (i in 501:T){
  retwindow <- ret[(i-500):(i-1)]
  fitb <- garchFit(fomula = ~garch(1,1), data = retwindow, trace=FALSE)
  sigmapred <- predict(fitb, n.ahead=1)$standardDeviation
  meanpred <- predict(fitb, n.ahead=1)$meanForecast
  var_garch[i] <- -qnorm(0.01, mean=meanpred, sd=sigmapred)
}
plot(var_garch, type='l', col='blue')

#2c
var_fhs <- numeric(T)
for (i in 501:T){
  retwindow <- ret[(i-500):(i-1)]
  fit2c <- garchFit(fomula = ~garch(1,1), data = retwindow, trace=FALSE)
  sigma <- sqrt(fit2c@h.t)
  sigmapred <- predict(fit2c, n.ahead=1)$standardDeviation
  retstand <- retwindow/sigma
  var_fhs[i] <- -sigmapred*quantile(retstand, probs=0.01)
  
}
points(var_fhs, type='l', col='red')

#From the two plots we had, the blue line, which is the line we plot using garch method,
#lies below the red line, which is the line returned from fhs method, this suggests that
# the fhs method might be a more accurate way to calculate the VaR, since it can predict 
# more loss in a single day.


#2d
qqnorm(ret)
qqline(ret, col='red')
# both tails deviate significantly from the 45 degree line, which means the return is 
# unconditionally nonnormal


#2e
fit4 <- garchFit(fomula = ~garch(1,1), data = ret, trace = FALSE)
sigma1 <- sqrt(fit4@h.t)
retstand1 <- ret/sigma1
qqnorm(retstand1)
qqline(retstand1, col='red')
#


#2f
rm(list=ls())
# Download historical prices 
# Construct the portfolio consisting AAPL, BAC and BP

getSymbols('BAC', from = '2000-1-3', to = '2017-3-24')
BAC <- BAC$BAC.Adjusted
getSymbols('AAPL', from = '2000-1-3', to = '2017-3-24')
AAPL <- AAPL$AAPL.Adjusted
getSymbols('BP', from = '2000-1-3', to = '2017-3-24')
BP <- BP$BP.Adjusted
portfolio <- 1/3*BP + 1/3*BAC + 1/3*AAPL

#daily log returns of the portfolio
Ret <- diff(log(portfolio))[-1,]

l <- length(Ret)
var_garch_2f <- numeric(l)
var_fhs_2f <- numeric(l)

# garch method and fhs method
for (i in 501:T){
  retwindow <- Ret[(i-500):(i-1)]
  fit2f <- garchFit(fomula = ~garch(1,1), data = retwindow, trace=FALSE)
  sigmapred_2f <- predict(fit2f, n.ahead=1)$standardDeviation
  meanpred_2f <- predict(fit2f, n.ahead=1)$meanForecast
  sigma_2f <- sqrt(fit2f@h.t)
  Retstand <- retwindow/sigma_2f
  var_garch_2f[i] <- -qnorm(0.01, mean = meanpred_2f, sd = sigmapred_2f)
  var_fhs_2f[i] <- -sigmapred_2f*quantile(Retstand, probs=0.01)
}
points(var_garch_2f, type='l', col='black')
points(var_fhs_2f, type='l', col='red')


