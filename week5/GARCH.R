



### Garch simulation and maximum likelihood estimation

rm(list=ls())
install.packages('fGarch')
library('fGarch')
spec   <- garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta=0.8, beta2 = 0.0003))
dd     <- garchSim(spec, n = 100000)
fit1   <- garchFit( formula = ~garch(2,1), data=dd, trace=FALSE)
sigma  <- sqrt(fit1@h.t)


### Maximumum likelihood estimation on SP500
rm(list=ls())
sp500 <- read.csv('SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))
fit2 <- garchFit( formula = ~garch(1, 1), data = Ret, trace = FALSE)
sigma <- sqrt(fit2@h.t)
plot(sigma, type='l')



### Maximum likelihood estimation for AAPL
### Download stock prices from Yahoo Finance
rm(list=ls())
library(quantmod)
getSymbols('JPM', from ="2000-01-03", to = "2017-02-21")
JPM <- JPM$JPM.Adjusted
Ret <- diff(log(JPM))
Ret <- Ret[-1,]
fit3 <- garchFit( formula = ~garch(1, 1), data = Ret, trace = FALSE)
sigma <- sqrt(fit3@h.t)
plot(sigma, type='l')






### Leverage effect
fit3 <- garchFit( formula = ~garch(1, 1), data = Ret, delta=2, leverage=TRUE, trace = FALSE)



