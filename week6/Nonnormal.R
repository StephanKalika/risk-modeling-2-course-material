



### Garch model can explain non-normal features of financial data

rm(list=ls())
#install.packages('fGarch')
library('fGarch')
spec   <- garchSpec(model = list(omega = 5e-6, alpha = 0.1, beta=0.8))
dd     <- garchSim(spec, n = 100000)
fit1   <- garchFit( formula = ~garch(1,1), data=dd, trace=FALSE)
sigma  <- sqrt(fit1@h.t)
ddstand <- dd/sigma

par(mfrow=c(1,2))
qqnorm(ddstand)
qqline(ddstand, col='red')

qqnorm(dd)
qqline(dd, col='red')



### Conditional nonnormality
rm(list=ls())
library('fGarch')
sp500 <- read.csv('SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))

fit2 <- garchFit( formula = ~garch(1, 1), data = Ret, trace = FALSE)
sigma <- sqrt(fit2@h.t)

Retstand <- Ret/sigma

par(mfrow=c(1,2))
qqnorm(Ret, main='S&P500 Returns')
qqline(Ret, col='red')


qqnorm(Retstand, main='Standardized S&P500 Returns')
qqline(Retstand, col='red')


qqplot(Retstand, rt(length(Retstand), 50))
qqline(Retstand, distribution=function(p) qt(p, df=50), col='blue')









### Student t-distribution
rm(list=ls())
y <- rt(2000, df = 5)
qqnorm(y) 
qqline(y, distribution=function(p) qt(p, df=5))
qqplot(y, rt(2000, df = 5))







