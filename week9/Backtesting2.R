
#### Backtesting


rm(list=ls())                                  

sp500 <- read.csv('sp500.csv', header=TRUE)
ret   <- diff(log(sp500$Close))
N     <- length(ret)


#### Filtered Historical simulation
library('fGarch')
var1_hs  <- numeric(N)
var1_fhs  <- numeric(N)
var1_garch <- numeric(N)

for (i in 501:N){
  retwindow   <- ret[(i-500):(i-1)]
  var1_hs[i] <- -quantile(retwindow, probs=0.01)
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  sigmapred <- predict(fit2, n.ahead=1)$standardDeviation
  sigma <- sqrt(fit2@h.t)
  retstand <- retwindow/sigma
  var1_fhs[i]  <- -sigmapred*quantile(retstand, probs=0.01)
  
  var1_garch[i] <- -qnorm(0.01, mean=0, sd=sigmapred)
}


violation <- numeric(N-500)
ind       <- which(ret[501:N] <= -var1_garch[501:N])
violation[ind] <- 1
plot(violation)
pii <- sum(violation)/length(violation)


T  <- length(violation)
violation <- violation[1:(T-1)]*10 +  violation[2:T]
T00 <- length(which(violation==0))
T01 <- length(which(violation==1))
T10 <- length(which(violation==10))
T11 <- length(which(violation==11))
pii01 <- T01/(T00 + T01)
pii11 <- T11/(T10 + T11)

Lpii <- (1-pii)^T00*pii^T01*(1-pii)^T10*pii^T11
Lpii1 <- (1-pii01)^T00*pii01^T01*(1-pii11)^T10*pii11^T11

LR  <- -2*log(Lpii/Lpii1)
LR



### Get the cutoff value for the Chi-squared distribution
qchisq(0.95, df=1)



backtest_independence <- function(x){
  T  <- length(x)
  pii <- sum(x)/T
  x <- x[1:(T-1)]*10 + x[2:T]
  T00 <- length(which(x==0))
  T01 <- length(which(x==1))
  T10 <- length(which(x==10))
  T11 <- length(which(x==11))
  pii01 <- T01/(T00 + T01)
  pii11 <- T11/(T10 + T11)
  

  
  Lpii <- (1-pii)^T00*pii^T01*(1-pii)^T10*pii^T11
  Lpii1 <- (1-pii01)^T00*pii01^T01*(1-pii11)^T10*pii11^T11
  
  LR  <- -2*log(Lpii/Lpii1)
  LR
  
}


backtest_independence(violation)



### Monte Carlo Simulation

MC <- 10000
LR_MC <- numeric(MC)
for (i in 1:MC){
  violation <- rbinom(T, 1, 0.01)
  LR_MC[i] <- backtest_independence(violation)
  }

length(which(LR_MC <= LR))/MC










