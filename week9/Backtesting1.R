
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
ind       <- which(ret[501:N] <= -var1_fhs[501:N])
violation[ind] <- 1
plot(violation)
sum(violation)/length(violation)


T  <- length(violation)
T1 <- sum(violation)
T0 <- T - T1
pii <- T1/T
# null hypothesis

Lp <- (1-0.01)^T0*0.01^T1
Lpii <- (1-pii)^T0*pii^T1
# alternative hypothesis

LR  <- -2*log(Lp/Lpii)
LR
# define likelihood ratio
pchisq(LR, df=1)



### Get the cutoff value for the Chi-squared distribution
qchisq(0.95, df=1)



backtest_coverage <- function(x, p){
  T <- length(x)
  T1 <- sum(x)
  T0 <- T - T1
  pii <- T1/T
  
  Lp <- (1-p)^T0*p^T1
  Lpii <- (1-pii)^T0*pii^T1
  
  LR  <- -2*log(Lp/Lpii)
  LR  <- c(LR, pii)
}


backtest_coverage(violation, 0.01)



### Monte Carlo Simulation

MC <- 10000
LR_MC <- numeric(MC)
pi_MC <- numeric(MC)
for (i in 1:MC){
  violation <- rbinom(T, 1, 0.01)
  LR_MC[i] <- backtest_coverage(violation, 0.01)[1]
  pi_MC[i] <- backtest_coverage(violation, 0.01)[2]
  }

length(which(LR_MC <= LR))/MC

plot(pi_MC, LR_MC, type='l', col='blue', lwd=2)










