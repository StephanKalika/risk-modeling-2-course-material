
#### This code illustrates a new method for estimating VAR: Filtered Historical Simulation


rm(list=ls())                                  ### Clean the R workspace

sp500 <- read.csv('sp500.csv', header=TRUE)       ### Load the daily return data of SP500 index
price <- sp500$Close                           ### Extract the price information

T     <- length(price)

ret   <- log(price[2:T]) - log(price[1:(T-1)]) ### Calculate the log return 

ind   <- which(ret!=0)                         ### Only keep those returns that are not zero (i.e. not on holidays)

ret   <- ret[ind]
T     <- length(ret)

-
  

#### Filtered Historical simulation
library('fGarch')
var1_hs  <- numeric(T)
var1_fhs  <- numeric(T)

for (i in 501:T){
  retwindow   <- ret[(i-500):(i-1)]
  var1_hs[i] <- -quantile(retwindow, probs=0.01)
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  sigmapred <- predict(fit2, n.ahead=1)$standardDeviation
  sigma <- sqrt(fit2@h.t)
  retstand <- retwindow/sigma
  var1_fhs[i]  <- -sigmapred*quantile(retstand, probs=0.01)
}

plot(var1_fhs, col='red', type='l', ylim=c(0,0.15))
points(var1_hs, col='blue', type='l')




####################################
N <- length(price)
violation <- numeric(N-500)
ind <- which(ret[501:N] <= var1_fhs[501:N])
violation[ind] <- 1
plot(violation)
sum(violation)/length(violation)

T <- length(violation)
T1 <- sum(violation)
T0 <- T - T1
pii <- T1/T
# null hypothesis

Lp <- (1-0.01)^T0*0.01^T1
Lpii <- (1-pii)^T0*pii*T1
# alternative happening

LR <- -2*log(Lp/Lpii)
# define likelihood ratio number
# LR = 17.67

qchisq(0.95, df=1)
pchisq(17.68, 1)













