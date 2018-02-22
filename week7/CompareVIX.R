
#### Compare Garch-estimated volatility with VIX


rm(list=ls())                                  

vix <- read.csv('VIX.csv', header=TRUE)              ## http://www.cboe.com/products/vix-index-volatility/vix-options-and-futures/vix-index/vix-historical-data
vix$VIX  <- vix$VIX/1000
Ret <- diff(log(vix$Close))
vix <- cbind(vix[-1,], Ret)
T   <- nrow(vix)



library('fGarch')
vol_garch <- numeric(T)

for (i in 500:T){
  retwindow   <- Ret[(i-500+1):i]

  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  vol_garch[i] <- predict(fit2, n.ahead=1)$standardDeviation
}



plot(vol_garch[500:T], col='red', type='l', ylim=c(0,0.15), lwd=4)
points(vix$VIX[500:T], col='blue', type='l', lwd=4)

plot(vol_garch[500:T], vix$VIX[500:T])
cor(vol_garch[500:T], vix$VIX[500:T])










