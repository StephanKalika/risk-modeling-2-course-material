
rm(list=ls())
library('fGarch')
sp500 <- read.csv('SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))
result <- garchFit( formula = ~garch(1, 1), data = Ret[1:1000], trace = FALSE)
sigma  <- sqrt(result@h.t)
Retstand <- Ret[1:1000]/sigma
sigmapred <- predict(result, n.ahead=1)$standardDeviation
result@fit$par
omega <- result@fit$par[2]
alpha <- result@fit$par[3]
beta  <- result@fit$par[4]
sigmapred/sqrt(omega/(1-alpha-beta))


MC <- 10000
T  <- 500
shock <- matrix(0, MC, T)
for (i in 1:T){
	shock[, i] <- sample(Retstand, MC, replace=T) 
}


ReturnMC <- matrix(NA, MC, T)
for (i in 1:MC){
	for (j in 1:T){
		ReturnMC[i, j] <- sigmapred*shock[i, j]
		sigmapred <- sqrt(omega + alpha*ReturnMC[i, j]^2 + beta*sigmapred^2)
	}
}


ReturnMCT <- matrix(NA, MC, T)
VaRMC     <- numeric(T)
for (i in 1:MC){
	ReturnMCT[i, ] <- cumsum(ReturnMC[i, ])
}

par(mfrow=c(1,3))
qqnorm(scale(ReturnMCT[, 1]))
qqline(scale(ReturnMCT[, 1]))
qqnorm(scale(ReturnMCT[, 200]))
qqline(scale(ReturnMCT[, 200]))
qqnorm(scale(ReturnMCT[, 500]))
qqline(scale(ReturnMCT[, 500]))



for (i in 1:T){
	VaRMC[i] <- -quantile(ReturnMCT[,i], probs=0.05)
}

plot(VaRMC/sqrt(1:T), type='l', lwd=4)






























