
rm(list=ls())
library('fGarch')
sp500 <- read.csv('SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))
result <- garchFit( formula = ~garch(1, 1), data = Ret[1:1000], trace = FALSE)
#result <- garchFit( formula = ~garch(1, 1), data = Ret[1:1000], trace = FALSE, cond.dist = 'std')
#df <- result@fit$par[-1]
sigmapred <- predict(result, n.ahead=1)$standardDeviation
#sigmapred <- predict(result, n.ahead=1)$standardDeviation * 6
# the plot will be downward sloping

result@fit$par
omega <- result@fit$par[2]
alpha <- result@fit$par[3]
beta  <- result@fit$par[4]
#df <- result@fit$par[-1]
sigmapred/sqrt(omega/(1-alpha-beta))


### simulate shock for next 500 period
MC <- 10000
T  <- 500
shock <- matrix(0, MC, T)
for (i in 1:T){
	shock[, i] <- rnorm(MC, 0, 1)
	# 对整列使用rnorm
	# iterate over the cols of the matrix,
	# MC = num of rows, randomly generating MC=10000 times of random number
	# shock[,1] <- rt(MC, df)/sqrt(df/(df-2))
}

### updating

ReturnMC <- matrix(NA, MC, T)
# creating an initial matrix, whose dim is same as shock
for (i in 1:MC){         # iterate over each row and simulate return of the next 500 days
	sigmapredMC <- sigmapred        
	# set the sigma in the first day to be sigmapred, which is predicted by the data of previous 500 days
	for (j in 1:T){        # iterate over each day in 500 days period
		ReturnMC[i, j] <- sigmapredMC*shock[i, j]
		# R501 = SIGMA501 * Z501(SHOCK)
		sigmapredMC <- sqrt(omega + alpha*ReturnMC[i, j]^2 + beta*sigmapredMC^2)
		# updating the sigmapred for the next day using today's return and next day's shock
	}
}

### calculate cumulative return
ReturnMCT <- matrix(NA, MC, T)
for (i in 1:MC){
	ReturnMCT[i, ] <- cumsum(ReturnMC[i, ])
}


par(mfrow=c(1,3))
qqnorm(scale(ReturnMCT[, 1]))
qqline(scale(ReturnMCT[, 1]))
qqnorm(scale(ReturnMCT[, 50]))
qqline(scale(ReturnMCT[, 50]))
qqnorm(scale(ReturnMCT[, 100]))
qqline(scale(ReturnMCT[, 100]))

# plot histogram and calculate cutoff of each col
VaRMC     <- numeric(T)
for (i in 1:T){
	VaRMC[i] <- -quantile(ReturnMCT[,i], probs=0.01)
}
plot(VaRMC/sqrt(1:T), type='l', lwd=4)
# term structure, n day VaR = 1 day VaR * sqrt(n)


ESMC     <- numeric(T)
for (i in 1:T){
	indES    <- which(ReturnMCT[,i] <= -VaRMC[i])
	ESMC[i] <- -mean(ReturnMCT[indES,i])
}
plot(ESMC/sqrt(1:T), type='l', lwd=4)

























