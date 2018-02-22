

### Modeling covariance and correlation

rm(list=ls())
library('fGarch')
retdata <- read.csv('SP500Tnote.csv', head=TRUE)
retsp500   <- diff(log(retdata$sp500))
rettnote   <- diff(log(retdata$tnote)) 
T            <- length(retsp500)

plot(retsp500, type='l', lwd=2, col='blue')
points(rettnote, type='l', lwd=2, col='red')


### Rolling window 
windowlength <- 25
rollingCov   <- numeric(T)

for (i in 25:T){
  rollingCov[i] <- cov(retsp500[(i-24):i], rettnote[(i-24):i])
}

plot(rollingCov, type='l', col='blue', lwd=4)


### Exponentially smoothed covariance
lambda      <- 0.94

exponentialCov  <- numeric(T)
for (i in 2:T){
  exponentialCov[i] <- (1-lambda)*retsp500[i-1]*rettnote[i-1] + lambda*exponentialCov[i-1]
}

plot(exponentialCov, type='l', col='blue', lwd=2)
points(rollingCov, type='l', col='red', lwd=2)





### Dynamic conditional correlation
#estimate volatility and standardize raw returns of tnote and sp500
library('fGarch')
fit1  <- garchFit( formula = ~garch(1, 1), data = retsp500, trace = FALSE)
sigma1 <- sqrt(fit1@h.t)
retsp500stand <- retsp500/sigma1

fit2  <- garchFit( formula = ~garch(1, 1), data = rettnote, trace = FALSE)
sigma2 <- sqrt(fit2@h.t)
rettnotestand <- rettnote/sigma2

#Exponential Smoother correlation
lambda      <- 0.94
q11         <- numeric(T)
q12         <- numeric(T)
q22         <- numeric(T)

for (i in 2:T){
  q11[i] <- (1-lambda)*retsp500stand[i-1]^2 + lambda*q11[i-1]
  q12[i] <- (1-lambda)*retsp500stand[i-1]*rettnotestand[i-1] + lambda*q12[i-1]
  q22[i] <- (1-lambda)*rettnotestand[i-1]^2 + lambda*q22[i-1]
  }

exponentialCorr <- q12/sqrt(q11*q22)
plot(exponentialCorr, type='l', col='blue', lwd=4)


# Mean-Reverting Correlation
alpha       <- 0.05
beta        <- 0.9
q11         <- numeric(T)
q12         <- numeric(T)
q22         <- numeric(T)
q11lr       <- mean(retsp500stand^2)
q12lr       <- mean(retsp500stand*rettnotestand)
q22lr       <- mean(rettnotestand^2)

for (i in 2:T){
  q11[i] <- q11lr + alpha*(retsp500stand[i-1]^2 - q11lr) + beta*(q11[i-1]-q11lr)
  # not too much meaning
  q12[i] <- q12lr + alpha*(retsp500stand[i-1]*rettnotestand[i-1] - q12lr) + beta*(q12[i-1]-q12lr)
  #estimated value
  q22[i] <- q22lr + alpha*(rettnotestand[i-1]^2 - q22lr) + beta*(q22[i-1]-q22lr)
  #not too much meaning
}

GarchCorr <- q12/sqrt(q11*q22)
plot(GarchCorr, type='l', col='blue', lwd=4)
points(exponentialCorr, type='l', col='red', lwd=4)





