rm(list=ls())
#############1a#########################
#install.packages('quantmod')
library(quantmod)
getSymbols('VIIIX', from = '1998-1-1', to = '2017-4-7')
getSymbols('VGTSX', from = '1998-1-1', to = '2017-4-7')

# calculating return of each index
VIIIX_ret <- diff(log(VIIIX$VIIIX.Adjusted))[-1,]
VGTSX_ret <- diff(log(VGTSX$VGTSX.Adjusted))[-1,]
T <- length(VIIIX_ret)
# unconditional correlation
c <- cor(VIIIX_ret, VGTSX_ret)

###################1b#############################
library(fGarch)
# Estimating parameters using garch and standardize the returns of VIIIX and VGTSX
fit1b <- garchFit(fomula = ~garch(1,1), data = VIIIX_ret, trace = F)
sigma1b <- sqrt(fit1b@h.t)
VIIIX_stand1b <- VIIIX_ret/sigma1b

fit2b <- garchFit(fomula = ~garch(1,1), data = VGTSX_ret, trace = F)
sigma2b <- sqrt(fit2b@h.t)
VGTSX_stand2b <- VGTSX_ret/sigma2b

#setting parameters
lambda <- 0.94
exponentialCor <- numeric(T)
q11 <- numeric(T)
q22 <- numeric(T)

for (i in 2:T){
  q11[i] <- (1-lambda)*VIIIX_stand1b[i-1]^2 + lambda*q11[i-1] 
  exponentialCor[i] <- (1-lambda)*VIIIX_stand1b[i-1]*VGTSX_stand2b[i-1] + lambda*exponentialCor[i-1]
  q22[i] <- (1-lambda)*VGTSX_stand2b[i-1]^2 + lambda*q22[i-1]
}
exponentialCor <- exponentialCor/sqrt(q11*q22)
plot(exponentialCor, type='l', col='blue', lwd=1)

##############1c######################
alpha <- 0.05
beta <- 0.9

fit1c <- garchFit(fomula = ~garch(1,1), data = VIIIX_ret, trace = F)
sigma1c <- sqrt(fit1c@h.t)
VIIIX_stand1c <- VIIIX_ret/sigma1c

fit2c <- garchFit(fomula = ~garch(1,1), data = VGTSX_ret, trace = F)
sigma2c <- sqrt(fit2c@h.t)
VGTSX_stand2c <- VGTSX_ret/sigma2c

dynaCor1c <- numeric(T)
p11 <- numeric(T)
p22 <- numeric(T)
meanret <- mean(VIIIX_stand1c * VGTSX_stand2c)
VIIIX_mean <- mean(VIIIX_stand1c^2)
VGTSX_mean <- mean(VGTSX_stand2c^2)

for (i in 2:T){
  p11[i] <- VIIIX_mean + alpha*(VIIIX_stand1c[i-1]^2 - VIIIX_mean) + beta*(p11[i-1] - VIIIX_mean)
  dynaCor1c[i] <- meanret + alpha*(VIIIX_stand1c[i-1]*VGTSX_stand2c[i-1] - meanret) + beta * (dynaCor1c[i-1]-meanret)
  p22[i] <- VGTSX_mean + alpha*(VGTSX_stand2c[i-1]^2 - VGTSX_mean) + beta*(p22[i-1] - VGTSX_mean)
}

dynaCor1c <- dynaCor1c/sqrt(p11*p22)
points(dynaCor1c, type='l', col='red', lwd=1)

########################1d##############################
#as it's showned in the plot, in the first half of this period, the correlation
# was very unstable, whereas in the second half, it's become much more integrated and 
# remained a very stable correlation


#2a
rm(list=ls())
mc <- 50000
t <- 500
sigma <- 0.01
shock <- matrix(0,mc,t)

for (i in 1:mc){
  shock[i,] <- rnorm(t, 0, 1)
}

retmc <- matrix(NA, mc,t)
for (i in 1:mc){
  sigmamc <- sigma
  for (j in 1:t){
    retmc[i,j] <- sigmamc*shock[i,j]
    sigmamc <- sqrt(0.94*sigmamc^2 + 0.06*retmc[i,j]^2)
  }
}

cumret <- matrix(NA, mc, t)
for (i in 1:mc){
  cumret[i,] <- cumsum(retmc[i,])
}

vol <- numeric(t)
for (i in 1:t){
  vol[i] <- sd(cumret[,i])
}
plot(vol/(sigma*sqrt(1:t)), type='l', ylim = c(0,2))

#### 2b
# Theoretically the SIGMA(t+1:t+k) = K*SIGMA(t+1)^2, so SIGMA(t+1:t+k)/(SIGMA(t+1)*sqrt(k)) = 1
# The plot would be a straight line of y = 1, and as we can see from the plot, it's very similiar
# with the theoretical value.


#### 2c
rm(list = ls())

w <- 1e-6
a <- 0.05
b <- 0.9
sigma <- sqrt(w/(1-a-b))
mc <- 50000
t <- 500

shock <- matrix(rnorm(mc*t,0,1),mc,t)

retmc <- matrix(NA, mc,t)
for (i in 1:mc){
  sigmamc <-sigma
  for (j in 1:t){
    retmc[i,j] <- sigmamc*shock[i,j]
    sigmamc <- sqrt(w + a*retmc[i,j]^2 + b*sigmamc^2)
  }
}
cumret <- matrix(NA, mc, t)
for (i in 1:mc){
  cumret[i,] <- cumsum(retmc[i,])
}

vol <- numeric(t)
for (i in 1:t){
  vol[i] <- sd(cumret[,i])
}
plot(vol/(sigma*sqrt(1:t)), type='l', ylim = c(0,1.2), col='blue')

####2e
# since SIGMA(t+1) = SIGMA, so the theoretical value is sqrt(K)*SIGMA = 0.1
theo_cp <- sqrt((1:t)*sigma^2)
plot(vol, type='l', col='red', ylim=c(-0.5,0.5))
plot(vol/theo_cp, type='l', ylim = c(0,2), col='blue')
abline(h=1, type='green')
# As we can see from the plot, the result doesn't deviate too much from 1,
# which means the simulated value is very closed to the theoretical value.




#### 3a
rm(list = ls())

a <- rt(10000, df = 5)
b <- rt(10000, df = 20)
c <- rt(10000, df = 100)

mean(a)
sd(a)

mean(b)
sd(b)

mean(c)
sd(c)

####3b
par(mfrow=c(1,3))
qqnorm(scale(a))
qqline(scale(a))

qqnorm(scale(b))
qqline(scale(b))

qqnorm(scale(c))
qqline(scale(c))
# As we can see from the ploton the left, whoes df = 5, has the fattest tail
# so we can conclude that the smaller the degree of freedom, the fatter the tail.








