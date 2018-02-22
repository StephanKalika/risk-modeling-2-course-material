


rm(list=ls())                                  ### Clean the R workspace

sp500 <- read.csv('sp500.csv', header=T)       ### Load the daily return data of SP500 index
price <- sp500$Close                           ### Extract the price information

T     <- length(price)

ret   <- log(price[2:T]) - log(price[1:(T-1)]) ### Calculate the log return 

ind   <- which(ret!=0)                         ### Only keep those returns that are not zero (i.e. not on holidays)

ret   <- ret[ind]
T     <- length(ret)



#### Historical simulation
var1_250  <- numeric(T)
var5_250  <- numeric(T)

for (i in 251:T){
  var1_250[i] <- -quantile(ret[(i-250):i], probs=0.01) # 99% VaR
  #the purpose is to calculate VaR of each date based on previous 250 scenarios.
  var5_250[i] <- -quantile(ret[(i-250):i], probs=0.05) # 95% VaR 
}

plot(var1_250, col='red', type='l', ylim=c(0,0.1))
points(var5_250, col='blue', type='l')






#### Historical simulation
var1_1000  <- numeric(T)

for (i in 1001:T){
  var1_1000[i] <- -quantile(ret[(i-1000):i], probs=0.01) 
}

plot(var1_250, col='red', type='l', ylim=c(0,0.1))
points(var1_1000, col='blue', type='l')          #over write the second line on the first









