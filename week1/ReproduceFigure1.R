


rm(list=ls())                                  ### Clean the R workspace

sp500 <- read.csv('sp500.csv', header=T)       ### Load the daily return data of SP500 index
price <- sp500$Close                           ### Extract the price information

T     <- length(price)

ret   <- log(price[2:T]) - log(price[1:(T-1)]) ### Calculate the log return 

ind   <- which(ret!=0)                         ### Only keep those returns that are not zero (i.e. not on holidays)

ret   <- ret[ind]

autocorr <- acf(ret, 100)$acf[2:101] ### Calculate the autocorrelation for LAGS from 1 to 100


plot(autocorr, type='l')
abline(0, 0, col='red')



