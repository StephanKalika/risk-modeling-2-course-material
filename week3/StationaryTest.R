


rm(list=ls())                                  ### Clean the R workspace
library('tseries')

sp500 <- read.csv('sp500.csv', header=T)       ### Load the daily return data of SP500 index
price <- sp500$Close                           ### Extract the price information

T     <- length(price)

ret   <- log(price[2:T]) - log(price[1:(T-1)]) ### Calculate the log return 

ind   <- which(ret!=0)                         ### Only keep those returns that are not zero (i.e. not on holidays)

ret   <- ret[ind]


adf.test(ret, alternative='stationary')        ### When the p-value is larger than some pre-specified small number, the series is considered non-stationary




shiller <- read.csv('Shillerdata.csv', header=TRUE)

adf.test(shiller$Price, alternative='stationary')
adf.test(shiller$PE, alternative='stationary')




