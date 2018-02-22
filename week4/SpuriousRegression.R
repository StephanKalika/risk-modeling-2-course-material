library('tseries')
shiller <- read.csv('Shillerdata.csv', header = T)
ret <- diff(log(shiller$Price))

### An example of supurious regression

rm(list=ls())
dd1 <- arima.sim( model = list(order=c(0, 1, 0)), n = 1000, sd = 0.1)
dd2 <- arima.sim( model = list(order=c(0, 1, 0)), n = 1000, sd = 0.1)

reg1 <- lm(dd1 ~ dd2) # *** highly significant,regression is misspecified.
summary(reg1)
acf(reg1$residuals) # residue is highly autocorrelated 

dd1_diff <- diff(dd1)# take the first difference, taking first difference reduces autocorrelation
dd2_diff <- diff(dd2)

reg2 <- lm(dd1_diff ~ dd2_diff) # no * means not correlated
summary(reg2)
acf(reg2$residuals) # no autocorrelation


shiller <- cbind(shiller, c(NA, ret)) # create sixth col
shiller <- shiller[-1,] # drop the last line which has no return
# pe ratio today contains returns in the future, negatively predicted returns


