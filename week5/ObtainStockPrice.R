



### Download stock prices from Yahoo Finance
rm(list=ls())
install.packages('quantmod')
library(quantmod)

# download data
getSymbols('MSFT', from ="1990-01-03", to = "2017-02-19")
head(MSFT)
tail(MSFT)

# extract adjusted closing prices
MSFT = MSFT$MSFT.Adjusted

# plot prices
plot(MSFT)
