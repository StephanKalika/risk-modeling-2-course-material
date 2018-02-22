

### Decompsition of Correlation Matrix

rm(list=ls())
A <- matrix(c(1, 0.9, 0.9, 1), 2, 2)
Omega <- t(chol(A))
Omega%*%t(Omega)




A <- matrix(c(1, 0.9, 0.6, 0.9, 1, 0.4, 0.6, 0.4, 1), 3, 3)
Tau <- t(chol(A))
Tau%*%t(Tau)


T <- 100000
shockA <- rnorm(T, 0, 1)
shockB <- rnorm(T, 0, 1)
shock <- cbind(shockA, shockB)
cor(shock)

Omega <- matrix(c(1, -0.4, -0.4, 1), 2,2)
Tau <- t(chol(Omega))
Tau%*%t(Tau)

shockNew <- matrix(NA, T, 2)
for (i in 1:T){
  shockNew[i,] <- Tau%*%shock[i,]
}
cor(shockNew)











