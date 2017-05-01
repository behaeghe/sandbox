## Feigenbaum.R
library(compiler) ## requires R >= 2.13.0
logistic.map <- function(r, x, N, M){
  ## r: bifurcation parameter, x: initial value
  ## N: number of iterations, M: iterations to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i]  * (1 - z[i])
  }
  z[c((N-M):N)]
}
logistic.map <- cmpfun(logistic.map)
r <- seq(2.8, 4, by=0.005)
N <- 200; M <- 100; start.x <- 0.25
Orbit <- as.vector(sapply(r, logistic.map, x=start.x, N=N, M=M))
r <- rep(r, each=(M+1))
plot(Orbit ~ r, pch=15, cex=0.5)