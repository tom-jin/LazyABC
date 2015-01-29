LazyABC <- function(data, rimport, dimport, prior, simInit, simCont, alpha, tolerance = 0.1, n = 10000) {
  theta <- rep(NA, n)
  weight <- rep(NA, n)
  
  for(i in 1:n) {
    theta[i] <- rimport()
    X <- simInit(theta)
    a <- alpha(theta, X)
    if(runif(1) < a){
      Z <- simCont(theta[i], X)
      l <- as.numeric(sum(abs(Z - data)) < tolerance) / a
      weight[i] <- l * prior(theta[i]) / dimport(theta[i])
    } else {
      weight[i] <- 0
    }
  }
  return(data.frame(theta = theta, weight = weight))
}