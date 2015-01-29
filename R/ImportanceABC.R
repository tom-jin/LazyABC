ImportanceABC <- function(data, rimport, dimport, prior, simulator, tolerance = 0.1, n = 10000) {
  theta <- rep(NA, n)
  weight <- rep(NA, n)
  
  for(i in 1:n) {
    theta[i] <- rimport()
    X <- simulator(theta)
    l <- as.numeric(sum(abs(X - data)) < tolerance)
    if(dimport(theta[i]) == 0){
      weight[i] <- 0
    } else {
      weight[i] <- l * prior(theta[i]) / dimport(theta[i])
    }
  }
  return(data.frame(theta = theta, weight = weight))
}