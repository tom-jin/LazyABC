RejectionABC <- function(data, prior, simulator, tolerance = 0.1, n = 10000) {
  posterior <- rep(NA, n)
  
  for(i in 1:n) {
    theta <- NA
    repeat {
      theta <- prior()
      X <- simulator(theta)
      if(abs(X - data) < tolerance)
        break
    }
    posterior[i] <- theta
  }
  return(posterior)
}