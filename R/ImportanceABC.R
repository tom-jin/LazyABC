#' Importance Approximate Bayesian Computation
#'
#' Perform importance ABC sampling on user specified distributions.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param data an observation
#' @param rimport a function that samples from the importance distribution
#' @param dimport a function taking theta and returning its density from the 
#' importance distribution
#' @param prior a function taking theta and returning its density from the prior
#' @param simulator a function taking theta performing simulation
#' @param tolerance the epsilon tolerance to accept simulations
#' @param n number of iterations to run
#' @return n weighted samples of the parameter from the posterior distribution.
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