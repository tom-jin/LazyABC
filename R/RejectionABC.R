#' Rejection Approximate Bayesian Computation
#'
#' Perform rejection ABC sampling on user specified distributions.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param data an observation
#' @param prior a function taking theta and returning its density from the prior
#' @param simulator a function taking theta performing simulation
#' @param tolerance the epsilon tolerance to accept simulations
#' @param n number of iterations to run
#' @return n weighted samples of the parameter from the posterior distribution.
#' @examples
#' posterior <- 
#' RejectionABC(data = 2, prior = function(){runif(1, -10, 10)}, 
#'              simulator = function(theta){rnorm(1, 2*(theta+2)*theta*(theta-2), 0.1+theta^2)}, 
#'              n = 3000, tolerance = 1)

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