#' LazyABC: A package to acompany an OxWaSP report.
#'
#' This package is intended to accompany an OxWaSP report and is not intended 
#' for general distribution.
#' 
#' @docType package
#' @name foo
NULL

#' Lazy Approximate Bayesian Computation
#'
#' Lazily perform importance ABC sampling on user specified distributions.
#'
#' This is a demo implementation of Dennis Prangle's Lazy ABC algorithm.
#'
#' @param data an observation
#' @param rimport a function that samples from the importance distribution
#' @param dimport a function taking theta and returning its density from the 
#' importance distribution
#' @param prior a function taking theta and returning its density from the prior
#' @param simInit a function taking theta performing initial simulation
#' @param simCont a function taking theta and x completing the simulation
#' @param alpha a function taking theta and the intermediate simulations 
#' returning a number (0,1] to determine the probability of continuing
#' @param tolerance the epsilon tolerance to accept simulations
#' @param n number of iterations to run
#' @return n weighted samples of the parameter from the posterior distribution.
#' @examples
#' obs <- sapply(1:100, function(x){rnorm(1, (-3)*x, 1)})
#' LazyABC(obs, 
#' rimport = function(){rnorm(1, 5, 5)}, 
#' dimport = function(x){dnorm(x, 5, 5)}, 
#' prior = function(x){dunif(x, 0, 10)}, 
#' simInit = function(x){rnorm(1, (x+2)*(x-2), 1)}, 
#' simCont = function(theta, x){
#'   c(x, data <- sapply(2:100, function(x){rnorm(1, (theta+2)*(theta-2)*x, 1)}))
#'   }, 
#'   alpha = function(theta, x){if(abs(x + 2.63) < 1){1} else {0.1}}, tol = 1000, n = 10000)
#'
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