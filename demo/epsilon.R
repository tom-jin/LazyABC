library(manipulate)

manipulate({
  startTime <- proc.time()[3]
  posterior <- 
    RejectionABC(data = 2, 
                 prior = function(){runif(1, -10, 10)}, 
                 simulator = function(theta){rnorm(1, 2*(theta+2)*theta*(theta-2), 0.1+theta^2)}, 
                 n = 3000,
                 tolerance = epsilon)
  usedTime <- proc.time()[3] - startTime
  hist(posterior, breaks = seq(-3.5, 3.5, length.out = 60), ylim = c(0, 1000), 
       main = expression(paste("Posterior Density of ", theta)),
       xlab = "Posterior")
  text(-2, 1000, paste("Run time:", signif(usedTime, 3)))
  x <- seq(-3, 3, 0.01)
  y <- sapply(x, function(theta){dnorm(2, 2*(theta+2)*theta*(theta-2), 0.1+theta^2)})
  lines(x, 1500*y, col = "red", lty=2)
  legend(2, 1000, c("Rejection ABC","Real"), cex=0.8, 
         col=c("black","red"), lty=1:2)
}, epsilon = slider(1, 10, step = 0.5))
