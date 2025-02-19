make_data <- function(n, beta){
  x <- cbind(1, rnorm(n, mean = 0, sd = 1))
  y <- rbinom(n, size = 1, prob = plogis(x %*% beta))
  
  list(x = x, y = y)
}
