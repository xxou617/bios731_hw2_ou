#####################################################################################
# Julia Wrobel
# January 2025
#
# This file produces implements an EM algorithm for a two-part Gaussian MM
#####################################################################################

# theta0: vector of initial values
# y: observed data
em_gmm = function(theta0,  y, tol = 1e-12, max_iter = 200){
  mu1 = theta0[1]
  mu2 =  theta0[2]
  sigma1 = theta0[3]
  sigma2 = theta0[4]
  lambda = theta0[5]

  n = length(y)
  iter = 1
  tol_criteria = Inf

  # define vectors to store elements of interest
  observed_ll  = rep(NA, length = max_iter)

  while(iter < max_iter  & tol_criteria > tol){

    ###############################################################
    ## E-step
    ###############################################################

    # estimate z and calculate Q
    # normal density for first mixture
    f1 = dnorm(y, mean = mu1, sd = sqrt(sigma1), log = FALSE)
    f2 = dnorm(y, mean = mu2, sd = sqrt(sigma2), log = FALSE)

    # compute current value of Q function and pi
    # don't actually need Q function current estimate for M-step, by the way
    pi =  lambda * f1 / (lambda * f1 + (1-lambda) * f2)

    ###############################################################
    ## M-step
    ###############################################################

    mu1 = sum(pi * y)/sum(pi)
    mu2 = sum((1-pi)*y)/sum(1-pi)

    sigma1 = sum(pi * (y - mu1)^2)/sum(pi)
    sigma2 = sum((1-pi)*(y - mu2)^2)/sum(1-pi)

    lambda = sum(pi)/n

    ###############################################################

    ## set up elements to define observed data likelihood
    f1 = dnorm(y, mean = mu1, sd = sqrt(sigma1))
    f2 = dnorm(y, mean = mu2, sd = sqrt(sigma2))

    ## define log likelihood at current iteration
    observed_ll[iter] = sum(log(lambda * f1 + (1-lambda) * f2))

    ## check convergence criteria and increase iteration

    if(iter > 1){
      tol_criteria = observed_ll[iter] - observed_ll[iter-1]
    }
    iter = iter + 1
    message(paste0("iteration: ", iter, "; ll: ", round(tol_criteria, 4)))
  }



  ### return parameters of interest
  list(solution = c(mu1, mu2, sigma1, sigma2, lambda),
       iterations = iter,
       observed_ll = observed_ll[1:iter])

}
