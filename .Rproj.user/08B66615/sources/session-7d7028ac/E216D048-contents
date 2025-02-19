library(purrr)


# ***********************************************
# 1. Newton's method for logistic regression ---- 
# ***********************************************
newton_logistic <- function(beta0 = c(0.5, 0.5), x, y, tol = 1e-6, max_iter = 100) {
  
  beta_cur = beta0
  beta_history = gradient_vec = matrix(NA, nrow = length(beta0), ncol = max_iter )
  for (iter in 1:max_iter) {
    
    # store results
    beta_history[ ,iter] = beta_cur
    
    # ***************************************
    # Compute the gradient and hessian
    pi = 1/(1 + exp(- x %*% beta_cur ))
    
    gradient = t(x)  %*% (y - pi)
    
    hessian <- -1 * reduce(
      map(seq_along(y), ~ pi[.x] * (1 - pi[.x]) * tcrossprod(x[.x, ], x[.x, ])), `+` )
    
    gradient_vec[ ,iter] = gradient
    
    beta_new = beta_cur - solve(hessian) %*% gradient
    
    # ***************************************
    # Check stopping criterion
    
    if(max(abs(beta_new - beta_cur)) < tol){
      message("Converged in ", iter, " iterations.\n")
      break
    }
    
    # Update the solution
    beta_cur = beta_new 
  }
  
  
  # ***************************************
  # calculate standard errors
  information = - hessian
  se = sqrt(solve(information))
  
  # output
  return(list(solution     = beta_cur, 
              beta_history = beta_history[ , 1:iter],
              gradient     = gradient_vec[ , 1:iter],
              se           = se,
              information  = information, # final hessian matrix
              converged    = (iter < max_iter),
              niter        = iter))
}


# **********************************************
# 2. MM method for logistic regression      ----
# **********************************************
MM_logistic <- function(beta0 = c(0.5, 0.5), x, y, tol = 1e-6, max_iter = 100) {
  
  beta_cur = beta0
  beta_history  = matrix(NA, nrow = length(beta0), ncol = max_iter )
  p = length(beta0)
  
  for (iter in 1:max_iter) {
    
    # Store history
    beta_history[, iter] = beta_cur
    
    
    # ***************************************
    # Solve for beta using uniroot
    f_beta <- function(beta_j, wi, x_j, sum_YX, p) {
      term = sum(wi * exp(p*x_j*beta_j)) 
      return( - term + sum_YX )
    }
    
    # ***************************************
    # Compute current values
    
    pi = exp(x %*% beta_cur) / (1 + exp(x %*% beta_cur))  # Sigmoid function
    hessian <- -1 * reduce(
      map(seq_along(y), ~ pi[.x] * (1 - pi[.x]) * tcrossprod(x[.x, ], x[.x, ])), `+` )
    
    # ***************************************
    # minorize-maximization

    beta_new <- rep(NA, p)
    for (j in 1:p) {
      wi = pi * x[, j] * exp(-p*x[, j]*beta_cur[j]) # weight
      sum_YX = sum(y * x[, j])
      
      solve <- uniroot(f_beta, interval = c(-100, 100), 
                       wi = wi, x_j = x[, j], sum_YX = sum_YX, p = p)
      
      beta_new[j] <- solve$root
    }
    
    # ***************************************
    # Check convergence using gradient norm
    if (max(abs(beta_new - beta_cur)) < tol) {
      message("Converged in ", iter, " iterations.\n")
      break
    }
    
    beta_cur = beta_new
  }
  
  # ***************************************
  # calculate standard errors
  information = - hessian
  se = sqrt(solve(information))
  
  
  # output
  return(list(solution     = beta_cur, 
              beta_history = beta_history[ , 1:iter],
              information  = information,
              se           = se,
              converged    = (iter < max_iter),
              niter        = iter))
}




# ******************************************************
# 3. Quasi-Newton approach for logistic regression  ----
# ******************************************************
QuasiNewton_logistic <- function(beta0 = c(0.5, 0.5), x, y){
  
  # ***************************************
  # Define negative log-likelihood function
  neg_log_likelihood <- function(beta, x, y) {
    log_lik <- sum(y * (x %*% beta) - log(1 + exp(x %*% beta)))
    return(-log_lik)  
  }
  
  # ***************************************
  # Define gradient
  gradient <- function(beta, x, y) {
    pi <- 1 / (1 + exp(-x %*% beta))  # Predicted probability
    return(-t(x) %*% (y - pi))  # Gradient of negative log-likelihood
  }
  
  # ***************************************
  # Optimize using BFGS (Quasi-Newton)
  fit <- optim(par = beta0, x = x, y = y,
               fn = neg_log_likelihood, gr = gradient,
               method = "BFGS", hessian = T)
  
  
  # output
  return(list(solution     = fit$par, 
              se           = sqrt(solve(fit$hessian)),
              niter        = fit$counts["function"]))
}




# *******************************************
# 4. GLM method for logistic regression  ----
# *******************************************
GLM_logistic <- function(x, y){
  
  fit = glm(y ~ x[ ,2], family = binomial())
  
  return(list(solution   = fit$coefficients, 
              se         = sqrt(vcov(fit)) ,
              converged  = fit$converged,
              niter      = fit$iter))
}


