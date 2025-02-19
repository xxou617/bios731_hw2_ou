
# summarize ouput from different methods
summarize_method <- function(method_output, method = "newton", time){
  
  beta_est <- as.numeric(method_output$solution)
  se <- sqrt(diag(method_output$se))  # Ensure se is in correct format
  niter <- method_output$niter
  
  # Create data frame
  data.frame(
    beta     = c("est_beta0", "est_beta1"),
    estimate = beta_est,
    CI.lower = beta_est - 1.96 * se,
    CI.up    = beta_est + 1.96 * se,
    iter.number = niter,
    methods   = method,
    times     = time,
    row.names = NULL
  )
  
}
