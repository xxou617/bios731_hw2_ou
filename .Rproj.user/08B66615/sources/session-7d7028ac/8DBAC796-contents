# ****************************************************************
# Homework 2 
# This file produces simulations for estimating logistic regression
# use four different methods
# ****************************************************************

library(tictoc)
library(tidyverse)

# ***************************************************
# define or source functions used in code below
# **************************************************

source(here::here("source", "1_make_data.R"))
source(here::here("source", "2_estimate_methods.R"))
source(here::here("source", "3_summarize.R"))


# ***************************************************
# set simulation design elements
# ***************************************************

nsim  = 1
n     = 200
beta_true = c(1, 0.3)

# ***************************************************
# start simulation code
# ***************************************************

# only for 1 simualtion 
seed = floor(runif(nsim, 1, 100))
set.seed(seed)  # seed = 82

# ++++++++++++++++++++++++++++++
##  generate data --------------

simdata <- make_data(n, beta_true)
x <- simdata$x
y <- simdata$y

# ++++++++++++++++++++++++++++++
##  apply methods --------------

beta0 = c(0.5, 0.5)

## Newton’s method  
tic()
fit_newton  = newton_logistic(beta0, x, y)
time_newton = toc()

## MM’s method
tic()
fit_MM  = MM_logistic(beta0, x, y)
time_MM = toc()

## GLM’s method
tic()
fit_GLM  = GLM_logistic(x, y)
time_GLM = toc()

# Quasi-Newton
tic()
fit_QuasiNewton  = QuasiNewton_logistic(beta0, x, y)
time_QuasiNewton = toc()


# ++++++++++++++++++++++++++++++
##  summarize   ----------------

# results for each method
out_newton <- summarize_method(fit_newton, method = "newton", time = time_newton$toc - time_newton$tic)
out_MM <- summarize_method(fit_MM, method = "MM", time = time_MM$toc - time_MM$tic)
out_GLM <- summarize_method(fit_GLM, method = "GLM", time = time_GLM$toc - time_GLM$tic)
out_QuasiNewton <- summarize_method(fit_QuasiNewton, method = "QuasiNewton", time = time_QuasiNewton$toc - time_QuasiNewton$tic)


# overall 
simu_out <- rbind(out_newton, out_MM, out_GLM, out_QuasiNewton) |> 
  mutate(true = if_else(beta == "est_beta0", beta_true[1], beta_true[2]))


# ++++++++++++++++++++++++++++++
##  save        ----------------

save(simu_out, file = here::here("data", "simu_out.Rdata"))
