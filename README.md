README
================
Xiaxian Ou

# Folder description

*simulations*

`simulations/run_simulation.R`

- set simulation design elements for one simulation
- make simulation data by `seed = 82`
- compare algorithms
- save the simulation results in folder `data`

*source*

`source/1_make_data.R`

- `make_data` function to get simulation data by true beta for logistics
  regression

`source/2_estimate_methods.R`

- `newton_logistic` for Newton’s method.
- `MM_logistic` for MM method. here, I can not get the solution of
  $\theta_{new}$ directly by solving
  $\frac{\partial g(\theta|\theta^{(k)})}{\partial \theta_j} = 0$, thus
  I just use `uniroot` to get the numerical solution to update
  $\theta_{new}$.
- `QuasiNewton_logistic` for Quasi-Newton approach
- `GLM_logistic` for GLM in R.

`source/3_summarize.R`

- `summarize_method` summarizes the results from methods in
  `source/2_estimate_methods.R`. Return estimates, confidence interval,
  iteration number and computation time.

`source/4_EM.R`

- `EM_censored_exp`: EM algorithm for censored exponential data
- `EM_boot`: bootstrapping to get confidence interval (quantile) for
  EM_censored_exp.

*data*

- simulation results (.Rdata)

*analysis*

- Rmarkdown file to analyze the results
- pdf file rendered from Rmarkdown

*results*

- plots for simulation
