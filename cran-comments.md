# Submission notes

## Purpose

* Disabled the possibility *lags_exog* = 0 for functions `lp_lin`, `lp_nl`, `lp_lin_iv` and `lp_nl_iv`. Solely *contemp_data* should be used for exogenous data with contemporaneous impact. 

* Updated documentation for functions `lp_lin`, `lp_nl`, `lp_lin_iv` and `lp_nl_iv` regardings *exog_data* and *lags_exog*.  
  
  

## Test environments
* local x86_64-pc-linux-gnu (64-bit)
* Running under: Debian GNU/Linux 11 (bullseye)
* R version 4.2.2
* rhub::check_for_cran()



