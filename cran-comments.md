# Submission notes

## Purpose
* Fixed an error in *lp_nl()* when lags are chosen by a lag length criterion. 
  The previous version (0.1.1) chooses lags based on linear instead of 
  nonlinear data.

* The dependency on the *vars* package is gone as I wrote my own function to estimate
  'AICc', 'AIC' and 'BIC'. 

* *lp_lin()* and *lp_nl()* now allow to include exogenous variables and exogenous variables
  with contemporaneous impact. 

* The functions *plot_lin_irfs()* and *plot_nl_irfs()* are deprecated and have been 
  replaced by *plot_lin()* and *plot_nl()*.

* Two new functions named *lp_lin_iv()* and *lp_nl_iv()* allow to estimate 
  impulse responses via instrument variables.

* Input names in *lp_lin* and *lp_nl* have been changed for consistency:

function | old input name | new input name
:--------|:-------------  |:------------- 
`lp_lin` | `lags_lin`     | `lags_endog_lin`
`lp_nl`  | `lags_lin`     | `lags_endog_lin`
`lp_nl`  | `lags_nl`      | `lags_endog_nl`
`lp_nl`  | `hp_filter`    | `use_hp`

 
## Test environments
* local x86_64-pc-linux-gnu (64-bit), R version 3.5.1
* ubuntu 14.04.5 (on travis-ci),      R version 3.4.4, R-oldrel, R-devel.
* win-builder 
* rhub (also checked for Oracle Solaris)

## Check results
There was 1 reoccurring NOTE:
  
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Philipp Adämmer <adaemmer@hsu-hh.de>'

Possibly mis-spelled words in DESCRIPTION:
  Auerbach (21:47)
  Gorodnichenko (21:60)
  Jordà (19:54)
  
### Response: 
The maintainer (Philipp Adämmer <adaemmer@hsu-hh.de>) is correct and the names 
are also spelled correctly.    

## Downstream dependencies
No errors, warnings, or notes were caused in other packages. I used devtools::revdep_check() to confirm. 
