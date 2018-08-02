# lpirfs 0.2.1
## Improvements

* Fixed an error in *lp_nl* when lags are selected by a lag length criterion. 
  The previous version chose those optimal lags based on linear lagged data and not on 
  nonlinear lagged data.

* The dependency on the *vars* package is gone as I wrote my own function to estimate
  'AICc', 'AIC' and 'BIC'. 

* *lp_lin* and *lp_nl* now allows to include exogenous variables. 

* The functions *plot_lin_irfs* and *plot_nl_irfs* are deprecated and have been renamed
  to *plot_lin* and *plot_nl*.

* Two new functions named *lp_lin_iv* and *lp_nl_iv* now allow to estimate 
  impulse responses with local projections and an instrument variable approach

* One input name in *lp_nl* has been changed for consistency:

function | old input name | new input name
:--------|:------------- 
`lp_nl`  | `hp_filter`      | `use_hp`





# lpirfs 0.1.1
## Improvements

* The dependency to the *mFilter* package is removed.  The HP-filter is now written in C++ which 
vastly improves efficiency. 

* A problem with a C++ function is resolved so that the package can also be installed on Oracle Solaris. 

* Renamed functions:

original name | new name |
:--------|:------------- 
`lm_function`       | `get_resids_ols` 
`reduced_var`       | `get_mat_chol`
`find_lag_c`        | `get_vals_lagcrit`
`newey_west_c`      | `newey_west`
`switching_series`  | `get_vals_switching`
 

# lpirfs 0.1.0
* Preparation for CRAN release.
