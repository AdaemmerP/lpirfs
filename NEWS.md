# lpirfs 0.1.2

* Fixed an error in `lp_nl()` when lags are chosen by a lag length criterion. 
  Version 0.1.1 chooses lags based on linear instead of 
  nonlinear data.

* Removed the dependency on the *vars* package. 

* `lp_lin()` and `lp_nl()` now allow to include exogenous variables and exogenous variables
  with contemporaneous impact. 

* `plot_lin_irfs()` and `plot_nl_irfs()` are deprecated and have been 
  replaced by `plot_lin()` and `plot_nl()`.

* Two new functions named `lp_lin_iv()` and `lp_nl_iv()` allow to estimate 
  linear and nonlinear impulse responses with identified shocks (instrument variables).

* Input names in `lp_lin()` and `lp_nl()` have been changed for consistency:

function | old input name | new input name
:--------|:-------------  |:------------- 
`lp_lin()` | `lags_lin`     | `lags_endog_lin`
`lp_nl()`  | `lags_lin`     | `lags_endog_lin`
`lp_nl()`  | `lags_nl`      | `lags_endog_nl`
`lp_nl()`  | `hp_filter`    | `use_hp`



# lpirfs 0.1.1

* The dependency on the *mFilter* package is removed. `hp_filter()` is now written in C++ to improve efficiency. 

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
