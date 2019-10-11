# lpirfs 0.1.6

* New S3 methods to use *summary()* and *plot()*, for all main functions

* More flexible options for the HAC estimators, such as pre-whitening, sample 
  size adjustment, etc. 

* New vignette

* Updated documentation


# lpirfs 0.1.5

* Fixed a bug for *lp_lin_panel()* and *lp_nl_panel()* regarding the construction of the 
  endogenous and exogenous variables.

* Fixed a bug for *lp_nl_panel()* when using the switching variable. 

* Fixed a bug for *lp_lin_panel()* and *lp_nl_panel()* when a pooling model is specified.

* New checks for *lp_lin_panel()* and *lp_nl_panel()* to see whether shock variable has been dropped during estimation, 
  potentially because of co-linearity or identification issues.
  
* *lp_nl_panel()* now returns the (transformed) switching variable as a *tibble* along with the original data
  for comparabaility.
  
* Updated documentation.  

* Updated examples.
  

# lpirfs 0.1.4

* Added two functions to estimate linear and nonlinear irfs for panel data.

* Added an option to choose whether to use lagged values of the switching variable in 
  nonlinear models. 

* Added an option to use a dummy approach for the nonlinear models.

* Deleted messages about how models are estimated (e.g. with or without exogenous data).

* Changed input name in `lp_lin_iv()` for consistency:

function | old input name | new input name
:--------|:-------------  |:------------- 
`lp_lin_iv()` | `twosls`     | `use_twosls`



# lpirfs 0.1.3

* Included an option to conduct 2SLS with *lp_lin_iv*.

* Included an option to set the lag length of the endogenous variables in *lp_lin_iv* and *lp_nl_iv* to zero.

* Deleted the deprecated plot functions *plot_lin_irfs* and *plot_nl_irfs*.

* Improved documentation.

* Changed input names in `lp_lin_iv()` and `lp_nl_iv()` for consistency:

function | old input name | new input name
:--------|:-------------  |:------------- 
`lp_lin_iv()` | `instr`     | `shock`
`lp_nl_iv()`  | `instr`     | `shock`


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
* First CRAN release.
