# lpirfs 0.1.1
* The dependency to the *mFilter* package is removed.  The HP-filter is now written in C++ which 
vastly improves efficiency. 

* A problem with a C++ function is resolved so that the package can also be installed on Oracle Solaris. 

* Renamed functions:
original name | new name | notes 
:--------|:------------- |:----- 
`lm_function`       | `get_resids_ols` | 
`reduced_var`       | `get_mat_chol`|
`find_lag_c`        | `get_vals_lagcrit`|
`newey_west_c`      | `newey_west`|
`switching_series`  | `get_vals_switching`|
 


# lpirfs 0.1.9000
* This is the development version. 

# lpirfs 0.1.0
* Preparation for CRAN release.
