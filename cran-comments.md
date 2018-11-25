# Submission notes

## Purpose
* Added two functions to estimate linear and nonlinear irfs for panel data.

* Added an option whether to use lagged values of the switching values in 
  all nonlinear models. 
  
* Added an option to use a dummy approach for the nonlinear models. 


## Test environments
* local x86_64-pc-linux-gnu (64-bit), R version 3.5.1

* ubuntu 14.04.5 (on travis-ci),      R version 3.4.4, R-oldrel, R-devel.

* win-builder (devel) 

* rhub (for Oracle Solaris)

## Check results
0 errors ✔ | 0 warnings ✔ | 1 notes 

checking installed package size ... NOTE
  
This is compiled code in the libs/ directory.  

## Downstream dependencies
No errors, warnings, or notes were caused in other packages. I used `devtools::revdep_check()` to confirm. 
