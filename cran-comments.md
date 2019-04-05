# Submission notes

## Purpose

* Fixed a bug in *lp_lin_panel()* and *lp_nl_panel()* when switching variable is used with logistic function. 

* Fixed a bug in *lp_lin_panel()* and *lp_nl_panel()* regarding the construction of the 
  endogenous and exogenous variables for panel estimation.

* Fixed a bug in *lp_lin_panel()* and *lp_nl_panel()* when a pooling model is specified.

* New input-checks for *lp_nl()*

* New checks in *lp_lin_panel()* and *lp_nl_panel()* to see whether shock variable has been dropped during estimation, 
  potentially because of co-linearity or identification issues. 
  
* *lp_nl_panel()* now returns the (transformed) switching variable as a *tibble* along with the original data
  for comparabaility.


## Test environments
* local x86_64-pc-linux-gnu (64-bit), R version 3.5.2

* ubuntu 14.04.5 (on travis-ci),      R version 3.4.4, R-oldrel, R-devel.

* rhub::check_for_cran()

## Check results
0 errors ✔ | 0 warnings ✔ | 1 note 

checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    libs   5.3Mb
 [8s/20s]
  
This is compiled code in the libs/ directory.  
