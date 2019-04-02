# Submission notes

## Purpose

* New input-checks for *lp_nl()*

* Fixed an error in *lp_lin_panel()* and *lp_nl_panel()* when a pooling model is specified.

* New checks in *lp_lin_panel()* and *lp_nl_panel()* to see whether shock variable has been dropped during estimation, 
  potentially because of co-linearity or identification issues. 


## Test environments
* local x86_64-pc-linux-gnu (64-bit), R version 3.5.1

* ubuntu 14.04.5 (on travis-ci),      R version 3.4.4, R-oldrel, R-devel.

* win-builder (devel) 

* rhub (for Oracle Solaris)

## Check results
0 errors ✔ | 0 warnings ✔ | 1 note 

checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    libs   5.3Mb
 [8s/20s]
  
This is compiled code in the libs/ directory.  
