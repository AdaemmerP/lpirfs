# Submission notes

## Purpose


* Resolved an integer/double issue with an Rcpp function so that the package can also be installed on Oracle Solaris. 

* Added the option to show how many lags are chosen when a lag length criterion is selected
  

  

## Test environments
* local x86_64-pc-linux-gnu (64-bit), R version 3.6.1

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
