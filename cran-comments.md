# Submission notes

## Purpose
* Deleted *mFilter* from *Imports* in the DESCRIPTION file as the package will be archived on 2018-07-20. 
  I wrote a C++ function based on the *hpfilter* function from the *mFilter* package.  The author of *mFilter* now appears in the 
  DESCRIPTION file as a contributor.

* Fixed a C++ error (§1.6.4 of the manual) which occurred when attempting to build the package on Oracle Solaris.

* Improved the *Help pages* as it now only shows those functions that are useful to the user.
  
* Renamed non-exported functions for clarity. Changes are outlined in NEWS.md.  
 
## Test environments
* local x86_64-pc-linux-gnu (64-bit), R version 3.5.1
* ubuntu 14.04.5 (on travis-ci),      R version 3.4.4, R-oldrel, R-devel.
* win-builder 
* rhub (also checked for Oracle Solaris)

## Check results
There was 1 NOTE in all checks:
  
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Philipp Adämmer <adaemmer@hsu-hh.de>'

Days since last update: 4

Possibly mis-spelled words in DESCRIPTION:
  Auerbach (21:47)
  Gorodnichenko (21:60)
  Jordà (19:54)
  
### Response: 
The maintainer (Philipp Adämmer <adaemmer@hsu-hh.de>) is correct and the names 
are also spelled correctly.    

## Downstream dependencies
No errors, warnings, or notes were caused in other packages. I used devtools::revdep_check() to confirm. 
