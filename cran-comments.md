# Submission notes

## Purpose
- Deleted a dependency problem from the soon to be archived (2018-08-20) 'mFilter' package. I wrote a C++ function ('hp_filter_c.cpp')
  based on a function in 'mFilter'. The author 'Mehmet Balcilar' is now mentioned in the DESCRIPTION file as a contributor ('ctb'):
  The problem was:
  > Thus, package mFilter is now scheduled for archival on 2018-07-20, and archiving this will necessitate also
    archiving its strong reverse dependencies. Please negotiate the necessary actions.
  
- Fixed a problem in a C++-function as it lead to an eror ('ambiguity problem') when installed on Solaris:  
  > Your C++ error was warned about in §1.6.4 of the manual: do check for similar occurrences.
  
## Test environments
* local x86_64-pc-linux-gnu (64-bit), R version 3.5.1
* ubuntu 14.04.5 (on travis-ci),      R version 3.4.4, R-oldrel, R-devel.
* win-builder 
* rhub (also checked for Solaris)


## Check results
There was 1 NOTE in all checks:
  
  * checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Philipp Adämmer <adaemmer@hsu-hh.de>’

  New submission

  Possibly mis-spelled words in DESCRIPTION:
    Auerbach (15:47)
    Gorodnichenko (15:60)
    Jordà (13:54)
  
### Response: 
The maintainer (Philipp Adämmer <adaemmer@hsu-hh.de>) is correct and the names 
are also spelled correctly.    

## Downstream dependencies
  > No ERRORs or WARNINGs found :)
