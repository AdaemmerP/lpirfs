
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.com/AdaemmerP/lpirfs.svg)](https://travis-ci.com/AdaemmerP/lpirfs) [![Coverage Status](https://codecov.io/gh/adaemmerp/lpirfs/graph/badge.svg)](https://codecov.io/github/adaemmerp/lpirfs?branch=master)

lpirfs
======

An R-package which estimates linear and nonlinear impulse responses with local projections by [Jordà (2005)](https://www.aeaweb.org/articles?id=10.1257/0002828053828518).

Main features
-------------

-   Estimates linear and nonlinear impulse responses with local projections.
-   Functions to plot linear and nonlinear impulse responses.
-   Functions are partly implemented in *Rcpp* and *RcppArmadillo* to improve efficiency.
-   High performance with parallel computation.

Installation
------------

You can install the development version of **lpirfs** from [GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("AdaemmerP/lpirfs")
```

The package compiles some C++ source code for installation, which is why you need the appropriate compilers:

On Windows you need [Rtools](https://cran.r-project.org/bin/windows/Rtools/) available from CRAN.

On macOS you need the Clang 6.x compiler and the GNU Fortran compiler from [macOS tools](https://cran.r-project.org/bin/macosx/tools/). Having installed the compilers, you need to open a terminal and start R via 'PATH=/usr/local/clang6/bin:$PATH R'. Yo can then install the package via *devtools::install\_github("AdaemmerP/lpirfs")*

Examples
--------

Examples can be found [here.](https://adaemmerp.github.io/lpirfs/README_docs.html)

Acknowledgements
----------------

I greatly benefit from the profound *R*, *Rcpp* and *GitHub* knowledge of Philipp (stack) [Wittenberg](https://github.com/wittenberg) and Detlef (overflow) [Steuer](https://github.com/dsteuer). Remaining errors are obviously mine.

Development
-----------

I am working on a function to include exogenous variables and a function which allows to manually identify the linear combinations of the reduced form residuals.

### Author

Philipp Adämmer

### License

GPL (&gt;= 2)
