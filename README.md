
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.com/AdaemmerP/lpirfs.svg)](https://travis-ci.com/AdaemmerP/lpirfs)

lpirfs
======

An R-package which estimates linear and non-linear impulse responses with local projections by [Jordà (2005)](https://www.aeaweb.org/articles?id=10.1257/0002828053828518).

Main features
-------------

-   Estimates linear and non-linear impulse responses with local projections.
-   Functions to plot linear and non-linear impulse responses.
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

On Windows you need the [Rtools](https://cran.r-project.org/bin/windows/Rtools/) available from CRAN.

On macOS you need the the Clang 6.x compiler from [macOS tools](https://cran.r-project.org/bin/macosx/tools/).

Examples
--------

Examples can be found [here.](https://adaemmerp.github.io/lpirfs/README_docs.html)

Acknowledgements
----------------

I greatly benefitted from the profound *R*, *Rcpp* and *GitHub* knowledge of Philipp (stack) [Wittenberg](https://github.com/wittenberg) and Detlef (overflow) [Steuer](https://github.com/dsteuer). Remaining errors are obviously mine.

Development
-----------

I am working on a function to include exogenous variables and a function which allows to manually identify the linear combinations of the reduced form residuals.

### Author

Philipp Adämmer

### License

GPL (&gt;= 2)
