
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- # lpirfs [![CRAN Version](https://www.r-pkg.org/badges/version/lpirfs)](https://CRAN.R-project.org/package=lpirfs) -->

<!-- [![Downloads](https://cranlogs.r-pkg.org/badges/lpirfs)](https://CRAN.R-project.org/package=lpirfs) -->

<!--[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/lpirfs?color=orange)](https://CRAN.R-project.org/package=lpirfs) -->

<!--[![Build Status](https://travis-ci.com/AdaemmerP/lpirfs.svg)](https://travis-ci.com/AdaemmerP/lpirfs)  -->

<!--[![Coverage Status](https://codecov.io/gh/adaemmerp/lpirfs/graph/badge.svg)](https://codecov.io/github/adaemmerp/lpirfs?branch=master) -->

# About

An R-package which estimates linear and nonlinear impulse responses with
local projections by [Jordà
(2005)](https://www.aeaweb.org/articles?id=10.1257/0002828053828518).

## Main features

  - Estimates linear and nonlinear impulse responses with local
    projections.
  - Estimates linear and nonlinear impulse responses with local
    projections and instrument variables (development version).
  - Functions to plot linear and nonlinear impulse responses.
  - Functions are partly implemented in *Rcpp* and *RcppArmadillo* to
    improve efficiency.
  - High performance with parallel computation.

## Updates and new features in the development version

  - Fixed an error in *lp\_nl* when the number of lags is chosen by a
    lag length criterion. The version on CRAN (0.1.1) chooses lags based
    on linear data instead of nonlinear lagged data.

  - All functions now allow to include (lagged) exogenous data and data
    with contemporaneous impact.

  - Two new functions allow to estimate linear (*lp\_lin\_iv*) and
    nonlinear (*lp\_nl\_iv*) impulse responses with local projections
    via instrument variables as in, e.g., [Ramey and Zubairy
    (2018)](https://www.journals.uchicago.edu/doi/10.1086/696277).

  - All functions now allow to manually choose the number of cores.

  - Some input names in *lp\_lin* and *lp\_nl* have been changed for
    consistency:

| function   | old input name | new input name   |
| :--------- | :------------- | :--------------- |
| `lp_lin()` | `lags_lin`     | `lags_endog_lin` |
| `lp_nl()`  | `lags_lin`     | `lags_endog_lin` |
| `lp_nl()`  | `lags_nl`      | `lags_endog_nl`  |
| `lp_nl()`  | `hp_filter`    | `use_hp`         |

## Examples

Examples can be found
[here.](https://adaemmerp.github.io/lpirfs/README_docs.html)

## Installation

You can install the released version (0.1.1) of **lpirfs** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("lpirfs")
```

You can install the development version of **lpirfs** from
[GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("AdaemmerP/lpirfs")
```

The package compiles some C++ source code for installation, which is why
you need the appropriate compilers:

On Windows you need
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) available from
CRAN.

On macOS you need the Clang 6.x compiler and the GNU Fortran compiler
from [macOS tools](https://cran.r-project.org/bin/macosx/tools/). Having
installed the compilers, you need to open a terminal and start R via
‘PATH=/usr/local/clang6/bin:$PATH R’. Yo can then install the package
via *devtools::install\_github(“AdaemmerP/lpirfs”)*

## Acknowledgements

I am thankful to Òscar
[Jordà](https://sites.google.com/site/oscarjorda/) for encouraging
comments and helpful suggestions. I am also indebted to Sarah
[Zubairy](https://sites.google.com/site/sarahzubairy/) for providing the
Matlab code before the publication of their
[paper](https://www.journals.uchicago.edu/doi/10.1086/696277).

I greatly benefit from the profound *R*, *Rcpp* and *GitHub* knowledge
of Philipp [Wittenberg](https://github.com/wittenberg) and Detlef
(overflow) [Steuer](https://github.com/dsteuer). Last but not least, I
am grateful to Philipp Dybowski for his rigorous comments and without
whom I would have never started this project.

All remaining errors are obviously mine.

## Development

I intend to further extend the package with functions that

  - allow to manually identify the linear combinations of the reduced
    form residuals,
  - conduct panel-lp estimation.

### Author

Philipp Adämmer

### License

GPL (\>= 2)
