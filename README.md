
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lpirfs [![CRAN Version](https://www.r-pkg.org/badges/version/lpirfs)](https://CRAN.R-project.org/package=lpirfs)

[![Build
Status](https://travis-ci.com/AdaemmerP/lpirfs.svg)](https://travis-ci.com/AdaemmerP/lpirfs)
[![Downloads](https://cranlogs.r-pkg.org/badges/lpirfs)](https://CRAN.R-project.org/package=lpirfs)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/lpirfs?color=orange)](https://CRAN.R-project.org/package=lpirfs)
[![Coverage
Status](https://codecov.io/gh/adaemmerp/lpirfs/graph/badge.svg)](https://codecov.io/github/adaemmerp/lpirfs?branch=master)

# About

An R-package which estimates linear and nonlinear impulse responses with
local projections by [Jordà
(2005)](https://www.aeaweb.org/articles?id=10.1257/0002828053828518).

## Main features

  - Estimates linear and nonlinear impulse responses with local
    projections.
  - Estimates linear and nonlinear impulse responses with identified
    shock and/or with 2SLS.
  - Estimates linear and nonlinear impulse responses with local
    projections for panel data.
  - Functions to plot linear and nonlinear impulse responses.
  - Functions are partly implemented in *Rcpp*/*RcppArmadillo* and
    partly written for parallel computation to improve efficiency.

## Installation

You can install the released version of **lpirfs** from
[CRAN](https://CRAN.R-project.org):

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

## How to use

The paper about the package can be found
[here.](https://journal.r-project.org/archive/2019/RJ-2019-052/index.html)
The vignette of the package can be found
[here.](https://cran.r-project.org/package=lpirfs)

## Acknowledgements

I am thankful to Òscar
[Jordà](https://sites.google.com/site/oscarjorda/) for encouraging
comments and helpful suggestions. I am also indebted to Sarah
[Zubairy](https://sites.google.com/site/sarahzubairy/) for providing the
Matlab code before the publication of their
[paper](https://www.journals.uchicago.edu/doi/10.1086/696277).

I greatly benefitted from the helpful remarks by Jon
[Danielsson](http://www.systemicrisk.ac.uk/people/jon-danielsson) and
the profound *R*, *Rcpp* and *GitHub* knowledge of Philipp
[Wittenberg](https://github.com/wittenberg) and Detlef (overflow)
[Steuer](https://github.com/dsteuer). Last but not least, I am grateful
to Philipp Dybowski without whom I would have never started this
project.

All remaining errors are obviously mine.

### Author

Philipp Adämmer

### License

GPL (\>= 2)
