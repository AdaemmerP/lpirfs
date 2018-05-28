
<!-- README.md is generated from README.Rmd. Please edit that file -->
lpirfs
======

An R-package which estimates linear and non-linear impulse responses with local projections by [Jordà (2005)](https://www.aeaweb.org/articles?id=10.1257/0002828053828518).

Main features
-------------

-   Estimate linear and non-linear impulse responses with local projections.
-   Functions to plot linear and non-linear impulse responses.
-   Functions are partly implemented in *Rcpp* and *RcppArmadillo* to improve efficiency.
-   High performance with parallel computation.

Installation
------------

You can install the released version of **lpirfs** from [CRAN](https://CRAN.R-project.org):

``` r
install.packages("lpirfs")
```

And the development version from [GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("AdaemmerP/lpirfs")
```

Description and examples
------------------------

A detailed description and examples can be found [here.](https://adaemmerp.github.io/lpirfs/README_docs.html)

Acknowledgements
----------------

I greatly benefitted from the profound *R*, *Rcpp* and *Github* knowledge of [Philipp Wittenberg](https://github.com/wittenberg) and [Detlef (overflow) Steuer](https://github.com/dsteuer). All remaning mistakes are obviously mine.

Development
-----------

I am working on a function to include exogenous variables and a function that allows to identify the shocks besides the Cholesky decomposition.

### Author

Philipp Adämmer

### License

GPL (&gt;= 2)
