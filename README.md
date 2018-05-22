
<!-- README.md is generated from README.Rmd. Please edit that file -->
lpirfs
======

An R-package which estimates linear and non-linear impulse responses with local projections by Jordà (2005).

Main features
-------------

-   Linear impulse responses with local projections
-   Non-linear impulse responses with local projections
-   Functions to plot impulse responses

Installation
------------

You can install the released version of **lpirfs** from [CRAN](https://CRAN.R-project.org) or from github:

``` r
install.packages("lpirfs")
# install.packages("devtools")
devtools::install_github("AdaemmerP/lpirfs")
```

Example: Linear impulse responses
---------------------------------

Load libraries:

``` r
# Load packages
  library(dplyr)
  library(doSNOW)
  library(parallel)
  library(Rcpp)
  library(lpirfs)
```

Load data set from package

``` r
# Load data (from package)
  data_set_df <- interest_rules_var_data
```

Make list for input

``` r
# Create list for input
  specs <- list()

# Specify inputs
  specs$lags_lin       <- 4L
  specs$lags_criterion <- NaN
  specs$max_lags       <- 2L
  specs$trend          <- 0L
  specs$shock_type     <- 1L
  specs$confint        <- 1.96
  specs$hor            <- 12L
```

Make list for input

``` r
# Estimate model 
  results_lin  <- lp_lin(data_set_df, specs)
```

``` r
# Make plots
  linear_plots <- plot_lin_irfs(results_lin)


  
# Show single plots
  linear_plots[[1]]
```

![](README-unnamed-chunk-7-1.png)

``` r
  linear_plots[[2]]
```

![](README-unnamed-chunk-7-2.png)

``` r

# Show all plots
  library(ggpubr)
  library(gridExtra)

  lin_plots_all <- sapply(linear_plots, ggplotGrob)
  marrangeGrob(lin_plots_all, nrow = ncol(data_set_df), ncol = ncol(data_set_df))
```

![](README-unnamed-chunk-7-3.png)
