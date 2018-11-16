context("hp_filter")

# Load data
data_set_df <- interest_rules_var_data

# R function from mFilter package
hp_filter_r <- function(x, lambda){

  x         = as.matrix(x)

  n         = length(x)
  imat      = diag(n)
  Ln        = rbind(matrix(0, 1, n), diag(1, n - 1, n))
  Ln        = (imat - Ln) %*% (imat - Ln)
  Q         = t(Ln[3:n, ])
  SIGMA.R   = t(Q) %*% Q
  SIGMA.n   = diag(n - 2)
  g         = t(Q) %*% as.matrix(x)
  b         = solve(SIGMA.n + lambda * SIGMA.R, g)
  x.cycle   = c(lambda * Q %*% b)
  x.trend   = x - x.cycle


  A = lambda * Q %*% solve(SIGMA.n + lambda * SIGMA.R) %*%  t(Q)

  ret_results <- list(cycle = x.cycle, trend = x.trend)
  return(ret_results)
}


test_that("Test whether value hp-function is identical to function from mFilter package", {


  x         <- as.matrix(rnorm(100))
  results_r <- hp_filter_r(x, 1600)
  results_c <- hp_filter(x, 1600)

  testthat::expect_equal(as.matrix(results_r$cycle), results_c[[1]],  tolerance = .001)
  testthat::expect_equal(as.matrix(results_r$trend), results_c[[2]],  tolerance = .001)
})


