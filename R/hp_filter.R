#' @name hp_filter
#' @title Detrend a times series via the Hodrick-Prescott filter
#' @description Estimates cyclicyal and trend component with HP-filter by Hodrick and Prescott (1997).
#'
#' @param x A something.
#' @param lambda A number.
#'
#' @return ret_results A list with results.
#' @references
#'  Hodrick, R.J., and Prescott, E. C. Prescott (1997) "Postwar U.S. Business Cycles: An Empirical Investigation."
#'  \emph{Journal of Money, Credit and Banking}, 29(1), 1-16.
#' @export
hp_filter <- function(x, lambda){

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

  ret_results <- list(cycle = x.cycle, trend = x.trend, fmatrix = A)
  return(ret_results)
}
