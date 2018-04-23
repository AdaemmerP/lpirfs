#' Title
#'
#' @param yy A matrix with 'endogenous' variable
#' @param xx A matrix with lagged 'exogenous' variables
#' @param specs
#'
#' @return
#' @export
#'
#' @examples
find_optim_lags <- function(yy, xx, specs){

  # Regression parameters
  beta    <- (solve(crossprod(xx))%*%t(xx))%*%yy

  # Output
  resids  <- yy - xx%*%beta
  n       <- nrow(xx)
  ssr     <- sum(resids^2)
  var_eps <- ssr/n
  p       <- ncol(xx) + 1

  # Loglikelihood
  ll      <- - n/2 * log(2*pi) - n/2 * log(var_eps) - ssr/(2 * var_eps)


  switch(specs$lags_criterion,
         'AICc'= (2 * p - 2 * ll) + (2*p^2 + 2*p)/(n-p-1) ,
         'AIC' = 2 * p - 2 * ll,
         'BIC' = -2 * ll + log(n)*p)


}
