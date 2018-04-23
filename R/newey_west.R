#' Title
#'
#' @param yy Matrix
#' @param xx Matrix
#'
#' @return List with betas and nwerrors
#'
#'
newey_west <- function(yy, xx){

  nlag    <- floor(nrow(xx)^(1/4))

  # Regression parameters
  xpxi    <- chol2inv(chol(crossprod(xx)))
  beta    <- xpxi%*%t(xx)%*%yy

  # Output
  resids    <- yy - xx%*%beta
  nobs      <- nrow(xx)
  num_exog  <- ncol(xx)

  # Matrices
  emat      <- t(replicate(num_exog, resids, simplify = 'matrix'))
  hhat      <- emat*t(xx)

  # C++ function for while loop
  G         <- nw_weight(hhat, nobs, num_exog, nlag)

  V         <- xpxi%*%G%*%xpxi
  nwerr     <- sqrt(diag(V))
  list(coefs = beta, nwerr = nwerr)

}
