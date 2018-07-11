#' @name get_resids_ols
#' @title Compute residuals from OLS model
#' @description Estimate residuals from OLS model.
#' @param y_data A vector for the left hand variable.
#' @param x_data A matrix with right hand variables.
#' @return A numeric vector with residuals from OLS regression.
#' @keywords internal
#' @author Philipp Adämmer

get_resids_ols  <- function(y_data, x_data){

  # Build matrices
  yy           <- as.matrix(y_data)
  xx           <- as.matrix(x_data)
  xx           <- cbind(rep(1,nrow(xx)), xx)  # Add vector of ones for constant

  # Regression parameters
  beta    <- (solve(crossprod(xx))%*%t(xx))%*%yy

  # Residuals
  resids  <- yy - xx%*%beta

  return(resids)

}

