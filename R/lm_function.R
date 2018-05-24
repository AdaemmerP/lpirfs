#' @name lm_function
#' @title Estimate residuals from OLS model.
#' @param y_data A vector for the left hand variable.
#' @param x_data A data frame with exogenous variables.
#' @return Vector with residuals from OLS regression.
#' @author Philipp Adämmer

lm_function  <- function(y_data, x_data){

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

