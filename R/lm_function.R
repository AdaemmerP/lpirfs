#' lm function which returns OLS parameter estimates
#'
#' @param y_data A vector for the left hand variable.
#' @param x_data A data frame with exogenous variables.
#' @return List with output from lm object.



lm_function  <- function(y_data, x_data){

  yy           <- as.matrix(y_data)
  xx           <- as.matrix(x_data)


  lm_output    <- lm(yy ~ xx)

}

