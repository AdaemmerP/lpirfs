#' Function to estimate VAR by OLS: equation for equation.
#'
#' @param y_data A data frame for the exogenous variables.
#' @param x_lin A data frame with exogenous variables.
#' @param lags Numeric value with number of lags.
#' @return List with output from lm object.



lm_function  <-  function(y_data, x_lin, specs){

  if(specs$lags_criterion == ''){
    data_lm       <- cbind(y = y_data, x_lin)

                } else {

    data_lm   <- cbind(y = y_data, x_lin[[specs$lags_lin]])}

  names(data_lm)[1] <- 'y'

  lm_output         <- lm(y ~ ., data = data_lm )


}
