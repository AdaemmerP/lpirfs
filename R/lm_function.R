#' Creates a data frame with lagged exogenous variables.
#'
#' @param data A data frame.
#' @param lags numeric: The number of lags.
#' @return A data frame.



lm_function  <-  function(m){

  if(specs$lags_criterion == ''){
    data_lm       <- cbind(y = m, x_lin)

                } else {

    data_lm   <- cbind(y = m, x_lin[[specs$lags_lin]])}

  names(data_lm)[1] <- 'y'

  lm_output         <- lm(y ~ ., data = data_lm )


}
