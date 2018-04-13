#' OLS function
#'
#' @param y_vector A vector for the left hand variable.
#' @param x_lin A data frame with exogenous variables.
#' @return List with output from lm object.



lm_function  <- function(y_vector, x_data){

                    data_lm           <- cbind(y = y_vector, x_data)
                    names(data_lm)[1] <- 'y'
                    lm_output         <- lm(y ~ ., data = data_lm )

}

