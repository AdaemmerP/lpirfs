#' @name create_lags
#' @title Compute a data frame with lagged exogenous variables
#' @description Create a \link{data.frame}() with lagged exogenous variables.
#' @param data A \link{data.frame}().
#' @param lags Integer for the number of lags.
#' @return Returns a \link{data.frame} with lagged values.
#' @import dplyr
#' @keywords internal
#' @author Philipp Adämmer

create_lags  <- function(data, lags){

  # Loop to construct lagged data
  for (i in 1:lags){

    lags_column  <- data %>%
                      dplyr::mutate_all(list(~dplyr::lag(., i)))       %>%
                      dplyr::rename_all(list(~paste0(.,"_", "lag_", i)))

              if(i == 1){

           lag_data <- lags_column

               } else {

           lag_data <- cbind(lag_data, lags_column)
        }
  }

  # Delete NAs
     lag_data <- lag_data

     return(lag_data)
}
