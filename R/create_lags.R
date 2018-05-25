#' @name create_lags
#' @title Creates a data frame with lagged exogenous variables.
#' @description Creates a data frame with lagged exogenous variables.
#' @param data A data frame.
#' @param lags Integer for the number of lags.
#' @return A data frame with lagged values from 'data'.
#' @import dplyr
#' @author Philipp Adämmer

create_lags  <- function(data, lags){

  # Loop to construct lagged data
  for (i in 1:lags){

    lags_column  <- data %>%
                      dplyr::mutate_all(funs(lag(., i)))       %>%
                      dplyr::rename_all(funs(paste0(.,"_", "lag_", i)))

              if(i == 1){

           lag_data <- lags_column

               } else {

           lag_data <- cbind(lag_data, lags_column)
        }
  }

  # Delete NAs
     lag_data <- lag_data %>%
                            stats::na.omit()
     return(lag_data)
}
