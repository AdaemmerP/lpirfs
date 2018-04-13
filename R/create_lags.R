#' Creates a data frame with lagged exogenous variables.
#'
#' @param data A data frame with variables in columns.
#' @param lags Numeric value for the number of lags.
#' @return lag_data: A data frame with lagged values from 'data'.

create_lags  <- function(data, lags){

  for (i in 1:lags){

        lags_column  <- data %>%
                          dplyr::mutate_all(funs(lag(., i))) %>%
                          dplyr::rename_all(funs(paste0(.,"_", "lag_", i)))

        ifelse(i == 1, lag_data <- lags_column, '')

        ifelse(i > 1,  lag_data <- cbind(lag_data, lags_column), '')

  }

  # Delete NAs
  lag_data <- lag_data %>%
                na.omit()
}
