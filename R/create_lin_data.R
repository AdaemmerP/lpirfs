#' Function to create data for linear model
#'
#' @param specs A list with specifications that go into 'lp_lin' function.
#'        specs$lags_criterion: Either NaN (given lag length) or 'BIC'|'AIC'
#'        specs$lags_lin:       Lag length
#'        specs$trend:          1 (no trend), 2 (trend), 3 (quadratic trend)
#'        specs$max_lags:       Maximum number of lags to use for lag length criteria
#' @param data_set A data frame with all endogenous variables.
#' @return List with output from lm object.



create_lin_data     <- function(specs, data_set){

 # Check whether lag length is given or has to be found based on criteria
  if (is.nan(specs$lags_criterion) == TRUE) {

    # Select data for endogenous variables
    y_lin <- data_set[(specs$lags_lin + 1):dim(data_set)[1],]


    # Make exogenous lagged data
    x_lin <- create_lags(data_set, specs$lags_lin) %>%
      na.omit()

    # Include no trend, trend or quadratic trend?
    switch(specs$trend,
           x_lin            <-   x_lin,
           x_lin            <-   x_lin                           %>%
             dplyr::mutate(trend = row_number()),
           x_lin            <-   x_lin                           %>%
             dplyr:: mutate(trend = row_number())  %>%
             dplyr::mutate(sq_trend = trend^2))


  } else {

    # ---  Make lag data based on max lag lengths
    y_lin     <- list()
    x_lin     <- list()

    for(ii in 1:specs$max_lags){
      y_lin[[ii]] <-  data_set[(ii + 1):dim(data_set)[1],]
      x_lin[[ii]] <-  dplyr::as_tibble(create_lags(data_set, ii)) %>%
        na.omit()

      # --- Include no trend, trend or quadratic trend
      switch(specs$trend,

             x_lin[[ii]]            <-   x_lin[[ii]],
             x_lin[[ii]]            <-   x_lin[[ii]]                %>%
               dplyr::mutate(trend = row_number()),
             x_lin[[ii]]            <-   x_lin[[ii]]                %>%
               dplyr::mutate(trend = row_number())   %>%
               dplyr::mutate(sq_trend = trend^2))


    }
  }

  # Return list with exogenous and endogenous data
  list(y_lin, x_lin)

}
