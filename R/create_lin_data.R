#' Function to create data for linear model.
#'
#' @param specs A list with specifications that go into 'lin_lp' or 'nl_lp' function.
#'        specs$lags_criterion: Either NaN (given lag length) or 'AICc'|'AIC'|'BIC'|
#'        specs$lags_lin:       Lag length
#'        specs$trend:          1 (no trend), 2 (trend), 3 (quadratic trend)
#'        specs$max_lags:       Maximum number of lags to use for lag length criteria
#' @param data_set_estim A data frame with all endogenous variables from VAR.
#' @export
#' @return List with left (y_lin) and right hand side (x_lin) variables
#' @author Philipp Adämmer


create_lin_data     <- function(specs, data_set_estim){

  if (is.nan(specs$lags_criterion) == TRUE) {

    # Select data for endogenous variables
    y_lin <- data_set_estim[(specs$lags_lin + 1):dim(data_set_estim)[1],]


    # Make exogenous lagged data
    x_lin <- create_lags(data_set_estim, specs$lags_lin) %>%
      na.omit()

    # Include no trend, trend or quadratic trend?
    switch(specs$trend,
           x_lin            <-   x_lin %>%
                                     as.matrix(),
           x_lin            <-   x_lin                                      %>%
                                      dplyr::mutate(trend    = row_number() %>%
                                                                as.matrix()),
           x_lin            <-   x_lin                           %>%
             dplyr::mutate(trend    = row_number())  %>%
             dplyr::mutate(sq_trend = trend^2) %>%
             as.matrix())

                                    } else {

    # ---  Make lag data based on max lag lengths
    y_lin     <- list()
    x_lin     <- list()

    for(i in 1:specs$max_lags){
      y_lin[[i]] <-  data_set_estim[(i + 1):dim(data_set_estim)[1],]   %>%
        as.matrix()




      x_lin[[i]] <-  dplyr::as_tibble(create_lags(data_set_estim, i)) %>%
        na.omit()

      # --- Include no trend, trend or quadratic trend
      switch(specs$trend,

             x_lin[[i]]            <-   x_lin[[i]]      %>%
               as.matrix(),
             x_lin[[i]]            <-   x_lin[[i]]                                 %>%
               dplyr::mutate(trend = row_number())   %>%
               as.matrix(),
             x_lin[[i]]            <-   x_lin[[i]]                                 %>%
               dplyr::mutate(trend = row_number())   %>%
               dplyr::mutate(sq_trend = trend^2) %>%
               as.matrix())


    }
  }

  # Return list with exogenous and endogenous data
  list(y_lin, x_lin)

}

