#' Function to create data for linear model.
#'
#' @param specs A list with specifications that go into 'lin_lp' or 'nl_lp' function.
#'        specs$lags_criterion: Either NaN (given lag length) or 'BIC'|'AIC'
#'        specs$lags_lin:       Lag length
#'        specs$trend:          1 (no trend), 2 (trend), 3 (quadratic trend)
#'        specs$max_lags:       Maximum number of lags to use for lag length criteria
#' @param data_set A data frame with all endogenous variables.
#' @return List with left (y_lin) and right hand side (x_lin) data.


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
           x_lin            <-   x_lin                                      %>%
                                    dplyr::mutate(trend    = row_number()),
           x_lin            <-   x_lin                                      %>%
                                    dplyr:: mutate(trend   = row_number())  %>%
                                    dplyr::mutate(sq_trend = trend^2))

################################################################################
                               } else {
################################################################################

    # Make lists to store data
    y_lin     <- list()
    x_lin     <- list()

    for(i in 1:specs$max_lags){
      y_lin[[i]] <-  data_set[(i + 1):dim(data_set)[1],]
      x_lin[[i]] <-  dplyr::as_tibble(create_lags(data_set, i)) %>%
        na.omit()

      # --- Include no trend, trend or quadratic trend
      switch(specs$trend,

         x_lin[[i]]            <-   x_lin[[i]],
         x_lin[[i]]            <-   x_lin[[i]]                               %>%
                                       dplyr::mutate(trend = row_number()),
         x_lin[[i]]            <-   x_lin[[i]]                               %>%
                                       dplyr::mutate(trend = row_number())   %>%
                                       dplyr::mutate(sq_trend = trend^2))


    }
  }

  # Return list with left- and right hand side data
  list(y_lin, x_lin)

}
