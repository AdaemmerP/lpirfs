#' @name create_lin_data
#' @title Compute data for linear model
#' @description Function to create data for linear model.
#' @param specs A \link{list}(). Inputs are created in \link{lp_lin_iv}.
#' @param endog_data A \link{data.frame} with dependent variables.
#' @return List with filled matrices of lagged left (y_lin) and right hand side (x_lin) variables.
#' @import dplyr
#' @keywords internal
#' @author Philipp Adämmer


create_lin_data     <- function(specs, endog_data){

  # Check whether lag length is provided or has to be determined
  if (is.nan(specs$lags_criterion)) {

    # Data for endogenous variables
    y_lin <- endog_data

    # Make exogenous lagged data and check, whether lag length is zero
    if(specs$lags_endog_lin == 0){

      x_lin <- data.frame(x = rep(Inf, nrow(endog_data)))

              } else {

      x_lin <- create_lags(endog_data, specs$lags_endog_lin)

      }

    # Check whether model type is 'iv'.
    # 0 = Normal model, 1 = IV model
    # Prepare instrument variable and add to exogenous data
    if(specs$model_type == 1){

      shock             <- specs$shock
      colnames(shock)   <- 'shock'
      x_lin             <- cbind(shock, x_lin)

    }

    # Include no trend, trend or quadratic trend
    if (specs$trend == 0){

      x_lin      <-   x_lin

            } else if (specs$trend == 1){

      x_lin      <-   x_lin                                           %>%
                      dplyr::mutate(trend = row_number())

              } else {

      x_lin      <-   x_lin                                   %>%
                      dplyr::mutate(trend = row_number())     %>%
                      dplyr::mutate(sq_trend = trend^2)
    }



    # Construct (lagged) exogenous data and merge it with lagged endogenous data
    if(!(is.null(specs$exog_data))){

      x_exog    <- create_lags(specs$exog_data, specs$lags_exog) %>%
                   `rownames<-`(NULL)

      x_lin     <- cbind(x_lin, x_exog)

    }

    # Add contemporaneous data if supplied
    if(!(is.null(specs$contemp_data))){

      x_contemp <-  specs$contemp_data

      x_lin     <- cbind(x_lin, x_contemp)

    }

    # Combine endogenous and exogenous data
      yx_all    <- cbind(y_lin, x_lin)   %>%
                   stats::na.omit()

      yx_all    <- yx_all[,  !(colSums(yx_all) == Inf)]


      y_lin     <- yx_all[, 1:ncol(endog_data)]  %>%
                   as.matrix()

      x_lin     <- yx_all[, (ncol(endog_data) + 1):dim(yx_all)[2]] %>%
                   as.matrix()



    # Check whether z_lin matrix has to be build for 2sls
      if(specs$use_twosls == TRUE){

    # Compare lag length between endog_lin and lags_exog
      z_lag         <- max(specs$lags_endog_lin, specs$lags_exog)
      z_lin         <- x_lin[, -1]
      z_lin         <- cbind(specs$instrum[(z_lag + 1):dim(specs$instrum)[1], ], z_lin)

    # Set instrument variable to NULL if use_twosls = FALSE
                } else {
      z_lin <- NULL
    }


################################################################################
                               } else {
################################################################################

    # Create list to store lagged data
    y_lin_store     <- rep(list(NaN), specs$max_lags)
    x_lin_store     <- rep(list(NaN), specs$max_lags)
    z_lin_store     <- rep(list(NaN), specs$max_lags)

    y_lin           <- endog_data



    # Make lag data based on max lag lengths
    for(i in 1:specs$max_lags){

      x_lin   <-  (create_lags(endog_data, i))


      if(specs$model_type == 1){
     # Prepare instrument variable
        shock            <- specs$shock
     # Add instrument to 'exogenous' data
        x_lin              <- cbind(shock, x_lin)
       }

     # Include no trend, trend or quadratic trend
        if (specs$trend == 0){

        x_lin          <-   x_lin

                } else if  (specs$trend == 1){

        x_lin          <-   x_lin                                %>%
                            dplyr::mutate(trend = row_number())

                 } else {

        x_lin          <-   x_lin                                %>%
                            dplyr::mutate(trend = row_number())  %>%
                            dplyr::mutate(sq_trend = trend^2)
        }


    # Add exogenous data and merge with endogenous data
      if(!(is.null(specs$exog_data))){

        x_exog    <- create_lags(specs$exog_data, specs$lags_exog) %>%
                     `rownames<-`(NULL)

        x_lin     <- cbind(x_lin, x_exog)

      }

    # Add contemporaneous data if supplied
      if(!(is.null(specs$contemp_data))){

        x_contemp <- specs$contemp_data
        x_lin     <- cbind(x_lin, x_contemp)

      }



    # Merge all and extract exogenous and endogenous data
        yx_all               <-  cbind(y_lin, x_lin)  %>%
                                 stats::na.omit()

        y_lin_store[[i]]     <-  yx_all[, 1:ncol(endog_data)] %>%
                                 as.matrix()

        x_lin_store[[i]]     <-  yx_all[, (ncol(endog_data) + 1):dim(yx_all)[2]] %>%
                                 as.matrix()

        # Check whether z_lin matrix has to be build for 2sls
        if(specs$use_twosls == TRUE){

          # Compare lag length between endog_lin and lags_exog
          z_lag                <- max(i, specs$lags_exog)
          z_lin                <- x_lin_store[[i]][, -1]
          z_lin                <- cbind(specs$instrum[(z_lag + 1):dim(specs$instrum)[1], ], z_lin)
          z_lin_store[[i]]     <- z_lin

                    }     else    {}



    }

        # Save values te return in list
        y_lin <- y_lin_store
        x_lin <- x_lin_store

        # Set instrument variable to NULL if use_twosls = FALSE
        if(specs$use_twosls == FALSE){

        z_lin <- NULL

           } else {

        z_lin <- z_lin_store

        }

}

# Return list with exogenous, endogenous data, and iv data
        return(list(y_lin, x_lin, z_lin))

}
