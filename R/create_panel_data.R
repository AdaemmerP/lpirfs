#' @name create_panel_data
#' @title Prepare data sets for linear panel model
#' @description Function to create panel data for linear panel model.
#' @param specs A list with specifications created in \link{lp_lin_panel}
#' @param data_set A data.frame consisting of a panel data set
#' @import dplyr
#' @return A list with prepared endogenous and exogenous data as well as the updated list \emph{specs}.
#' @keywords internal
#'
create_panel_data <- function(specs, data_set){


  # Function which takes first differences and sets the first value to NA
  # to be consistent with dplyr
  diff_function <- function(data){

      return(c(NA, diff(data)))

  }

  # Function which creates lags to be consistent with dplyr
  lag_function  <- function(data, lag_nr){

      lag_data  <- dplyr::lag(data, lag_nr)

  }

  # Lead-lag function to create cumulative endogenous variables
  cumul_function <- function(data, hor){

        val_lag  <- dplyr::lag(data, 1)
        val_lead <- dplyr::lead(data, hor)

        return(val_lead - val_lag)

  }


  # Make list to store endogenous variables
  y_data <- rep(list(NaN), specs$hor)

  # Create horizons of dependent variables based on whether to use cumulative multipliers
  if(specs$cumul_mult == TRUE){

    # Loop to create endogenous variables
    for(ii in 0:(specs$hor-1)){

      # Create cumulative endogenous vector
      y_data[[ii + 1]]     <-  data_set %>%
                                dplyr::select(cross_id, date_id, specs$endog_data) %>%
                                dplyr::mutate_at(vars(specs$endog_data),
                                       funs(cumul_function(., ii)))                %>%
                                dplyr::ungroup()
    }
                         ################
                         }    else     {
                         ################

    # Loop to create endogenous variables
    for(ii in 0:(specs$hor-1)){

      # Create lead endogenous vectors
      y_data[[ii + 1]]     <-  data_set %>%
                                dplyr::select(cross_id, date_id, specs$endog_data)  %>%
                                dplyr::mutate_at(vars(specs$endog_data ),
                                                 funs(dplyr::lead(., ii)))
    }

  }


  # Choose shock variable
  x_reg_data    <- data_set %>%
                   dplyr::select(cross_id, date_id,  specs$shock)


  # Take first differences of shock?
  if(specs$diff_shock == TRUE){

    x_reg_data    <- x_reg_data %>%
                      dplyr::mutate_at(vars(specs$shock), diff_function) %>%
                      dplyr::rename_at(vars(specs$shock), funs(paste0("d",.)))

    # Rename shock variable
    specs$shock   <- colnames(x_reg_data)[which(!(colnames(x_reg_data) %in% c("cross_id", "date_id")))]

  }


  # Choose instrument variable?
  if(isTRUE(specs$iv_reg)){
    x_instrument  <- data_set %>%
                     dplyr::select(cross_id, date_id,  specs$instrum)
  }


  # Take first differences of instrument?
  if(isTRUE(specs$iv_reg) & specs$diff_shock == TRUE){

    x_instrument   <- x_instrument %>%
                          dplyr::mutate_at(vars(specs$instrum), diff_function) %>%
                          dplyr::rename_at(vars(specs$instrum), funs(paste0("d",.)))

    # Rename instrument to use later in instrument regression
    specs$instrum  <- colnames(x_instrument)[which(!(colnames(x_instrument) %in% c("cross_id", "date_id")))]

  }



  # Choose exogenous data
  x_data        <- data_set %>%
                    dplyr::select(cross_id, date_id,  specs$exog_data)


  # Choose exogeonus data with contemporaneous impact
  if(!is.null(specs$c_exog_data)){

    c_x_data      <- x_data   %>%
                      dplyr::select(cross_id, date_id, specs$c_exog_data)

    # Use lagged contemporaneous data as regressors
    x_reg_data    <- suppressMessages(x_reg_data %>%
                      dplyr::left_join(c_x_data))

  }

  # Create lagged exogenous data
  if(!is.null(specs$l_exog_data)){


    # Make lag sequence
    lags_exog     <- seq(specs$lags_exog_data)
    lag_names     <- paste("lag", lags_exog,  sep = "_")

    # Create lag function to use with dplyr
    lag_functions <- stats::setNames(paste("dplyr::lag(., ", lags_exog, ")"), lag_names)

    l_x_data      <-  x_data %>%
                      dplyr::select(cross_id, date_id, specs$l_exog_data) %>%
                      dplyr::group_by(cross_id)                           %>%
                      dplyr::mutate_at(vars(specs$l_exog_data), funs_(lag_functions))              %>%
                      dplyr::ungroup()                                    %>%
                      dplyr::select(cross_id, date_id, contains("lag_"))


    # Use lagged exogenous data as regress
    x_reg_data    <- suppressMessages(x_reg_data %>%
                      dplyr::left_join(l_x_data))

  }



  # Calculate first differences of exogenous data?
  if(!is.null(specs$c_fd_exog_data) | !is.null(specs$l_fd_exog_data)){

    d_x_data       <- x_data                                                           %>%
                        dplyr::group_by(cross_id)                                      %>%
                        dplyr::mutate_at(vars(-cross_id, -date_id), diff_function)     %>%
                        dplyr::ungroup()                                               %>%
                        dplyr::rename_at(vars(-cross_id, -date_id), funs(paste0("d",.)))

  }


  # Create data with contemporanous impact of first differences
  if(!is.null(specs$c_fd_exog_data)){

    # Specify column names to choose
    specs$c_fd_exog_data <- paste("d", specs$c_fd_exog_data, sep = "")

    # Create data
    cd_x_data            <- d_x_data   %>%
                            dplyr::select(cross_id, date_id, specs$c_fd_exog_data)


    # Use first differences as regressors
    x_reg_data           <- suppressMessages(x_reg_data %>%
                            dplyr::left_join(cd_x_data))


  }


  # Create lagged exogenous data of first differences
  if(!is.null(specs$l_fd_exog_data)){

    # Specify column names to choose
    specs$l_fd_exog_data <- paste("d", specs$l_fd_exog_data, sep = "")


    # Make lag sequence
    lags_exog     <- seq(specs$lags_fd_exog_data)
    lag_names     <- paste("lag", lags_exog,  sep = "_")

    # Create lag function to use with dplyr
    lag_functions <- stats::setNames(paste("dplyr::lag(., ", lags_exog, ")"), lag_names)

    # Create data
    ld_x_data     <- d_x_data            %>%
                      group_by(cross_id) %>%
                      mutate_at(vars(specs$l_fd_exog_data), funs_(lag_functions)) %>%
                      dplyr::select(cross_id, date_id, contains("lag_"))

    # Use lags of first differences as regressors
    x_reg_data    <- suppressMessages(x_reg_data %>%
                      dplyr::left_join(ld_x_data))
  }

  # Save x_instrument if
  if(isTRUE(specs$iv_reg)){

    x_instrument <- x_instrument

            } else {

    x_instrument <- NULL

  }

  return(list(x_reg_data   = x_reg_data,
              y_data       = y_data,
              x_instrument = x_instrument,
              specs        = specs))

}
