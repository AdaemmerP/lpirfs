#' @name create_panel_data
#' @title Prepare data sets for linear and nonlinear panel model
#' @description Function to create panel data for linear and nonlinear model.
#' @param specs A list with specifications created in \link{lp_lin_panel} or \link{lp_nl_panel}.
#' @param data_set A data.frame consisting of a panel data set.
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


  # Lead-lag function to create cumulative endogenous variables
  cumul_function <- function(data, hor){

        val_lag  <- dplyr::lag(data, 1)
        val_lead <- dplyr::lead(data, hor)

        return(val_lead - val_lag)

  }


  # Make list to store endogenous variables
  y_data <- rep(list(NaN), specs$hor)

  # Create horizons of dependent variables based on whether to use cumulative multipliers
  if(isTRUE(specs$cumul_mult)){

    # Loop to create endogenous variables
    for(ii in 0:(specs$hor-1)){

      # Create cumulative endogenous vector
      y_data[[ii + 1]]     <-  data_set %>%
                                dplyr::select(cross_id, date_id, specs$endog_data) %>%
                                dplyr::group_by(cross_id)                          %>%
                                dplyr::mutate_at(vars(specs$endog_data),
                                       list(~cumul_function(., ii)))                %>%
                                dplyr::ungroup()
    }

                         }    else     {


    # Loop to create endogenous variables
    for(ii in 0:(specs$hor-1)){

      # Create lead endogenous vectors
      y_data[[ii + 1]]     <-  data_set %>%
                                dplyr::select(cross_id, date_id, specs$endog_data)  %>%
                                dplyr::group_by(cross_id)                  %>%
                                dplyr::mutate_at(vars(specs$endog_data ),
                                                 list(~dplyr::lead(., ii))) %>%
                                dplyr::ungroup()
    }

  }


  # Choose shock variable and make a data frame called 'x_reg_data' which will
  # be filled with the other variables continously
  x_reg_data    <- data_set %>%
                   dplyr::select(cross_id, date_id,  specs$shock)


  # Take first differences of shock variable?
  if(isTRUE(specs$diff_shock)){

    x_reg_data    <- x_reg_data                                               %>%
                      dplyr::group_by(cross_id)                                %>%
                      dplyr::mutate_at(vars(specs$shock), diff_function)       %>%
                      dplyr::rename_at(vars(specs$shock), list(~paste0("d",.))) %>%
                      dplyr::ungroup()

    # Rename shock variable
    specs$shock   <- colnames(x_reg_data)[which(!(colnames(x_reg_data) %in% c("cross_id", "date_id")))]

  }


  # Choose instrument variable?
  if(isTRUE(specs$iv_reg)){
    x_instrument  <- data_set %>%
                     dplyr::select(cross_id, date_id,  specs$instrum)
  }


  # Take first differences of instrument?
  if(isTRUE(specs$iv_reg) & isTRUE(specs$diff_shock)){

    x_instrument   <- x_instrument %>%
                          dplyr::group_by(cross_id)                                  %>%
                          dplyr::mutate_at(vars(specs$instrum), diff_function)       %>%
                          dplyr::rename_at(vars(specs$instrum), list(~paste0("d",.))) %>%
                          dplyr::ungroup()

    # Rename instrument
    specs$instrum  <- colnames(x_instrument)[which(!(colnames(x_instrument) %in%
                                                     c("cross_id", "date_id")))]

  }


  # Choose exogenous data
  x_data          <- data_set %>%
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

    #--- Deprecated
    # # Make lag sequence
    # lags_exog     <- seq(specs$lags_exog_data)
    # lag_names     <- paste("lag", lags_exog,  sep = "_")
    #
    # # Create lag function to use with dplyr
    # lag_functions <- stats::setNames(paste("dplyr::lag(., ", lags_exog, ")"), lag_names)
    #
    # l_x_data      <-  x_data %>%
    #                   dplyr::select(cross_id, date_id, specs$l_exog_data) %>%
    #                   dplyr::group_by(cross_id)                           %>%
    #                   dplyr::mutate_at(vars(specs$l_exog_data), funs_(lag_functions))  %>%
    #                   dplyr::ungroup()                                    %>%
    #                   dplyr::select(-specs$l_exog_data) #cross_id, date_id, contains("lag_")) #
    # ---

    # Make lag sequence
    lags_exog      <- seq(specs$lags_exog_data)

    # Lag function
    lag_functions  <- lapply(lags_exog, function(x) function(col) dplyr::lag(col, x))

    # Make labels for lagged variables
    col_lag_labels <- paste(unlist(lapply(specs$l_exog_data, rep, max(lags_exog))), "_lag_", lags_exog, sep = "")

    # Make and get lagged data
    l_x_data       <- x_data %>%
                      dplyr::select(cross_id, date_id, specs$l_exog_data)     %>%
                      dplyr::group_by(cross_id)                               %>%
                      dplyr::mutate(across(specs$l_exog_data, lag_functions)) %>%
                      dplyr::ungroup()                                        %>%
                      dplyr::select(-specs$l_exog_data)

    # Rename columns
    colnames(l_x_data)[3:dim(l_x_data)[2]] <- col_lag_labels


    # Use lagged exogenous data as regressor
    x_reg_data        <- suppressMessages(x_reg_data %>%
                         dplyr::left_join(l_x_data))

  }



  # Calculate first differences of exogenous data?
  if(!is.null(specs$c_fd_exog_data) | !is.null(specs$l_fd_exog_data)){

    d_x_data       <- x_data                                                           %>%
                        dplyr::group_by(cross_id)                                      %>%
                        dplyr::mutate_at(vars(-cross_id, -date_id), diff_function)     %>%
                        dplyr::ungroup()                                               %>%
                        dplyr::rename_at(vars(-cross_id, -date_id), list(~paste0("d",.)))

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

    #--- Deprecated
    # Specify column names to choose
    # specs$l_fd_exog_data <- paste("d", specs$l_fd_exog_data, sep = "")

    # # Make lag sequence
    # lags_exog     <- seq(specs$lags_fd_exog_data)
    # lag_names     <- paste("lag", lags_exog,  sep = "_")
    #
    # # Create lag function to use with dplyr
    # lag_functions <- stats::setNames(paste("dplyr::lag(., ", lags_exog, ")"), lag_names)
    #
    # # Create data
    # ld_x_data     <- d_x_data                                                            %>%
    #                   dplyr::select(cross_id, date_id, specs$l_fd_exog_data )            %>%
    #                   dplyr::group_by(cross_id)                                          %>%
    #                   dplyr::mutate_at(vars(specs$l_fd_exog_data), funs_(lag_functions)) %>%
    #                   dplyr::ungroup()                                                   %>%
    #                   dplyr::select(-specs$l_fd_exog_data)# cross_id, date_id, contains("lag_")) #
    # ---

    # Specify column names to choose
    specs$l_fd_exog_data <- paste("d", specs$l_fd_exog_data, sep = "")

    # Make lag sequence
    dlags_exog      <- seq(specs$lags_fd_exog_data)

    # Lag function
    lag_functions  <- lapply(dlags_exog, function(x) function(col) dplyr::lag(col, x))

    # Make labels for lagged variables
    col_dlag_labels <- paste(unlist(lapply(specs$l_fd_exog_data, rep, max(dlags_exog))), "_lag_", dlags_exog, sep = "")

    # Make and get lagged data
    ld_x_data       <- d_x_data %>%
                      dplyr::select(cross_id, date_id, specs$l_fd_exog_data) %>%
                      dplyr::group_by(cross_id)                           %>%
                      dplyr::mutate(across(specs$l_fd_exog_data, lag_functions)) %>% # , .names = "{col_lag_labels}")
                      dplyr::ungroup()                                    %>%
                      dplyr::select(-specs$l_fd_exog_data)


    # Rename columns
    colnames(ld_x_data)[3:dim(ld_x_data)[2]] <- col_dlag_labels

    # Use lags of first differences as regressors
    x_reg_data    <- suppressMessages(x_reg_data %>%
                      dplyr::left_join(ld_x_data))
  }

################################################################################
#################    Separate data in two states if model is nonlinear  ########
################################################################################

    if(specs$model_type == 2 & isTRUE(specs$is_nl)){



    # Get transition probabilities by logistic function?
      if(isTRUE(specs$use_logistic)){

        fz      <- get_vals_switching(data_set, specs)

                 } else {

        # fz    <- as.matrix(data_set[, specs$switching])
        fz      <-  as.numeric(unlist(data_set[, specs$switching]))

        # Use first lag of value from switching function?
        if(isTRUE(specs$lag_switching)){

          fz_df <- tibble(cross_id = data_set$cross_id, date_id = data_set$date_id,
                          fz = fz)

          fz_df <- fz_df %>%
                   dplyr::group_by(cross_id)                              %>%
                   dplyr::mutate_at(vars(fz), list(~dplyr::lag(., 1)))   %>%
                   dplyr::ungroup()

          fz    <- fz_df$fz


        }

      }

    # Separate shock into two regimes
    # Choose shock variable
    shock           <- data_set %>%
                       dplyr::select(cross_id, date_id, if_else(specs$diff_shock,
                                                                substring(specs$shock, 2),
                                                                specs$shock))        %>%
                       dplyr::rename(shock = if_else(specs$diff_shock,
                                                     substring(specs$shock, 2),
                                                     specs$shock))

    # Take first difference of shock?
    if(isTRUE(specs$diff_shock)){

    shock          <- shock %>%
                       dplyr::group_by(cross_id) %>%
                       dplyr::mutate_at(vars(shock), diff_function) %>%
                       dplyr::ungroup()
    }


    # Make states of shock variable
    shock_s1        <- shock %>%
                        dplyr::mutate_at(vars(-cross_id, -date_id), list(shock_s1 = ~.*(1 - fz)))  %>%
                        dplyr::select(-shock)

    shock_s2        <- shock %>%
                        dplyr::mutate_at(vars(-cross_id, -date_id), list(shock_s2 = ~.*fz))        %>%
                        dplyr::select(-shock)

    # Exclude shock variable from 'x_reg_data'
    x_reg_data      <- x_reg_data %>%
                        dplyr::select(-c(specs$shock))

    # Separate exogenous data
    x_linear_names  <- colnames(x_reg_data)[!colnames(x_reg_data) %in% c("cross_id", "date_id")]

    x_nl_s1         <- x_reg_data %>%
                        dplyr::mutate_at(vars(-cross_id, -date_id), list(s1 = ~.*(1 - fz))) %>%
                        dplyr::select(-one_of(x_linear_names))

    x_nl_s2         <- x_reg_data %>%
                        dplyr::mutate_at(vars(-cross_id, -date_id), list(s2 = ~.*fz))  %>%
                        dplyr::select(-one_of(x_linear_names))

    x_reg_data      <-  suppressMessages(dplyr::left_join(shock_s1, shock_s2) %>%
                        dplyr::left_join(x_nl_s1)            %>%
                        dplyr::left_join(x_nl_s2))
  }

################################################################################
################################################################################

  # Save x_instrument if specified
  if(isTRUE(specs$iv_reg)){

    x_instrument <- x_instrument

            } else {

    x_instrument <- NULL

  }

  # Return different lists depending on whether the model is linear or nonlinear
  if(!is.null(specs$switching)) {

  return(list(x_reg_data     = x_reg_data,
                y_data       = y_data,
                x_instrument = x_instrument,
                fz           = fz,
                specs        = specs))

                 } else {

  return(list(x_reg_data     = x_reg_data,
              y_data         = y_data,
              x_instrument   = x_instrument,
              specs          = specs))
 }
}
