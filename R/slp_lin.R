#' @name slp_lin
#' @title Compute linear smooth local projections
#' @description Compute linear impulse responses with smooth local projection approach by Barnichon and Brownless (2018).
#' @param endog_data A \link{data.frame}, containing the values of the dependent variable. This function only works for one endogenous
#' variable at a time
#' @param shock A one column \link{data.frame}, including the variable to shock with. The row length has to be the same as \emph{endog_data}.
#' @param use_twosls  Boolean. Use two stage least squares? TRUE or FALSE.
#' @param lags_endog_lin Integer. Integer for number of lags for \emph{endog_data}.
#' @param exog_data A \link{data.frame}, containing exogenous variables. The row length has to be the same as \emph{endog_data}.
#' @param lags_exog NULL or Integer. Integer for the number of lags for the exogenous data.
#' @param contemp_data A \link{data.frame}, containing exogenous data with contemporaneous impact.
#'                      The row length has to be the same as \emph{endog_data}.
#' @param trend Integer. No trend =  0 , include trend = 1, include trend and quadratic trend = 2.
#' @param confint Double. Width of confidence bands. 68\% = 1; 90\% = 1.65; 95\% = 1.96.
#' @param hor Integer. Number of horizons for impulse responses.
#' @param h1 Integer. Start of local projections. Default is 1.
#' @param bdeg Integer. Start values of knots to evaluate the design matrix for the B-splines. The default is 3.
#' @param r Integer. Order of the limit polynomial.
#' @param shock_type Integer. Standard deviation shock = 0, unit shock = 1.
#' @param K Integer. Number for k-fold cross validation. The default value is 5.
#' @param use_cv Boolean. Use cross validation?
#' @param lambda Numeric. A numeric number or numeric vector (if use_cv = TRUE) for shrinkage value(s).
#' @param type Character. Type of estimation. Default is "smooth" for smooth local projections. "reg" estimates regular impulse responses.
#'
#' @importFrom dplyr if_else
#'
#' @return A list with all values.
#' @export
#'
#' @references
#'
#' Barnichon, R., Brwonless, C. (2018), "Impulse Response Estimation By Smooth Local Projections",
#' \emph{The Review of Economics and Statistics}, Forthcoming.
#'
#' @examples
#'
#' \donttest{
#'
#' # Load package
#' library(lpirfs)
#'
#' # Load data
#' data(us_macro)
#'
#' # Make vector of possible shrinkage parameters
#'   lambda         <- c(0.0001,0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0,9,10)/1000
#'
#'
#' # Estimate irf with smooth local projections
#' smooth_results<- slp_lin(endog_data      = tibble(yg = us_macro$yg),
#'                           shock          = tibble(ir = us_macro$ir),
#'                           use_twosls     = FALSE,
#'                           lags_endog_lin = 4,
#'                           exog_data      = tibble(pi = us_macro$pi,ir = us_macro$ir),
#'                           lags_exog      = 4,
#'                           contemp_data   = tibble(yg = us_macro$yg, pi = us_macro$pi),
#'                           trend          = 0,
#'                           confint        = 1.28,
#'                           hor            = 20,
#'                           bdeg           = 3,
#'                           r              = 2,
#'                           K              = 5,
#'                           use_cv         = TRUE,
#'                           lambda         = lambda)
#'
#'  # Make plots
#'  slp_lin_plots <- plot_lin(smooth_results)
#'
#'  # This figure replicates the upper right plot in Figure 3 of
#'  # Barnichon and Brownless (2018)
#'  plot(slp_lin_plots[[1]])
#'
#' }
#'
#'
slp_lin <- function(endog_data,
                    shock          = NULL,
                    use_twosls     = FALSE,
                    lags_endog_lin = NULL,
                    exog_data      = NULL,
                    lags_exog      = NULL,
                    contemp_data   = NULL,
                    trend          = NULL,
                    confint        = NULL,
                    hor            = NULL,
                    h1             = NULL,
                    bdeg           = NULL,
                    r              = NULL,
                    shock_type     = NULL,
                    K              = 5,
                    use_cv         = TRUE,
                    lambda         = 0,
                    type           = "smooth"){



  # Create list to store inputs
    specs <- list()

    shock                    <- as.matrix(shock)

  # Specify inputs
    specs$shock              <- shock
    specs$use_twosls         <- use_twosls
    specs$lags_endog_lin     <- lags_endog_lin
    specs$exog_data          <- exog_data
    specs$lags_exog          <- lags_exog
    specs$contemp_data       <- contemp_data
    specs$lags_criterion     <- NaN
  #  specs$max_lags           <- max_lags
    specs$trend              <- trend
    specs$confint            <- confint
    specs$hor                <- hor
    specs$model_type         <- 1
    specs$use_cv             <- use_cv

  # Safe data frame specifications in 'specs' for functions
    specs$starts         <- 1                       # Sample Start
    specs$ends           <- dim(endog_data)[1]      # Sample end
    specs$column_names   <- names(endog_data)       # Name endogenous variables
    specs$endog          <- ncol(endog_data)        # Set the number of endogenous variables

  # A constant will always be included
    const <- TRUE

  # Construct (lagged) endogenous data
    data_lin             <- create_lin_data(specs, endog_data)

    y     <- data_lin[[1]]
    w     <- data_lin[[2]][, -1] # Delete shock vector from exogenous data
    x     <- data_lin[[2]][, 1]  # Shock time series

    # Adding lags and trend
    w     <- cbind(rep(1, nrow(w)), w)

    # Use slp_estim() function to estimate impulse responses
    obj            <- slp_estim(y, w, x, hor, h1, r, K, type, lambda, specs)

    ## Estimate confidence bands
    # Find optimal 'position' of parameters
    l              <- which.min(obj$rss)
    conf_bands     <- slp_conf(obj , specs, l = l)

    # Add confidence bands and specs to obj
    obj$conf_bands <- conf_bands
    obj$specs      <- specs

    # Build matrices of impulse respones
    irf_lin_mean <- t(as.matrix(obj$ir.opt))
    irf_lin_low  <- t(conf_bands$irc[, 1])
    irf_lin_up   <- t(conf_bands$irc[, 2])

    # Replace NAs with 0 in upper and lower confidence bands matrices
    irf_lin_low  <- t(as.matrix(dplyr::if_else(is.na(irf_lin_low), 0, irf_lin_low)))
    irf_lin_up   <- t(as.matrix(dplyr::if_else(is.na(irf_lin_up), 0,  irf_lin_up)))

    # Return estimates and specs list
    return(list(irf_lin_mean = irf_lin_mean,
                irf_lin_low  = irf_lin_low,
                irf_lin_up   = irf_lin_up,
                obj          = obj,
                specs        = specs))


  }


