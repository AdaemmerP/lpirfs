#' @name lp_nl_iv
#' @title Compute nonlinear impulse responses with identified shock
#' @description Compute nonlinear impulse responses with local projections and identified shock.
#' The data can be separated into two states by a smooth transition function as applied in Auerbach and Gorodnichenko (2012),
#' or by a simple dummy approach.
#' @param endog_data A \link{data.frame}, containing all endogenous variables for the VAR.
#' @param lags_endog_nl NaN or integer. NaN if lags are chosen by a lag length criterion. Integer for number of lags for \emph{endog_data}.
#' @param shock One column \link{data.frame}, including the instrument to shock with.
#'              The row length has to be the same as \emph{endog_data}.
#' @param instr Deprecated input name. Use \emph{shock} instead. See \emph{shock} for details.
#' @param exog_data A \link{data.frame}, containing exogenous variables. The row length has to be the same as \emph{endog_data}.
#'                  Lag lengths for exogenous variables have to be given and will no be determined via a lag length criterion.
#' @param lags_exog NULL or Integer. Integer for the number of lags for the exogenous data.
#' @param contemp_data A \link{data.frame}, containing exogenous data with contemporaneous impact. This data will not be lagged.
#'                      The row length has to be the same as \emph{endog_data}.
#' @param lags_criterion NaN or character. NaN means that the number of lags
#'         will be given at \emph{lags_endog_nl}. Possible lag length criteria are 'AICc', 'AIC' or 'BIC'.
#' @param max_lags NaN or integer. Maximum number of lags (if \emph{lags_criterion} = 'AICc', 'AIC', 'BIC'). NaN otherwise.
#' @param trend Integer. Include no trend =  0 , include trend = 1, include trend and quadratic trend = 2.
#' @param confint Double. Width of confidence bands. 68\% = 1; 90\% = 1.65; 95\% = 1.96.
#' @param hor Integer. Number of horizons for impulse responses.
#' @param switching Numeric vector. A column vector with the same length as \emph{endog_data}. This series can either
#'               be decomposed via the Hodrick-Prescott filter (see Auerbach and Gorodnichenko, 2013) or
#'               directly plugged into the following smooth transition function:
#'               \deqn{ F_{z_t} = \frac{exp(-\gamma z_t)}{1 + exp(-\gamma z_t)}. }
#'               Warning: \eqn{F_{z_t}} will be lagged by one and then multiplied with the data.
#'               If the variable shall not be lagged, the vector has to be given with a lead of one.
#'               The data for the two regimes are: \cr
#'               Regime 1 = (1-\eqn{F(z_{t-1})})*y_{(t-p)}, \cr
#'               Regime 2 = \eqn{F(z_{t-1})}*y_{(t-p)}.
#'@param lag_switching Boolean. Use the first lag of the values of the transition function? TRUE (default) or FALSE.
#'@param gamma Double. Positive number which is used in the transition function.
#'@param use_logistic Boolean. Use logistic function to separate states? TRUE (default) or FALSE. If FALSE, the values of the switching variable
#'                     have to be binary (0/1).
#'@param use_hp Boolean. Use HP-filter? TRUE or FALSE.
#'@param lambda Double. Value of \eqn{\lambda} for the Hodrick-Prescott filter (if use_hp = TRUE).
#'@param num_cores Integer. The number of cores to use for the estimation. If NULL, the function will
#'                 use the maximum number of cores minus one.
#'
#'@seealso \url{https://adaemmerp.github.io/lpirfs/README_docs.html}
#'
#'
#'
#'@return A list containing:
#'
#'\item{irf_s1_mean}{A \link{matrix}, containing the impulse responses of the first regime.
#'                    The row in each matrix denotes the responses of the \emph{ith}
#'                    variable to the shock. The columns are the horizons.}
#'
#'\item{irf_s1_low}{A \link{matrix}, containing all lower confidence bands of
#'                    the impulse responses, based on robust standard errors by Newey and West (1987).
#'                    Properties are equal to \emph{irf_s1_mean}.}
#'
#'\item{irf_s1_up}{A \link{matrix}, containing all upper confidence bands of the
#'                    impulse responses, based on robust standard errors by Newey and West (1987).
#'                    Properties are equal to \emph{irf_s1_mean}.}
#'
#'\item{irf_s2_mean}{A \link{matrix}, containing all impulse responses for the second regime.
#'                    The row in each matrix denotes the responses of the \emph{ith} variable to the shock.
#'                    The columns denote the horizon.}
#'
#'\item{irf_s2_low}{A \link{matrix}, containing all lower confidence bands of the responses,
#'                    based on robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s2_mean}.}
#'
#'\item{irf_s2_up}{A \link{matrix}, containing all upper confidence bands of the responses, based on
#'                    robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s2_mean}.}
#'
#'\item{specs}{A list with properties of \emph{endog_data} for the plot function. It also contains
#'             lagged data (y_nl and x_nl) used for the estimations of the impulse responses.}
#'
#'\item{fz}{A vector, containing the values of the transition function F(z_{t-1}).}
#'
#'@export
#'
#'@references
#'
#' Akaike, H. (1974). "A new look at the statistical model identification", \emph{IEEE Transactions on Automatic Control}, 19 (6): 716–723.
#'
#' Auerbach, A. J., and  Gorodnichenko Y. (2012). "Measuring the Output Responses to Fiscal Policy."
#' \emph{American Economic Journal: Economic Policy}, 4 (2): 1-27.
#'
#' Auerbach, A. J., and Gorodnichenko Y. (2013). "Fiscal Multipliers in Recession and Expansion."
#' \emph{NBER Working Paper Series}. Nr 17447.
#'
#' Blanchard, O., and Perotti, R. (2002). “An Empirical Characterization of the
#' Dynamic Effects of Changes in Government Spending and Taxes on Output.” \emph{Quarterly
#' Journal of Economics}, 117(4): 1329–1368.
#'
#' Hurvich, C. M., and Tsai, C.-L. (1989), "Regression and time series model selection in small samples",
#' \emph{Biometrika}, 76(2): 297–307
#'
#' Jordà, Ò. (2005) "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' Jordà, Ò, Schularick, M., Taylor, A.M. (2015), "Betting the house", \emph{Journal of International Economics},
#' 96, S2-S18.
#'
#' Newey, W.K., and West, K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
#' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.
#'
#' Ramey, V.A., and Zubairy, S. (2018). "Government Spending Multipliers in Good Times
#' and in Bad: Evidence from US Historical Data." \emph{Journal of Political Economy},
#' 126(2): 850 - 901.
#'
#' Schwarz, Gideon E. (1978). "Estimating the dimension of a model", \emph{Annals of Statistics}, 6 (2): 461–464.
#'
#' @import foreach
#' @examples
#'\donttest{
#'
#'# This example replicates results from the Supplementary Appendix
#'# by Ramey and Zubairy (2018) (RZ-18).
#'
#'# Load and prepare data
#'  ag_data           <- ag_data
#'  sample_start      <- 7
#'  sample_end        <- dim(ag_data)[1]
#'  endog_data        <- ag_data[sample_start:sample_end, 3:5]
#'
#'# The shock is estimated by RZ-18
#'  shock             <- ag_data[sample_start:sample_end, 7]
#'
#'# Include four lags of the 7-quarter moving average growth rate of GDP
#'# as exogenous variables (see RZ-18)
#'  exog_data         <- ag_data[sample_start:sample_end, 6]
#'
#'# Use the 7-quarter moving average growth rate of GDP as switching variable
#'# and adjust it to have suffiently long recession periods.
#'  switching_variable <- ag_data$GDP_MA[sample_start:sample_end] - 0.8
#'
#'# Estimate local projections
#'  results_nl_iv <- lp_nl_iv(endog_data,
#'                            lags_endog_nl     = 3,
#'                            shock             = shock,
#'                            exog_data         = exog_data,
#'                            lags_exog         = 4,
#'                            trend             = 0,
#'                            confint           = 1.96,
#'                            hor               = 20,
#'                            switching         = switching_variable,
#'                            use_hp            = FALSE,
#'                            gamma             = 3)
#'
#'# Make and save plots
#'  plots_nl_iv <- plot_nl(results_nl_iv)
#'
#'# Show single impulse responses
#'# Compare with red line of left plot (lower panel) in Figure 12 in Supplementary Appendix of RZ-18.
#'  plot(plots_nl_iv$gg_s1[[1]])
#'# Compare with blue line of left plot (lower panel) in Figure 12 in Supplementary Appendix of RZ-18.
#'  plot(plots_nl_iv$gg_s2[[1]])
#'
#'# Show all impulse responses by using 'ggpubr' and 'gridExtra'
#'# lpirfs does not depend on those packages so they have to be installed
#'  library(ggpubr)
#'  library(gridExtra)
#'
#'  s1_plots <- sapply(plots_nl_iv$gg_s1, ggplotGrob)
#'  s2_plots <- sapply(plots_nl_iv$gg_s2, ggplotGrob)
#'
#'# Show all responses of state 1
#'  marrangeGrob(s1_plots, nrow = ncol(endog_data), ncol = 1, top = NULL)
#'
#'# Show all responses of state 2
#'  marrangeGrob(s2_plots, nrow = ncol(endog_data), ncol = 1, top = NULL)
#'
#'}
#'@author Philipp Adämmer
#'
lp_nl_iv <- function(endog_data,
                            lags_endog_nl     = NULL,
                            shock             = NULL,
                            instr             = NULL,
                            exog_data         = NULL,
                            lags_exog         = NULL,
                            contemp_data      = NULL,
                            lags_criterion    = NaN,
                            max_lags          = NaN,
                            trend             = NULL,
                            confint           = NULL,
                            hor               = NULL,
                            switching         = NULL,
                            lag_switching     = TRUE,
                            use_logistic      = TRUE,
                            use_hp            = NULL,
                            lambda            = NULL,
                            gamma             = NULL,
                            num_cores         = NULL){

  # Give warning if 'instr' is used as input name
  if(!is.null(instr)){
    shock = instr
    warning("'instr' is a deprecated input name. Use 'shock' instead.")
  }

  # Check whether data is a data.frame
  if(!(is.data.frame(endog_data))){
    stop('The endogenous data has to be a data.frame.')
  }


  if(!is.null(exog_data) & !(is.data.frame(exog_data))){
    stop('The exogenous data has to be a data.frame.')
  }


  if(!is.null(contemp_data) & !(is.data.frame(contemp_data))){
    stop('The exogenous data with contemporary impact has to be a data.frame.')
  }


  # Check whether 'trend' is given
  if(is.null(trend) == TRUE){
    stop('Please specify whether and which type of trend to include.')
  }

  # Check whether trend is correctly specified
  if(!(trend %in% c(0,1,2))){
    stop('For trend please set 0 = no trend, 1 = trend, 2 = trend and quadratic trend.')
  }


   # Check whether switching variable is given
  if(is.null(switching) == TRUE){
    stop('Please provide a switching variable.')
  }

  # Check whether 'use_hp' is given
  if(is.null(use_hp) == TRUE){
    stop('Please specify whether to use the HP-filter for the switching variable.')
  }

  # Check whether lambda is given if 'use_hp == 1'
  if((use_hp == 1) &
     (is.null(lambda) == TRUE)){
    stop('Please specify lambda for the HP-filter.')
  }

  # Check whether 'gamma' is given
  if(isTRUE(use_logistic) & is.null(gamma) == TRUE){
    stop('Please specify gamma for the transition function.')
  }

  # Check whether gamma is positive
  if((gamma < 0)){
    stop('Gamma has to be a positive number.')
  }

  # Check whether 'confint' is given
  if(is.null(confint) == TRUE){
    stop('Please specify a value for the width of the confidence bands.')
  }

  # Check whether width of confidence bands is >=0
  if(!(confint >=0)){
    stop('The width of the confidence bands has to be >=0.')
  }

  # Check whether number of horizons is given
  if(is.null(hor) == TRUE){
    stop('Please specify the number of horizons.')
  }

  # Check whether wrong lag length criterion is given
  if(!(is.nan(lags_criterion)          | lags_criterion == 'AICc'|
       lags_criterion         == 'AIC' | lags_criterion == 'BIC') == TRUE){
    stop('Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.')
  }

  # Check whether lags criterion and fixed number of lags for nonlinear model is given
  if((is.character(lags_criterion) == TRUE) &
     (!is.na(lags_endog_nl) == TRUE)){
    stop('You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.')
  }

  # Check whether maximum number of lags is given for lag length criterion
  if((is.character(lags_criterion)) &
     (is.na(max_lags))){
    stop('Please provide a maximum number of lags for the lag length criterion.')
  }

  # Check whether values for horizons are correct
  if(!(hor > 0) | is.nan(hor) |  !(hor %% 1 == 0)){
    stop('The number of horizons has to be an integer and > 0.')
  }

  # Create list to store inputs
    specs <- list()

  # Specify inputs
   specs$lags_endog_nl               <- lags_endog_nl

  if(is.data.frame(shock)){
     specs$shock   <- shock  }  else {
     specs$shock   <- as.data.frame(shock)
  }

  if(is.null(exog_data) | is.data.frame(exog_data)){
     specs$exog_data    <- exog_data}  else  {
     specs$exog_data    <- as.data.frame(exog_data)
  }

  if(is.null(contemp_data) | is.data.frame(contemp_data)){
     specs$contemp_data <- contemp_data}  else {
     specs$contemp_data <- as.data.frame(contemp_data)
    }


    specs$lags_exog      <- lags_exog
    specs$lags_criterion <- lags_criterion
    specs$max_lags       <- max_lags
    specs$trend          <- trend
    specs$confint        <- confint
    specs$hor            <- hor
    specs$switching      <- switching
    specs$lag_switching  <- lag_switching
    specs$use_logistic   <- use_logistic
    specs$use_hp         <- use_hp
    specs$lambda         <- lambda
    specs$gamma          <- gamma

    specs$model_type     <- 1


  # Safe data frame specifications in 'specs for functions
    specs$starts         <- 1                       # Sample Start
    specs$ends           <- dim(endog_data)[1]      # Sample end
    specs$column_names   <- names(endog_data)       # Name endogenous variables
    specs$endog          <- ncol(endog_data)        # Set the number of endogenous variables

  # Construct data for nonlinear model
    data_nl <- create_nl_data(specs, endog_data)
    y_nl    <- data_nl[[1]]
    x_nl    <- data_nl[[2]]
    fz      <- data_nl[[3]]

  # Save endogenous and lagged exogenous data in specs
    specs$y_nl        <- y_nl
    specs$x_nl        <- x_nl


    # Matrices to store irfs for each horizon
    irf_temp_s1_mean  <-  matrix(NaN, specs$endog, specs$hor)
    irf_temp_s1_low   <-  irf_temp_s1_mean
    irf_temp_s1_up    <-  irf_temp_s1_mean

    irf_temp_s2_mean  <-  matrix(NaN, specs$endog, specs$hor)
    irf_temp_s2_low   <-  irf_temp_s2_mean
    irf_temp_s2_up    <-  irf_temp_s2_mean

  # Arrays to store irfs
    irf_s1_mean  <-  matrix(NaN, nrow = specs$endog, ncol = specs$hor)
    irf_s1_low   <-  irf_s1_mean
    irf_s1_up    <-  irf_s1_mean

    irf_s2_mean  <-  matrix(NaN, nrow = specs$endog, ncol = specs$hor)
    irf_s2_low   <-  irf_s2_mean
    irf_s2_up    <-  irf_s2_mean

  # Matrices to store OLS parameters for regime 1 & 2
    b1_s1        <- matrix(NaN, specs$endog, specs$endog)
    b1_low_s1    <- matrix(NaN, specs$endog, specs$endog)
    b1_up_s1     <- matrix(NaN, specs$endog, specs$endog)

    b1_s2        <- matrix(NaN, specs$endog, specs$endog)
    b1_low_s2    <- matrix(NaN, specs$endog, specs$endog)
    b1_up_s2     <- matrix(NaN, specs$endog, specs$endog)

  # Define coefficient position to extract regime_1 and regime_2 parameters in loop
    start_nl_s1   <- 2
    end_nl_s1     <- specs$endog + 1
    samp_nl_s1    <- start_nl_s1:end_nl_s1


  # Make cluster
  if(is.null(num_cores)){
  num_cores     <- min(specs$endog, parallel::detectCores() - 1)
  }

  cl            <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)

  # Determine whether manual lag lengths are given or have to be determined
  if(is.nan(specs$lags_criterion) == TRUE) {

    # Loops to estimate local projections
    nl_irfs <- foreach( s        = 1:specs$endog,
                        .packages = 'lpirfs') %dopar%{ # Accounts for the shocks

                          for (h in 1:specs$hor){   # Accounts for the horizons

                            yy  <-   y_nl[h:dim(y_nl)[1], ]
                            xx  <-   x_nl[1:(dim(x_nl)[1] - h + 1), ]

                            for (k in 1:specs$endog){ # Accounts for the reactions of the endogenous variables

                              # Estimate coefficients and newey west std.err
                              if(specs$endog == 1 ){
                                nw_results   <- lpirfs::newey_west(yy, xx, h)
                                                  } else {
                                nw_results   <- lpirfs::newey_west(yy[, k], xx, h)
                                                  }

                              b                <- nw_results[[1]]
                              std_err          <- sqrt(diag(nw_results[[2]]))*specs$confint

                              irf_temp_s1_mean[k, h] <- b[2]
                              irf_temp_s1_low[k,  h] <- b[2] - std_err[2]
                              irf_temp_s1_up[k,   h] <- b[2] + std_err[2]

                              irf_temp_s2_mean[k, h] <- b[3]
                              irf_temp_s2_low[k,  h] <- b[3] - std_err[3]
                              irf_temp_s2_up[k,   h] <- b[3] + std_err[3]
                            }
                          }

                          list(irf_temp_s1_mean, irf_temp_s1_low, irf_temp_s1_up,
                               irf_temp_s2_mean, irf_temp_s2_low, irf_temp_s2_up)

                        }

      # Fill irfs
      irf_s1_mean[ , ]  <- as.matrix(do.call(rbind, nl_irfs[[1]][1]))
      irf_s1_low[ , ]   <- as.matrix(do.call(rbind, nl_irfs[[1]][2]))
      irf_s1_up[ , ]    <- as.matrix(do.call(rbind, nl_irfs[[1]][3]))

      irf_s2_mean[ , ]  <- as.matrix(do.call(rbind, nl_irfs[[1]][4]))
      irf_s2_low[ , ]   <- as.matrix(do.call(rbind, nl_irfs[[1]][5]))
      irf_s2_up[ , ]    <- as.matrix(do.call(rbind, nl_irfs[[1]][6]))

################################################################################
                               } else {
################################################################################

    # Convert lag length criterion to number for Rcpp loop
    lag_crit     <- switch(specs$lags_criterion,
                           'AICc'= 1,
                           'AIC' = 2,
                           'BIC' = 3)

    # --- Loops to estimate local projections.
    nl_irfs <- foreach(s         = 1:specs$endog,
                       .packages = 'lpirfs') %dopar% { # Accounts for shocks

                         for (h in 1:specs$hor){      # Accounts for the horizons
                           for (k in 1:specs$endog){ # Accounts for the reactions of the endogenous variables

                             # Find optimal lag length
                             n_obs           <- nrow(y_nl[[1]]) - h + 1 # Number of observations for model with lag one
                             val_criterion   <- lpirfs::get_vals_lagcrit(y_nl, x_nl, lag_crit, h, k,
                                                                         specs$max_lags, n_obs)

                             lag_choice      <- which.min(val_criterion)

                             yy              <- y_nl[[lag_choice]][, k]
                             yy              <- yy[h: length(yy)]

                             xx              <- x_nl[[lag_choice]]
                             xx              <- xx[1:(dim(xx)[1] - h + 1),]

                             nw_results      <- lpirfs::newey_west(yy, xx, h)

                             b               <- nw_results[[1]]
                             std_err         <- sqrt(diag(nw_results[[2]]))*specs$confint

                             irf_temp_s1_mean[k, h] <- b[2]
                             irf_temp_s1_low[k,  h] <- b[2] - std_err[2]
                             irf_temp_s1_up[k,   h] <- b[2] + std_err[2]

                             irf_temp_s2_mean[k, h] <- b[3]
                             irf_temp_s2_low[k,  h] <- b[3] - std_err[3]
                             irf_temp_s2_up[k,   h] <- b[3] + std_err[3]

                           }
                          }

                         list(irf_temp_s1_mean, irf_temp_s1_low, irf_temp_s1_up,
                              irf_temp_s2_mean, irf_temp_s2_low, irf_temp_s2_up)
                       }

      # Fill irfs
      irf_s1_mean[ , ]  <- as.matrix(do.call(rbind, nl_irfs[[1]][1]))
      irf_s1_low[ , ]   <- as.matrix(do.call(rbind, nl_irfs[[1]][2]))
      irf_s1_up[ , ]    <- as.matrix(do.call(rbind, nl_irfs[[1]][3]))

      irf_s2_mean[ , ]  <- as.matrix(do.call(rbind, nl_irfs[[1]][4]))
      irf_s2_low[ , ]   <- as.matrix(do.call(rbind, nl_irfs[[1]][5]))
      irf_s2_up[ , ]    <- as.matrix(do.call(rbind, nl_irfs[[1]][6]))



  }

  # Close cluster
  parallel::stopCluster(cl)

  results_list <- list(irf_s1_mean = irf_s1_mean, irf_s1_low = irf_s1_low, irf_s1_up = irf_s1_up,
                       irf_s2_mean = irf_s2_mean, irf_s2_low = irf_s2_low, irf_s2_up = irf_s2_up,
                       fz          = fz,
                       specs       = specs)
  return(results_list)

}
