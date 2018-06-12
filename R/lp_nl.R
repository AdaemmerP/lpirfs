#' @name lp_nl
#' @title Compute (nonlinear) impulse responses
#' @description Compute nonlinear impulse responses with local projections by Jordà (2005). The
#' data are separated into two states with a smooth transition function as proposed by Auerbach and Gorodnichenko (2012).
#'
#' @param data_set_df A \link{data.frame}() containing all endogenous variables for the VAR. The column order
#'                     is used for the Cholesky decomposition.
#' @param lags_criterion NaN or character. NaN means that the number of lags
#'         will be given at \emph{lags_nl} and \emph{lags_lin}. The lag length criteria are 'AICc', 'AIC' and 'BIC'.
#' @param lags_lin Integer. Number of lags for linear VAR to identify shock.
#' @param lags_nl Integer. Number of lags for (nonlinear) VAR (if \emph{lags_criterion} = NaN).
#' @param max_lags Integer. Maximum number of lags (if \emph{lags_criterion} = 'AICc', 'AIC', 'BIC').
#' @param trend Integer. Include no trend =  0 , include trend = 1, include trend and quadratic trend = 2.
#' @param shock_type Integer. Standard deviation shock = 0, Unit shock = 1.
#' @param confint Double. Width of confidence bands. 68\% = 1; 90\% = 1.65; 95\% = 1.96.
#' @param hor Integer. Number of horizons for impulse responses.
#' @param switching Vector. A column vector with the same length as \emph{data_set_df}. This series can either
#'               be decomposed by the Hodrick-Prescott filter (see Auerbach and Gorodnichenko, 2013) or
#'               directly plugged into the smooth transition function:
#'               \deqn{ F_{z_t}) = \frac{exp(-\gamma z_t)}{1 + exp(-\gamma z_t)} }
#'               Warning: \eqn{F_{z_t}} will be lagged in \link{create_nl_data} by one and then multiplied with the data.
#'               If the variable shall not be lagged, the vector has to be given with a lead of one.
#'               The data for the two regimes are: \cr
#'               Regime 1 = (1-\eqn{F(z_{t-1})})*y_{t-p}, \cr
#'               Regime 2 = \eqn{F(z_{t-1})}*y_{t-p}.
#'@param gamma Double. Value of \eqn{\gamma} which is used in the transition function.
#'@param hp_filter Integer. No HP-filter = 0. Use HP-filter = 1.
#'@param lambda Double. Value of \eqn{\lambda} for the Hodrick-Prescott filter if \emph{hp_filter} = 1.
#'
#'
#'
#' @return A list with impulse responses and their robust confidence bands.
#' It also contains an updated list of \emph{specs} with further properties of \emph{data_set_df} for the plot function.
#'
#'\item{irf_s1_mean}{A three 3D \link{array}(), containing all impulse responses for all endogenous variables of the first state.
#'                    The last dimension denotes the shock variable. The row in each matrix
#'                    denotes the respones of the \emph{ith} variable ordered as in \emph{data_set_df}. The columns are the horizons.
#'                    For example, if the results are saved in \emph{results_nl}, results_nl$irf_s1_mean[, , 1] returns a KXH matrix,
#'                    where K is the number of variables and H the number of horizons. '1' is the variable which shocks, i.e. the
#'                    variable in the first column of \emph{data_set_df}.}
#'
#'\item{irf_s1_low}{A three 3D \link{array}(), containing all lower confidence bands of the impulse responses, based on
#'                    robust standard standard errors by Newey and West (1987). Properties are equal to \emph{irf_s1_mean}.}
#'
#'\item{irf_s1_up}{A three 3D \link{array}(), containing all upper confidence bands of the impulse responses, based on
#'                    robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s1_mean}.}
#'
#'\item{irf_s2_mean}{A three 3D \link{array}(), containing all impulse responses for all endogenous variables of the second state.
#'                    The last dimension denotes the shock variable. The row in each matrix
#'                    denotes the respones of the \emph{ith} variable as ordered in data_set_df. The columns denote the horizon.
#'                    For example, if the results are saved in \emph{results_nl}, results_nl$irf_s2_mean[, , 1] returns a KXH matrix,
#'                    where K is the number of variables and H the number of horizons. '1' is the first shock variable corresponding to the
#'                    variable in the first column of \emph{data_set_df}.}
#'
#'\item{irf_s2_low}{A three 3D \link{array}(), containing all lower confidence bands of the responses,
#'                    based on robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s2_mean}.}
#'
#'\item{irf_s2_up}{A three 3D \link{array}(), containing all upper confidence bands of the responses, based on
#'                    robust standard errors by Newey and West (1987). Properties are equal to \emph{irf_s2_mean}.}
#'
#'\item{specs}{An updated list of \emph{specs} for the plot function.}
#'
#'\item{fz}{A vector containing the values of the transition function F(z_{t-1})}
#'
#' @export
#' @references
#' Auerbach, A. J., and  Gorodnichenko Y. (2012). "Measuring the Output Responses to Fiscal Policy."
#' \emph{American Economic Journal: Economic Policy}, 4 (2): 1-27.
#'
#' Auerbach, A. J., and Gorodnichenko Y. (2013). "Fiscal Multipliers in Recession and Expansion."
#' \emph{NBER Working Paper Series}. Nr 17447.
#'
#' Jordà, Ò. (2005) "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' Newey W.K., and West K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
#' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.
#'
#' @import foreach
#' @examples
#' \dontrun{
#'# Load package
#'   library(lpirfs)
#'
#'# Load data (from package)
#'   data_set_df <- monetary_var_data
#'
#'# Estimate model and save results
#'   results_nl <- lp_nl(data_set_df, lags_lin       = 4L,
#'                                    lags_nl        = 3L,
#'                                    lags_criterion = NaN,
#'                                    max_lags       = NaN,
#'                                    trend          = 0L,
#'                                    shock_type     = 1L,
#'                                    confint        = 1.96,
#'                                    hor            = 24L,
#'                                    switching      = data_set_dff$FF,
#'                                    hp_filter      = 1L,
#'                                    lambda         = 1600,
#'                                    gamma          = 3)
#'
#'# Make and save all plots
#'   nl_plots <- plot_nl_irfs(results_nl)
#'
#'# Show all plots
#'   library(ggpubr)
#'   library(gridExtra)
#'
#'# Save plots based on states
#'   s1_plots <- sapply(nl_plots$gg_s1, ggplotGrob)
#'   s2_plots <- sapply(nl_plots$gg_s2, ggplotGrob)
#'
#'# Show first irf of each state
#'   plot(s1_plots[[1]])
#'   plot(s2_plots[[1]])
#'
#'# Show all plots
#'   marrangeGrob(s1_plots, nrow = ncol(data_set_df), ncol = ncol(data_set_df), top=NULL)
#'   marrangeGrob(s2_plots, nrow = ncol(data_set_df), ncol = ncol(data_set_df), top=NULL)
#'
#'}
#' @author Philipp Adämmer
#'
lp_nl <- function(data_set_df, lags_lin  = 4, lags_nl = 3,  lags_criterion = NaN, max_lags = NaN,
                               trend     = 0L, shock_type    = 1L,  confint  = 1.96,
                               hor       = 24, switching     = data_set_df[, 1],
                               hp_filter = 1, lambda         = 1600, gamma   = 3){

  # Create list to store inputs
    specs <- list()

  # Specify inputs
    specs$lags_lin       <- lags_lin
    specs$lags_nl        <- lags_nl
    specs$lags_criterion <- lags_criterion
    specs$max_lags       <- max_lags
    specs$trend          <- trend
    specs$shock_type     <- shock_type
    specs$confint        <- confint
    specs$hor            <- hor

    specs$switching      <- switching
    specs$hp_filter      <- hp_filter
    specs$lambda         <- lambda
    specs$gamma          <- gamma

#--- Check inputs

  # Check whether data is a data.frame
  if(!(is.data.frame(data_set_df))){
    stop('The data has to be a data.frame().')
  }

  # Check whether 'trend' is given
  if(is.null(specs$trend) == TRUE){
    stop('Please specify whether and which type of trend to include.')
  }

  # Check whether 'shock_type' is given
  if(is.null(specs$shock_type) == TRUE){
    stop('Please specify which type of shock to use.')
  }

  # Check whether switching variable is given
  if(is.null(specs$switching) == TRUE){
    stop('Please specify a switching variable.')
  }

  # Check whether 'hp_filter' is given
  if(is.null(specs$hp_filter) == TRUE){
    stop('Please specify whether to use the HP-filter for the switching variable.')
  }

  # Check whether lambda is given if 'hp_filter == 1'
  if((specs$hp_filter == 1) &
     (is.null(specs$lambda) == TRUE)){
    stop('Please specify lambda for the HP-filter.')
  }

  # Check whether 'gamma' is given
  if(is.null(specs$gamma) == TRUE){
    stop('Please specify gamma for the transition function.')
  }

  # Check whether 'confint' is given
  if(is.null(specs$confint) == TRUE){
    stop('Please specify a value for the width of the confidence bands.')
  }


  # Check whether number of horizons is given
  if(is.null(specs$hor) == TRUE){
    stop('Please specify the number of horizons.')
  }

  # Check whether wrong lag length criterion is given
  if(!(is.nan(specs$lags_criterion)          | specs$lags_criterion == 'AICc'|
       specs$lags_criterion         == 'AIC' | specs$lags_criterion == 'BIC') == TRUE){
    stop('Possible lag length criteria are AICc, AIC or BIC or NaN if lag length is specified.')
  }


  # Check whether lags criterion and fixed number of lags for nonlinear model is given
  if((is.character(specs$lags_criterion) == TRUE) &
     (!is.na(specs$lags_nl) == TRUE)){
    stop('You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.')
  }


  # Check whether lags criterion and fixed number of lags for linear model is given
  if((is.character(specs$lags_criterion) == TRUE) &
     (!is.na(specs$lags_lin) == TRUE)){
    stop('You can not provide a lag criterion (AICc, AIC or BIC) and a fixed number of lags.')
  }


  # Check whether maximum number of lags is given for lag length criterion
  if((is.character(specs$lags_criterion)  == TRUE) &
     (is.na(specs$max_lags)               == TRUE)){
    stop('Please provide a maximum number of lags for the lag length criterion.')
  }


  # Check whether lin_lags is given if nl_lags is given
  if((is.numeric(specs$lags_nl) == TRUE) &
     (is.null(specs$lags_lin) == TRUE)){
    stop('Please provide a lag length for the linear model to identify the shock.')
  }


  # Check whether values for horizons are correct
  if(!(specs$hor > 0) | is.nan(specs$hor) |  !(specs$hor %% 1 == 0)){
    stop('The number of horizons has to be an integer and > 0.')
  }

  # Check whether lags for linear model are integers
  if(is.numeric(specs$lags_nl) & !is.nan(specs$lags_nl)){
    if(!(specs$lags_nl %% 1 == 0) | specs$lags_nl < 0){
      stop('The numbers of lags have to be a positive integer.')
    }
  } else {}


  # Check whether lags for linear model are integers
  if(is.numeric(specs$lags_lin) & !is.nan(specs$lags_lin)){
    if(!(specs$lags_lin %% 1 == 0)  | specs$lags_lin < 0){
      stop('The numbers of lags have to be a positive integer.')
    }
  } else {}



  # Check whether trend is correctly specified
  if(!(specs$trend %in% c(0,1,2))){
    stop('For trend please put 0 = no trend, 1 = trend, 2 = trend and quadratic trend.')
  }


  # Check whether shock type is correctly specified
  if(!(specs$shock_type %in% c(0,1))){
    stop('The shock_type has to be 0 = standard deviation shock or 1 = unit shock.')
  }


  # Check whether width of confidence bands is >=0
  if(!(specs$confint >=0)){
    stop('The width of the confidence bands has to be >=0.')
  }

  # Check whether gamma is positive
  if((specs$gamma < 0)){
    stop('Gamma has to be a positive number.')
  }

  # Check whether hp_filter is either 0 or 1 is positive
  if(!(specs$hp_filter %in% c(0, 1))){
    stop('Please set hp_filter = 0 (do not use HP-filter), or hp_filter = 1 (use HP-filter).')
  }



  # Safe data frame specifications in 'specs for functions
  specs$starts         <- 1                        # Sample Start
  specs$ends           <- dim(data_set_df)[1]      # Sample end
  specs$columns        <- names(data_set_df)       # Name endogenous variables
  specs$endog          <- ncol(data_set_df)        # Set the number of endogenous variables

  # Construct data for nonlinear model
  data_nl <- create_nl_data(specs, data_set_df)
  y_nl    <- data_nl[[1]]
  x_nl    <- data_nl[[2]]
  fz      <- data_nl[[3]]

  # Construct data for linear model for reduced shocks
  data_lin   <- create_lin_data(specs, data_set_df)
  y_lin      <- data_lin[[1]]
  x_lin      <- data_lin[[2]]

  # Construct shock matrix
  d <- reduced_var(y_lin, x_lin, data_set_df, specs)

  # Matrices to store irfs for each horizon
  irf_temp_s1_mean  <-  matrix(NaN, specs$endog, specs$hor + 1)
  irf_temp_s1_low   <-  irf_temp_s1_mean
  irf_temp_s1_up    <-  irf_temp_s1_mean

  irf_temp_s2_mean  <-  matrix(NaN, specs$endog, specs$hor + 1)
  irf_temp_s2_low   <-  irf_temp_s2_mean
  irf_temp_s2_up    <-  irf_temp_s2_mean

  # Arrays to store irfs
  irf_s1_mean  <-  array(NaN, dim = c(specs$endog, specs$hor + 1, specs$endog))
  irf_s1_low   <-  irf_s1_mean
  irf_s1_up    <-  irf_s1_mean

  irf_s2_mean  <-  array(NaN, dim = c(specs$endog, specs$hor + 1, specs$endog))
  irf_s2_low   <-  irf_s2_mean
  irf_s2_up    <-  irf_s2_mean

  # Matrices to store OLS parameters for regime 1 & 2
  b1_s1      <- matrix(NaN, specs$endog, specs$endog)
  b1_low_s1  <- matrix(NaN, specs$endog, specs$endog)
  b1_up_s1   <- matrix(NaN, specs$endog, specs$endog)

  b1_s2      <- matrix(NaN, specs$endog, specs$endog)
  b1_low_s2  <- matrix(NaN, specs$endog, specs$endog)
  b1_up_s2   <- matrix(NaN, specs$endog, specs$endog)

  # Define coefficient position to extract regime_1 and regime_2 parameters in loop
  start_nl_s1   <- 2
  end_nl_s1     <- specs$endog + 1
  samp_nl_s1    <- start_nl_s1:end_nl_s1


  # Make cluster
  numb_cores     <- min(specs$endog, parallel::detectCores() - 1)
  cl             <- parallel::makeCluster(numb_cores)
  doParallel::registerDoParallel(cl)

 # Determine whether manual lag lengths are given or have to be determined
  if(is.nan(specs$lags_criterion) == TRUE) {

# Determine parameter position for regime 2
 start_nl_s2   <- 2 + specs$endog*specs$lags_nl
 end_nl_s2     <- start_nl_s2 + specs$endog - 1
 samp_nl_s2    <- start_nl_s2:end_nl_s2

 # Loops to estimate local projections
  nl_irfs <- foreach( s        = 1:specs$endog,
                     .packages = 'lpirfs') %dopar%{ # Accounts for the shocks

        for (h in 1:specs$hor){   # Accounts for the horizons

         yy  <-   y_nl[h:dim(y_nl)[1], ]
         xx  <-   x_nl[1:(dim(x_nl)[1] - h + 1), ]

         for (k in 1:specs$endog){ # Accounts for the reactions of the endogenous variables

           # Estimate coefficients and newey west std.err
           nw_results       <- lpirfs::newey_west_c(yy[, k], xx, h)
           b                <- nw_results[[1]]
           std_err          <- sqrt(diag(nw_results[[2]]))*specs$confint

           # Extract coefficients
           b1_s1[k, ]         <-   b[samp_nl_s1]
           b1_low_s1[k, ]     <-   b[samp_nl_s1] - std_err[samp_nl_s1]
           b1_up_s1[k, ]      <-   b[samp_nl_s1] + std_err[samp_nl_s1]

           b1_s2[k, ]         <-   b[samp_nl_s2]
           b1_low_s2[k, ]     <-   b[samp_nl_s2] - std_err[samp_nl_s2]
           b1_up_s2[k, ]      <-   b[samp_nl_s2] + std_err[samp_nl_s2]
          }

          # Estimate local projections
           irf_temp_s1_mean[, h + 1] <- t(b1_s1        %*%  d[ , s])
           irf_temp_s1_low[,  h + 1] <- t(b1_low_s1    %*%  d[ , s])
           irf_temp_s1_up[,   h + 1] <- t(b1_up_s1     %*%  d[ , s])

           irf_temp_s2_mean[, h + 1] <- t(b1_s2        %*%  d[ , s])
           irf_temp_s2_low[,  h + 1] <- t(b1_low_s2    %*%  d[ , s])
           irf_temp_s2_up[,   h + 1] <- t(b1_up_s2     %*%  d[ , s])
   }

    list(irf_temp_s1_mean, irf_temp_s1_low, irf_temp_s1_up,
         irf_temp_s2_mean, irf_temp_s2_low, irf_temp_s2_up)

}

 # Fill arrays with irfs
 for(i in 1:specs$endog){

   # Fill irfs
   irf_s1_mean[, , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][1]))
   irf_s1_low[, ,  i] <- as.matrix(do.call(rbind, nl_irfs[[i]][2]))
   irf_s1_up[, ,   i] <- as.matrix(do.call(rbind, nl_irfs[[i]][3]))

   irf_s2_mean[, , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][4]))
   irf_s2_low[,  , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][5]))
   irf_s2_up[,   , i] <- as.matrix(do.call(rbind, nl_irfs[[i]][6]))

   # First value of is merely the shock
   irf_s1_mean[, 1, i]   <- t(d[, i])
   irf_s1_low[,  1, i]   <- irf_s1_mean[, 1, i]
   irf_s1_up[,   1, i]   <- irf_s1_mean[, 1, i]

   irf_s2_mean[, 1, i]   <- t(d[, i])
   irf_s2_low[,  1, i]   <- irf_s1_mean[, 1, i]
   irf_s2_up[,   1, i]   <- irf_s1_mean[, 1, i]
}

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

             # Find optimal lag length and select matrices from lists accordingly
              val_criterion   <- lpirfs::find_lag_c(y_lin, x_lin, lag_crit, h, k,
                                                        specs$max_lags)
              lag_choice      <- which.min(val_criterion)

              yy              <- y_nl[[lag_choice]][, k]
              yy              <- yy[h: length(yy)]

              xx              <- x_nl[[lag_choice]]
              xx              <- xx[1:(dim(xx)[1] - h + 1),]

             # Estimate parameters and newey west standard errors
              nw_results      <- lpirfs::newey_west_c(yy, xx, h)
              b               <- nw_results[[1]]
              std_err         <- sqrt(diag(nw_results[[2]]))

             # Set start and values of parameters for regime 2
              start_nl_s2     <- 2 + specs$endog*lag_choice
              end_nl_s2       <- start_nl_s2 + specs$endog - 1
              samp_nl_s2      <- start_nl_s2:end_nl_s2

             # Fill paramater matrices
              b1_s1[k, ]      <-   b[samp_nl_s1]
              b1_low_s1[k, ]  <-   b[samp_nl_s1] - std_err[samp_nl_s1]
              b1_up_s1[k, ]   <-   b[samp_nl_s1] + std_err[samp_nl_s1]

              b1_s2[k, ]      <-   b[samp_nl_s2]
              b1_low_s2[k, ]  <-   b[samp_nl_s2] - std_err[samp_nl_s2]
              b1_up_s2[k, ]   <-   b[samp_nl_s2] + std_err[samp_nl_s2]
              }

             # Estimate local projections
              irf_temp_s1_mean[, h + 1] <- t(b1_s1        %*%  d[ , s])
              irf_temp_s1_low[,  h + 1] <- t(b1_low_s1    %*%  d[ , s])
              irf_temp_s1_up[,   h + 1] <- t(b1_up_s1     %*%  d[ , s])

              irf_temp_s2_mean[, h + 1] <- t(b1_s2        %*%  d[ , s])
              irf_temp_s2_low[,  h + 1] <- t(b1_low_s2    %*%  d[ , s])
              irf_temp_s2_up[,   h + 1] <- t(b1_up_s2     %*%  d[ , s])
             }

          list(irf_temp_s1_mean, irf_temp_s1_low, irf_temp_s1_up,
               irf_temp_s2_mean, irf_temp_s2_low, irf_temp_s2_up)
        }

# Fill arrays with local projection irfs
  for(i in 1:specs$endog){

  # Fill irfs
   irf_s1_mean[, , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][1]))
   irf_s1_low[,  , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][2]))
   irf_s1_up[,  ,  i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][3]))

   irf_s2_mean[, , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][4]))
   irf_s2_low[,  , i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][5]))
   irf_s2_up[, ,   i]  <- as.matrix(do.call(rbind, nl_irfs[[i]][6]))

  # First value of horizon is merely the shock
   irf_s1_mean[, 1, i]   <- t(d[, i])
   irf_s1_low[,  1, i]   <- irf_s1_mean[, 1, i]
   irf_s1_up[,   1, i]   <- irf_s1_mean[, 1, i]

   irf_s2_mean[, 1, i]   <- t(d[, i])
   irf_s2_low[,  1, i]   <- irf_s2_mean[, 1, i]
   irf_s2_up[,   1, i]   <- irf_s2_mean[, 1, i]

    }

 }

# Close cluster
  parallel::stopCluster(cl)

  list(irf_s1_mean = irf_s1_mean, irf_s1_low = irf_s1_low, irf_s1_up = irf_s1_up,
       irf_s2_mean = irf_s2_mean, irf_s2_low = irf_s2_low, irf_s2_up = irf_s2_up,
       fz          = fz, specs = specs)

}
