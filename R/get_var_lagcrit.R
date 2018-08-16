#'@name  get_var_lagcrit
#'@title Computes AICc, AIC and BIC for VAR
#'@description Computes AICc, AIC and BIC for VAR models.
#'@param endog_data A \link{data.frame} with endogenous variables for the VAR
#'@param specs A \link{list} created in \link{lp_lin}
#'@keywords internal
#'@references
#'
#' Akaike, H. (1974). "A new look at the statistical model identification",
#' \emph{IEEE Transactions on Automatic Control}, 19 (6): 716–723.
#'
#' Hamilton, J. D. (1994). "Time Series Analysis."
#' Princeton: Princeton University Press.
#'
#' Hurvich, C. M., and Tsai, C.-L. (1989), "Regression and time series model selection in small samples",
#' \emph{Biometrika}, 76(2): 297–307
#'
#' Lütkepohl, H. (2005). "New Introduction to Multiple Time Series Analysis.",
#' New York: Springer.
#'
#' Schwarz, Gideon E. (1978). "Estimating the dimension of a model",
#' \emph{Annals of Statistics}, 6 (2): 461–464.
#'
#' @return A list with lag length criteria
#'
get_var_lagcrit <- function (endog_data,
                                    specs    = NULL){
# Maximum number of lags
  max_lags   <- specs$max_lags

# Number of endogenous variables (i.e. equations)
  K          <- ncol(endog_data)


# Count contemporaneous variables
  if(!is.null(specs$exog_data)){
    num_contemp <- ncol(specs$exog)
  } else {
    num_contemp <- 0
  }


# Count exogenous variables
  if(!is.null(specs$exog_data)){
    num_exog  <- ncol(specs$exog)
    lags_exog <- specs$lags_exog
  } else {
    num_exog  <- 0
    lags_exog <- 0
  }


# Count constant, trend, trend^2
  if(specs$trend == 0){
    K_cte       <- 1
  } else if(specs$trend == 1){
    K_cte       <- 2
  } else if(specs$trend == 2){
    K_cte       <- 3
  }

# Construct lagged data
  y_lin      <- specs$y_lin
  x_lin      <- specs$x_lin

# Number of observations
  n      <- nrow(y_lin[[length(y_lin)]])

# Prepare matrices to store values
  lagcrit_vals           <- matrix(NA, nrow = max_lags, ncol = 3)
  colnames(lagcrit_vals) <- c("AICc", "AIC", "BIC")

# Start of observation to guarantee equal sample length
  obs_start <- specs$max_lags

for (ii in 1:max_lags) {

# Get data
  yy           <- y_lin[[ii]][obs_start:nrow(y_lin[[ii]]),]
  xx           <- x_lin[[ii]][obs_start:nrow(x_lin[[ii]]),]

# Add constant for regression
  xx           <- cbind(xx, rep(1, nrow(xx)))

# Estimate model
  ols_resids   <- stats::lm.fit(x = xx, y = yy)$residuals
  sigma_det    <- det(crossprod(ols_resids)/n)

# Lag order
  m            <- ii

# Estimate log-liklihood for VAR (See Hamilton, p. 296)
  ll <- -(n/2)*log(sigma_det) - (n/2)*log(2*pi)*K - n/2*K


# Count number of parameters to estimate
  tp  <- m*K^2                 + # endogenous parameters
         num_exog*lags_exog*K  + # number of exogenous parameters
         num_contemp*K         + # number of contemporeaneous parameters
         K_cte*K                 # constant, trend and trend^2

# Estimate AIC
  lagcrit_vals[ii, 2] <- -2*(ll) + 2*tp

# Estimate AICc
  lagcrit_vals[ii, 1] <- lagcrit_vals[ii, 2] + 2*tp*(tp + 1)/(n - tp - 1)

# Estimate BIC
  lagcrit_vals[ii, 3] <- -2*(ll) + log(n)*tp

# Decrease start observation
  obs_start <- obs_start - 1

}

  order_vals <- apply(lagcrit_vals, 2, which.min)
  return(list(lagcrit_vals = lagcrit_vals, order_vals = order_vals))

}


