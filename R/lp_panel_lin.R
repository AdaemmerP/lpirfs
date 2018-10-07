#' @name lp_panel_lin
#' @title Estimate impulse responses with local projections for panel data
#' @description This function estimates impulse responses with local projections for panel data with fixed effects
#'              as in e.g. Jordà et al. (2018). It uses the \emph{plm()} function from the plm package to estimate the irfs.
#'               The function also allows to estimate cumulative impulse responses.
#' @param data_set A \link{data.frame}, containing the entire panel data set. The first column has to be the
#'                 variable denoting the cross-section. The second column has to be the
#'                 variable denoting the time-section.
#' @param sample   Boolean or character. Use full sample? TRUE (default). To estimate a subset, you have to provide
#'                 a sequence of those dates to use. This sequence has to be in the same format as the second column,
#'                 which contains the time-section.
#' @param endog_data Character. Name of the endogenous variable. You can only provide one endogenous variable at a time.
#' @param cumul_mult Boolean. Estimate cumulative multipliers? TRUE or FALSE. If TRUE, cumulative responses
#'                   are estimated via: \deqn{y_(t+h) - y_(t-1)} for h = 0,..., H-1.
#' @param shock      Character. The column name of the variable to shock with.
#' @param diff_shock Boolean. Take first differences of the shock variable? TRUE or FALSE.
#' @param iv_reg     Boolean. Use instrument variable approach? TRUE or FALSE.
#' @param instrum    NULL or Character. The name(s) of the instrument variable(s) if iv_reg = TRUE.
#' @param robust_se Boolean. Estimate robust standard errors á la Driscoll and Kraay (1998)? TRUE or FALSE.
#' @param c_exog_data NULL or Character. Names of exogenous variables with contemporaneous impact.
#' @param l_exog_data NULL or Character. Names of exogenous variables with lagged impact.
#' @param lags_exog_data Integer. Lag length for the exogenous data with lagged impact.
#' @param c_fd_exog_data NULL or Character. Names of exogenous variables with contemporaneous impact of first differences.
#' @param l_fd_exog_data NULL or Character. Names of exogenous variables with lagged impact of first differences.
#' @param lags_fd_exog_data NaN or Integer. Number of lags for first differenced variables.
#' @param confint Double. Width of confidence bands. 68\% = 1; 90\% = 1.65; 9\% = 1.96.
#' @param hor Integer. Number of horizons for impulse responses.
#'
#' @author Philipp Adämmer
#'
#' @return A list containing:
#'
#'\item{irf_lin_mean}{A \link{matrix}, containing the impulse responses.
#'                   The columns are the horizons.}
#'
#'\item{irf_lin_low}{A \link{matrix}, containing all lower confidence bands.
#'                   The columns are the horizons.}
#'
#'\item{irf_lin_up}{A \link{matrix}, containing all upper confidence bands.
#'                                   The columns are the horizons.}
#'
#'\item{reg_summaries}{Regression output for each horizon.}
#'
#'\item{xy_data_sets}{Panel data sets with endogenous and exogenous variables for each horizon.}
#'
#'\item{specs}{A list with data properties for the plot function.}
#'
#' @importFrom dplyr lead lag filter
#' @importFrom plm plm
#' @importFrom lmtest coeftest
#' @export
#'
#' @references
#'
#' Driscoll, J.C., and Kraay, A.C. (1998). "Consistent Covariance Matrix Estimation with Spatially Dependent
#' Panel Data", \emph{Review of Economics and Statistics}, 80(4), pp. 549–560.
#'
#' Jordà, Ò. (2005). "Estimation and Inference of Impulse Responses by Local Projections."
#' \emph{American Economic Review}, 95 (1): 161-182.
#'
#' Jordà, Ò., Schualrick, M., Taylor, A.M. (2018). "Large and State-Dependent Effects of Quasi-Random Monetary Experiments",
#' \emph{NBER} working paper 23074, \emph{FRBSF} working paper 2017-02.
#'
#' @examples
#'\donttest{
#'
#'# This example aims to replicate the STATA example for panel estimation, provided on
#'# Òscar Jordà's website (https://sites.google.com/site/oscarjorda/home/local-projections)
#'# It estimates the impulse reponse of the ratio of (mortgage lending/GDP) to a
#'# +1% change in the short term interest rate
#'
#'# Load libraries to download and read excel file from the website
#'  library(httr)
#'  library(readxl)
#'  library(dplyr)
#'
#'# Retrieve the JST Macrohistory Database
#'  url_jst <-"http://www.macrohistory.net/JST/JSTdatasetR3.xlsx"
#'  GET(url_jst, write_disk(jst_link <- tempfile(fileext = ".xlsx")))
#'  jst_data <- read_excel(jst_link, 2L)
#'
#'# Swap the first two columns so that 'country' is the first (cross section) and 'year' the
#'# second (time section) column
#'  jst_data <- jst_data %>%
#'              dplyr::select(country, year, everything())
#'
#'# Prepare variables. This is based on the STATA 'data.do' file on Òscar Jordà's website
#'   data_set <- jst_data %>%
#'                mutate(stir     = stir)                         %>%
#'                mutate(mortgdp  = 100*(tmort/gdp))              %>%
#'                mutate(hpreal   = hpnom/cpi)                    %>%
#'                group_by(country)                               %>%
#'                mutate(hpreal   = hpreal/hpreal[year==1990][1]) %>%
#'                mutate(lhpreal  = log(hpreal))                  %>%
#'
#'                mutate(lhpy     = lhpreal - log(rgdppc))        %>%
#'                mutate(lhpy     = lhpy - lhpy[year == 1990][1]) %>%
#'                mutate(lhpreal  = 100*lhpreal)                  %>%
#'                mutate(lhpy     = 100*lhpy)                     %>%
#'                ungroup()                                       %>%
#'
#'                mutate(lrgdp    = 100*log(rgdppc))              %>%
#'                mutate(lcpi     = 100*log(cpi)) 		            %>%
#'                mutate(lriy     = 100*log(iy*rgdppc))           %>%
#'                mutate(cay      = 100*(ca/gdp))                 %>%
#'                mutate(tnmort   = tloans - tmort)               %>%
#'                mutate(nmortgdp = 100*(tnmort/gdp))             %>%
#'
#'              # Prepare instrument
#'                mutate(lnarrow  = log(narrowm))                 %>%
#'                mutate(rlnarrow = lnarrow - lcpi)               %>%
#'                dplyr::select(country, year, mortgdp, stir, ltrate, lhpy,
#'                              lrgdp, lcpi, lriy, cay, nmortgdp, rlnarrow)
#'
#'
#'# Use sample from 1870 - 2013 BUT exclude WWI and WWII
#'   sample <-   seq(1870, 2016)[which(!(seq(1870, 2016) %in%
#'                               c(seq(1914, 1918),
#'                                 seq(1939, 1947),
#'                                 seq(2014, 2016))))]
#'
#'# Estimate panel model
#' results_panel <-  lp_panel_lin(data_set          = data_set,
#'                                sample            = sample,
#'                                endog_data        = "mortgdp",
#'                                cumul_mult        = TRUE,
#'
#'                                shock             = "stir",
#'                                diff_shock        = TRUE,
#'                                iv_reg            = FALSE,
#'                                instrum           = NULL,
#'                                robust_se         = TRUE,
#'
#'                                c_exog_data       = "cay",
#'                                l_exog_data       = NULL,
#'                                lags_exog_data    = NULL,
#'                                c_fd_exog_data    = colnames(data_set)[c(seq(4,9),11)],
#'                                l_fd_exog_data    = colnames(data_set)[c(seq(3,9),11)],
#'                                lags_fd_exog_data = 2,
#'
#'                                confint           = 1.67,
#'                                hor               = 10)
#'
#'# Create and plot irfs
#'  plot_panel_lin <- plot_lin(results_panel)
#'
#'  plot(plot_panel_lin[[1]])
#'
#'
#'# Create and add instrument to data_set
#'
#'  instrument <- 0.9*data_set$stir + rnorm(length(data_set$stir), 0, sd(na.omit(data_set$stir)))
#'  data_set   <- data_set %>%
#'                mutate(instrument = instrument)
#'
#'
#' # Estimate panel model with iv
#' results_panel <-  lp_panel_lin(data_set          = data_set,
#'                                sample            = sample,
#'                                endog_data        = "mortgdp",
#'                                cumul_mult        = TRUE,
#'
#'                                shock             = "stir",
#'                                diff_shock        = TRUE,
#'                                iv_reg            = TRUE,
#'                                instrum           = "instrument",
#'                                robust_se         = TRUE,
#'
#'                                c_exog_data       = "cay",
#'                                l_exog_data       = NULL,
#'                                lags_exog_data    = NULL,
#'                                c_fd_exog_data    = colnames(data_set)[c(seq(4,9),11)],
#'                                l_fd_exog_data    = colnames(data_set)[c(seq(3,9),11)],
#'                                lags_fd_exog_data = 2,
#'
#'                                confint           = 1.67,
#'                                hor               = 10)
#'
#'# Create and plot irfs
#'  plot_panel_lin <- plot_lin(results_panel)
#'  plot(plot_panel_lin[[1]])
#'
#'}
#'
lp_panel_lin <- function(
                    data_set          = NULL,
                    sample            = "Full",
                    endog_data        = NULL,
                    cumul_mult        = TRUE,

                    shock             = NULL,
                    diff_shock        = TRUE,
                    iv_reg            = FALSE,
                    instrum           = NULL,
                    robust_se         = FALSE,

                    c_exog_data       = NULL,
                    l_exog_data       = NULL,
                    lags_exog_data    = NaN,
                    c_fd_exog_data    = NULL,
                    l_fd_exog_data    = NULL,
                    lags_fd_exog_data = NaN,
                    confint           = NULL,
                    hor               = NULL){

# Check inputs

 # Check whether column names are named properly
  if(any(colnames(data_set)[3:dim(data_set)[2]] %in% c("cross_id", "date_id"))){
    stop("You cannot use the column names 'cross_id' or 'date_id' besides the first two columns of your data.frame.
         Please rename them." )
  }

 # Check whether data_set is given
  if(is.null(data_set)){
    stop("You have to provide the data set." )
  }


 # Check whether name for endogenous variable is given
  if(is.null(endog_data)){
    stop("You have to provide the name of the endogenous variable." )
  }


  # Check whether shock is given
  if(is.null(shock)){
    stop("You have to provide the name of the variable to shock with." )
  }

  # Check whether instrument is given if iv_reg = TRUE
  if(isTRUE(iv_reg) & is.null(instrum)){
    stop("You have to provide the name of the instrument." )
  }




  # Rename first two column names of data.frame
  colnames(data_set)[1]     <- "cross_id"
  colnames(data_set)[2]     <- "date_id"

  # Create list to store inputs
  specs <- list()

  # Specify inputs
  specs$sample              <- sample
  specs$endog_data          <- endog_data
  specs$cumul_mult          <- cumul_mult

  specs$shock               <- shock
  specs$diff_shock          <- diff_shock

  specs$instrum             <- instrum
  specs$iv_reg              <- iv_reg
  specs$robust_se           <- robust_se

  specs$exog_data           <- colnames(data_set)[which(! colnames(data_set) %in% c("cross_id", "date_id"))]
  specs$c_exog_data         <- c_exog_data
  specs$l_exog_data         <- l_exog_data
  specs$lags_exog_data      <- lags_exog_data

  specs$c_fd_exog_data      <- c_fd_exog_data
  specs$l_fd_exog_data      <- l_fd_exog_data
  specs$lags_fd_exog_data   <- lags_fd_exog_data

  specs$confint             <- confint
  specs$hor                 <- hor

  specs$model_type          <- 1
  specs$endog               <- 1        # Set the number of endogenous variables for plot function
  specs$column_names        <- endog_data


  #--- Create data
  lin_panel_data            <- create_panel_data(specs, data_set)

  # Extract endogenous and exogenous data
  specs            <- lin_panel_data$specs
  x_reg_data       <- lin_panel_data$x_reg_data
  y_data           <- lin_panel_data$y_data

  x_instrument     <- lin_panel_data$x_instrument


  # Prepare matrices to store irfs
  irf_panel_mean   <- matrix(NaN, 1, specs$hor)
  irf_panel_up     <- matrix(NaN, 1, specs$hor)
  irf_panel_low    <- matrix(NaN, 1, specs$hor)


  # List to store regression results
  reg_summaries    <- list(rep(NaN), specs$hor)
  xy_data_sets     <- list(rep(NaN), specs$hor)


  # Prepare formula names
  y_reg_name     <- specs$endog_data
  x_reg_names    <- names(x_reg_data)


  # Make formula for normal panel estimation without iv
  ols_formula    <- paste(y_reg_name, "~",
                          paste(x_reg_names[!(x_reg_names %in% c("cross_id", "date_id"))], collapse = " + "))

  # Make formula for iv_regression
  if(isTRUE(specs$iv_reg)){

    # Make formula string for iv_regression
    iv_formula     <- paste(ols_formula, paste(x_reg_names[!(x_reg_names %in% c("cross_id",
                                                                                "date_id", y_reg_name, specs$shock))], collapse = " + "), sep =  "| ")
    iv_formula     <- paste(iv_formula, "+", paste(specs$instrum, collapse =  " + "))

    # Convert string to formula
    plm_formula    <- stats::as.formula(iv_formula)

               } else {


    # Convert ols string to formula
    plm_formula    <- stats::as.formula(ols_formula)
  }




  # Loop to estimate irfs with local projections
  for(ii in 1:specs$hor){

    if(isTRUE(specs$iv_reg)){

      yx_data        <- suppressMessages(
                        left_join(y_data[[ii]], x_reg_data)  %>%
                        left_join(x_instrument)              %>%
                        stats::na.omit()
                        )

               }   else    {


      yx_data        <- suppressMessages(
                        left_join(y_data[[ii]], x_reg_data)  %>%
                        stats::na.omit()
                        )

    }


    # Choose sample if specified
    if(!(specs$sample[1] == 'Full')){

      yx_data      <-   yx_data %>%
                        dplyr::filter(date_id %in% specs$sample)

    }


    # Do panel estimation
    panel_results  <- plm::plm(formula = plm_formula,
                          data     = yx_data,
                          index    = c("cross_id", "date_id"),
                          model    = "within")


    # Estimate confidence bands with robust standard errors?
    if(isTRUE(specs$robust_se)){

      # Estimate model robust standard errors a la Driscoll and Kraay (1998)
      reg_results <-  lmtest::coeftest(panel_results, vcov. = plm::vcovSCC, maxlag = ii)

      # Estimate irfs and confidence bands
      irf_panel_mean[[1, ii]]   <- reg_results[1, 1]
      irf_panel_up[[1,   ii]]   <- reg_results[1, 1] + specs$confint*reg_results[1,2]
      irf_panel_low[[1,  ii]]   <- reg_results[1, 1] - specs$confint*reg_results[1,2]

                         }      else      {

      reg_results <- summary(panel_results)

      # Estimate irfs and confidence bands
      irf_panel_mean[[1, ii]]   <- reg_results$coefficients[1, 1]
      irf_panel_up[[1,   ii]]   <- reg_results$coefficients[1, 1] + specs$confint*reg_results$coefficients[1,2]
      irf_panel_low[[1,  ii]]   <- reg_results$coefficients[1, 1] - specs$confint*reg_results$coefficients[1,2]

    }


    # Save regression results and data_sets
    reg_summaries[[ii]]       <- reg_results
    xy_data_sets[[ii]]        <- yx_data

  }


  return(list(irf_panel_mean   = irf_panel_mean,
              irf_panel_low    = irf_panel_low,
              irf_panel_up     = irf_panel_up,
              reg_summaries    = reg_summaries,
              xy_data_sets     = xy_data_sets,
              specs            = specs
              ))




}
