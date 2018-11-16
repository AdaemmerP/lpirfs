context("get_var_lagcrit")

# Adapted VARselect function from 'vars' package
varSELECT     <- function (endog_data, lag.max = 2, type = c("const", "trend", "both",
                                                             "none"), season = NULL, exogen = NULL){

  y           <- as.matrix(endog_data)
  colnames(y) <- make.names(colnames(y))
  K           <- ncol(y)
  lag.max     <- abs(as.integer(lag.max))
  type        <- match.arg(type)
  lag         <- abs(as.integer(lag.max + 1))
  ylagged     <- embed(y, lag)[, -c(1:K)]
  yendog      <- y[-c(1:lag.max), ]
  sample      <- nrow(ylagged)

  rhs <- switch(type,
                const = rep(1, sample),
                trend = seq(lag.max + 1, length = sample),
                both  = cbind(rep(1, sample),
                              seq(lag.max + 1, length = sample)),
                none  = NULL)


  idx <- seq(K, K * lag.max, K)

  if (!is.null(rhs)) {
    detint <- ncol(as.matrix(rhs))
  }  else {
    detint <- 0
  }
  criteria <- matrix(NA, nrow = 2, ncol = lag.max)
  rownames(criteria) <- c("AIC(n)", "SC(n)")
  colnames(criteria) <- paste(seq(1:lag.max))

  for (i in 1:lag.max) {
    ys.lagged      <- cbind(ylagged[, c(1:idx[i])], rhs)
    sampletot      <- nrow(y)

    resids         <- lm.fit(x = ys.lagged, y = yendog)$residuals
    sigma.det      <- det(crossprod(resids)/sample)

    criteria[1, i] <- log(sigma.det) + (2/sample) *
                      (i * K^2 + K * detint)

    criteria[2, i] <- log(sigma.det) + (log(sample)/sample) *
      (i * K^2 + K * detint)

  }

  order <- apply(criteria, 1, which.min)
  return(list(selection = order, criteria = criteria))

}

# Load (endogenous) data
 endog_data     <- interest_rules_var_data

# Get results
 resultsVS <- varSELECT(endog_data, lag.max = 24, type= 'const')
 resultsVS$selection



 test_that("Test whether package function chooses same number of lags as
           VARselect function", {


   # Estimate linear model
results_lin <- lp_lin(endog_data,
                      lags_endog_lin = NaN,
                      lags_criterion = 'AIC',
                      exog_data      = NULL,
                      lags_exog      = NULL,
                      max_lags       = 24,
                      trend          = 0L,
                      shock_type     = 1L,
                      confint        = 1.96,
                      hor            = 12,
                      num_cores      = 1)



    specs             <- results_lin$specs

  # Get values of lag criteria
    results_lpirfs    <- get_var_lagcrit(endog_data, specs)

  # AIC results from VARselect function
    AIC_VS            <- resultsVS$selection[1]
    names(AIC_VS)     <- 'AIC'

  # AIC results from lpirfs
    AIC_lpirfs        <- results_lpirfs$order_vals[2]
    names(AIC_lpirfs) <- 'AIC'

    # AIC results from VARselect function
    BIC_VS            <- resultsVS$selection[2]
    names(BIC_VS)     <- 'BIC'

    # AIC results from lpirfs
    BIC_lpirfs        <- results_lpirfs$order_vals[3]
    names(BIC_lpirfs) <- 'BIC'

    expect_equal(AIC_VS, AIC_lpirfs)
    expect_equal(BIC_VS, BIC_lpirfs)

 } )



