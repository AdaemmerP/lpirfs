
test_that("Test whether results from newey_west_cpp function coincide
          with NeweyWest from sandwich package", {

      # Newey West without prewhitening
            x <- matrix(rnorm(500), 100, 5)
            y <- matrix(rnorm(100))
            nlag <- round(runif(1, 0, 10))


            lm_obj <- lm(y  ~ x)

          # Newey West from sandwich
           nw_sandwich <- sandwich::NeweyWest(lm_obj, prewhite = F, lag = nlag)

          # Newey West from lpirfs
           nw_lpirfs   <- newey_west(y, x, nlag)[[2]]

           testthat::expect_equal(nw_sandwich, nw_lpirfs, tolerance=1e-10, ignore_attr = T)

            } )


test_that("Test whether prewhitening results coincide with results from sandwich package", {

            # Newey West without prewhitening
            x <- matrix(rnorm(500), 100, 5)
            y <- matrix(rnorm(100))
            nlag <- round(runif(1, 0, 10))


            lm_obj <- lm(y  ~ x)

            # Newey West from sandwich
            nw_sandwich <- sandwich::NeweyWest(lm_obj, prewhite = T, lag = nlag)

            # lpirfs pre whitening
            nw_results_pre     <- newey_west(y, x, nlag)
            x_u                <- nw_results_pre[[3]]
            xpxi               <- nw_results_pre[[4]]

            resid_pw        <- var_one(x_u)[[2]]
            D_mat           <- var_one(x_u)[[3]]

            nw_lpirfs       <- newey_west_pw(resid_pw, xpxi, D_mat, nlag)[[1]]

            testthat::expect_equal(nw_sandwich, nw_lpirfs, tolerance=1e-10, ignore_attr = T)

          } )


test_that("Test whether Newey-West 2SLS cov coincides with sandwich and AER package", {

  # Set seed for reproducibility
  set.seed(123)

  x <- matrix(rnorm(100), 100, 1)
  z <- matrix(rnorm(100), 100, 1)
  y <- matrix(rnorm(100))

  # Step 2: 2SLS Estimation using ivreg from AER package
  model_ivreg <- AER::ivreg(y ~ x | z)

  # Step 3: Compute Newey-West Standard Errors
  vcov_nw_2sls = summary(model_ivreg, vcov =  sandwich::NeweyWest(model_ivreg,
                                                        lag = 1,
                                                        prewhite = FALSE,
                                                        adjust = F),
                         df = Inf,
                         diagnostics = TRUE)$vcov
  testthat::expect_equal(newey_west_tsls(y, x, z, 1)[[2]], unname(vcov_nw_2sls), tolerance=1e-10)

} )

