context("Test newey west function")



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

           testthat::expect_equal(nw_sandwich, nw_lpirfs, tolerance=1e-6, check.attributes = FALSE)

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

            testthat::expect_equal(nw_sandwich, nw_lpirfs, tolerance=1e-6, check.attributes = FALSE)

          } )
