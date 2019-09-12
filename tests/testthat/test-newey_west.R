context("Test whether Newey West coincides with sandwich package")



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

           expect_equal(nw_sandwich, nw_lpirfs, tolerance=1e-2, check.attributes = FALSE)

            } )
