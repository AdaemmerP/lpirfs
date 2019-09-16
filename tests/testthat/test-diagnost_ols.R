
context("Test diagnostic function ")




test_that("Test whether diagnostic function returns same values as base function", {

  xx <- matrix(rnorm(500), nrow = 100, 5)
  yy <- matrix(rnorm(100))

# Get diagnostocs for summary()
get_diagnost              <- lpirfs::ols_diagnost(yy, xx)
R_sqrd_lpirfs             <- get_diagnost[[3]]
R_sqrd_adj_lpirfs         <- get_diagnost[[4]]
F_stat_lpirfs             <- get_diagnost[[5]]
p_value_F_lpirfs          <- stats::pf(F_stat_lpirfs, get_diagnost[[6]], get_diagnost[[7]], lower.tail = F)

lm_modeĺ <- lm(yy ~ xx)
summary_lm <- summary(lm_modeĺ)
summary_lm$fstatistic

testthat::expect_equal(R_sqrd_lpirfs, summary_lm$r.squared)
testthat::expect_equal(R_sqrd_adj_lpirfs, summary_lm$adj.r.squared)
testthat::expect_equal(F_stat_lpirfs, summary_lm$fstatistic[1], check.attributes = FALSE)


})
