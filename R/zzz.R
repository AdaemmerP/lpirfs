.onLoad <- function(libname, pkgname){

 utils::globalVariables(c(".", "trend", "s", "x", "low", "up", "endog_data",
                          "x_lin", "d", "y_lin", "cross_id", "date_id", ":=",
                          "specs", "irf_lin_mean", "irf_lin_low", "irf_lin_up"))

}
