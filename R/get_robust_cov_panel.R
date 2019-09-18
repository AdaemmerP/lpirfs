

#' Function to get robust covariance matrix for panel data
#' @name get_robust_cov_panel
#' @param panel_results Plm object from estimation
#' @param specs List with specifications
#' @importFrom plm vcovBK vcovDC vcovG vcovHC vcovNW vcovSCC
#' @return Object with robust covariance matrix
#' @export
#'

get_robust_cov_panel <- function(panel_results, specs){

      if(specs$robust_cov         == "vcovBK"){

        reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovBK(panel_results,
                                                                      type    = specs$robust_type,
                                                                      cluster = specs$robust_cluster))

      } else if (specs$robust_cov == "vcovDC"){

        reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovDC(panel_results,
                                                                      type    = specs$robust_type))


      } else if (specs$robust_cov == "vcovHC"){

        reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovHC(panel_results,
                                                                      method   = specs$robust_method,
                                                                      type     = specs$robust_type,
                                                                      cluster  = specs$robust_cluster))

      } else if (specs$robust_cov == "vcovNW"){

        reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovNW(panel_results,
                                                                      type    = specs$robust_type,
                                                                      maxlag  = specs$robust_maxlag))
      } else if (specs$robust_cov == "vcovSCC"){

        reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovSCC(panel_results,
                                                                       type    = specs$robust_type,
                                                                       maxlag  = specs$robust_maxlag))

      }

        return(reg_results)

}
