#' @name get_robust_vcxt_panel
#' @title Create and returns a function to estimate hc and clustered standard errors for panel data.
#' @param func_name A character.
#'
#' @return A function.
#' @export
#' @keywords internal
#' @references
#'
#'  Croissant, Y., Millo, G. (2017). "Robust Standard Error Estimators for Panel Models: A Unifying Approach." \emph{Journal of Statistical Software}, 27(2), 1-43. doi:
#' 	10.18637/jss.v082.i03 (URL: \url{https://www.jstatsoft.org/article/view/v082i03}).
#'
get_robust_vcxt_panel <- function(func_name){

                        Vw    <- function(x) vcovHC(x, method  = "white1")
                        Vcx   <- function(x) vcovHC(x, cluster = "group", method = "arellano")
                        Vct   <- function(x) vcovHC(x, cluster = "time",  method = "arellano")
                        Vcxt  <- function(x) Vcx(x) + Vct(x) - Vw(x)

                        return(get(func_name))

}




