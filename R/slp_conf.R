
#' @name slp_conf
#' @title Estimates confidence bands by Newey West estimator
#' @description Compute confidence for smooth local projections by Newey and West (1987).
#' The function is based on the R-code by Barnichon and Brownless (2018), available on GitHub.
#' @param obj A list with values built in slp_estim.
#' @param l Integer. Numeric value.
#' @param specs A list with values, built in slp_lin.
#'
#' @return A \link{list}, named obj.
#' @keywords internal
#'
#' @importFrom Matrix t
#' @importFrom Matrix solve
#'
#' @references
#'
#' Barnichon, R., Brwonless, C. (2018), "Impulse Response Estimation By Smooth Local Projections",
#' emph{The Review of Economics and Statistics}, Forthcoming.
#'
#' Newey, W.K., and West, K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
#' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.
#'
#' @source \url{https://github.com/ctbrownlees/R-Package-lproj}
#'
#'
slp_conf <- function(obj , specs, l = 1 ){

  u       <- obj$Y - obj$X %*% obj$theta[, l];
  S       <- obj$X*(u%*%t(rep(1,ncol(obj$X))) )

  # BREAD
  bread   <- Matrix::solve(Matrix::t(obj$X)%*%obj$X + obj$lambda[l]*obj$TS*obj$P )

  # MEAT
  nlag    <- min(floor(1.2*(obj$Tdim)**(1/3)),obj$Tdim)
  nlag    <- obj$hor
  weights <- (nlag+1-(0:nlag))/(nlag+1)
  V       <- Matrix::t(S)%*%S

  for(i in 1:nlag){
    Gammai      <- Matrix::t(S[(i+1):obj$Tdim,] ) %*% S[1:(obj$Tdim-i),]
    GplusGprime <- Gammai+Matrix::t(Gammai)
    V           <- V + weights[i + 1]*GplusGprime
  }

  meat  <- V
  V     <- bread %*% meat %*% bread

  if(obj$type == 'reg'){
    se       <- sqrt( diag( V[ 1:(obj$hor+1-obj$h1) , 1:(obj$hor+1-obj$h1) ] ) )
    conf     <- matrix( 0 , length(se) , 2 )
    conf[,1] <- obj$mul[, l] + se*specs$confint    # stats::qnorm(0.10)
    conf[,2] <- obj$mul[, l] + se*specs$confint    # stats::qnorm(0.90)

               }  else {

    V        <- as.matrix(obj$basis) %*% V[ 1:obj$XS , 1:obj$XS ] %*% t(as.matrix(obj$basis))
    se       <- sqrt( diag( as.matrix(V) ) )
    conf     <- matrix( 0 , length(se) , 2 )
    conf[, 1] <- obj$mul[, l] - se*specs$confint
    conf[, 2] <- obj$mul[, l] + se*specs$confint

  }

  irc                           <- matrix(NA, obj$hor + 1, 2)
  irc[(1+obj$h1):(obj$hor+1), ] <- conf*obj$delta

  obj$se  <- se
  obj$irc <- irc

  obj
}
