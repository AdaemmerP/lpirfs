
#' @name slp_estim
#' @title Function to compute linear smooth local projections
#' @description Function to compute linear impulse responses with smooth local projection approach.
#' The function is based on the R-code by Barnichon and Brownless (2018), available on GitHub.
#' @param y A \link{matrix} with the endogenous variable
#' @param w A matrix with the endogenous variables (including constant, trend, etc.).
#' @param x A matrix with the variable to shock with.
#' @param hor Integer. Number of horizons for impulse responses.
#' @param h1 Integer. Start of local projections. Default is 1.
#' @param r Integer. Order of the limit polynomial.
#' @param K Integer. Number for k-fold cross validation. The default value is 5.
#' @param type Character. Type of estimation. Default is "smooth" for smooth local projections. "reg" estimates regular impulse responses.
#' @param specs List. A list with values, built in slp_lin.
#'
#' @importFrom splines spline.des
#' @importFrom utils tail
#' @importFrom Matrix Matrix
#' @importFrom Matrix t
#' @importFrom Matrix solve
#'
#' @keywords internal
#'
#' @return A list with estimation results
#'
#' @source \url{https://github.com/ctbrownlees/R-Package-lproj}
#'
#'
#' @references
#'
#' Barnichon, R., Brwonless, C. (2018), "Impulse Response Estimation By Smooth Local Projections",
#' \emph{The Review of Economics and Statistics}, Forthcoming.
#'

slp_estim <- function(y, w, x, hor, h1, r, K, type, lambda, specs){

  h1    <- 1
  const <- TRUE
  zero  <- FALSE

  # dimensions
  Tdim   <- length(y)

  # construct basis
  if(type=='smooth'){
    hor.range <- h1:hor
    bdeg    <- 3
    knots   <- seq(-bdeg + h1, hor + bdeg, 1)
    basis   <- splines::spline.des(knots, hor.range, bdeg + 1, 0 * hor.range ,outer.ok=TRUE)$design
  }

  # 1 shock std dev
  if( !is.null(w) ){
    delta <- summary(stats::lm(x ~ 0+w))$sigma
  } else  {
    delta <- stats::sd(x)
  }

  # dimensions
  HR  <- hor+1-h1
  TS  <- Tdim*HR
  if( type=='reg' ){
    XS  <- HR
  } else {
    XS  <- ncol(basis)
  }
  WS  <- HR
  if( !is.null(w) | const==TRUE ){
    NW  <- ncol(w)
  } else {
    NW  <- 0
  }

  # y
  IDX <- matrix(0,TS,2)
  Y   <- rep(NA,TS)
  Xb  <- matrix(0,TS,XS)
  Xc  <- array(0,dim=c(TS,HR,NW))

  # construct Y and X
  for( t in 1:(Tdim-h1) ){
    idx.beg <- (t-1)*HR + 1
    idx.end <- t*HR

    IDX[ idx.beg:idx.end , 1 ] <- t
    IDX[ idx.beg:idx.end , 2 ] <- h1:hor

    # y
    y.range <- (t+h1) : min((t+hor),Tdim)
    Y [ idx.beg:idx.end ] <- c( y[ y.range ] , rep(NA,HR-length(y.range)) )

    # x
    if( type=='reg' ){
      Xb[ idx.beg:idx.end , ] <- diag(HR)*x[t]
    } else {
      Xb[ idx.beg:idx.end , ] <- basis*x[t]
    }

    # w
    for( i in seq_len(NW) ){
      Xc[ idx.beg:idx.end ,  , i ] <- diag(HR)*w[t,i]
    }
  }

  X   <- cbind(Xb)
  for( i in seq_len(NW)){
    X <- cbind(X,Xc[,,i])
  }
  X  <- Matrix::Matrix(X,sparse=TRUE)

  sel <- !is.na(Y)
  IDX <- IDX[sel, ]
  Y   <- Y[sel]
  X   <- X[sel, ]
  TS  <- length(Y)

  XX <- Matrix::t(X)%*%X
  XY <- Matrix::t(X)%*%Y

  # penalty
  P <- matrix(0, ncol(X), ncol(X))
  P <- Matrix::Matrix(P, sparse=TRUE)

  if( type=='smooth' ){
    D   <- diag(XS)
    for (k in seq_len(r)) D <- diff(D)

    if( zero ){
      DP     <- rep(0,XS)
      DP[XS] <- 1
      D      <- rbind(D,DP)
    }

    P[1:XS,1:XS] <- t(D) %*% D
  }

  ir    <- matrix(0, hor+1,     length(lambda))
  theta <- matrix(0, ncol(X), length(lambda))
  mul   <- matrix(0, HR,      length(lambda))

  for( i in 1:length(lambda) ){

    A           <- XX + lambda[i]*TS*P
    b           <- XY
    theta[ , i] <- as.vector(Matrix::solve( A , b ))

    if(type == 'reg'){
      mul[,i]   <- theta[1:XS, i]
    } else {
      beta        <- theta[1:XS, i]
      mul[ , i]   <- as.matrix(basis)%*%as.vector(beta)
    }

    ir[(h1+1):(hor+1),i]   <- mul[ , i]*delta
  }

  obj        <- list()
  obj$type   <- type

  if(type=='smooth'){
    obj$basis <- basis
  }
  obj$h1     <- h1
  obj$hor    <- hor
  obj$XS     <- XS
  obj$HR     <- HR
  obj$Tdim   <- Tdim
  obj$TS     <- TS
  obj$IDX    <- IDX
  obj$y      <- y
  obj$x      <- x
  obj$w      <- w
  obj$Y      <- Y
  obj$X      <- X
  obj$theta  <- theta
  obj$mul    <- mul
  obj$lambda <- lambda
  obj$P      <- P
  obj$ir     <- ir
  obj$delta  <- delta


################################################################################
########################### Cross validation ###################################
################################################################################

  if(isTRUE(specs$use_cv)){

    Tdim   <- obj$Tdim
    L   <- length(obj$lambda)

    ind <- ceiling( (obj$IDX[,1]/Tdim)*K )
    rss <- rep(0,L)

    for( l in 1:L ){

      rss.l <- rep(0, K)

      for( i in 1:K ){

        Y.in   <- obj$Y[ ind != i ]
        X.in   <- obj$X[ ind != i , ]
        Y.out  <- obj$Y[ ind == i ]
        X.out  <- obj$X[ ind == i , ]

        A        <- Matrix::t(X.in)%*%X.in + obj$lambda[l] * obj$TS * ((K-1)/K) * obj$P
        b        <- Matrix::t(X.in)%*%Y.in
        beta     <- Matrix::solve(A, b)
        rss.l[i] <- mean(data.matrix((Y.out - X.out%*%beta)^2))
      }

      rss[l] <- mean(rss.l)
    }

    obj$rss     <- rss
    obj$idx.opt <- utils::tail(which(min(rss)==rss), 1)
    obj$ir.opt  <- obj$ir[ , tail(which(min(rss)==rss), 1)]


  }

  obj

}
