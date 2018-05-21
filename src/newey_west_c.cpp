#include <RcppArmadillo.h>
using namespace Rcpp;
//' @name newey_west_c
//' @title Compute newey west covariance matrix
//' @description This function estimates OLS's coefficients and the corresponding covariance matrix
//' based on Newey and West (1987).
//' The function is based on the Matlab code by James LeSage.
//'
//' @param y Numeric vector
//' @param x Numeric Matrix
//' @param h Integer
//' @references
//' Newey W.K., and West K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
//' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.
// [[Rcpp::export]]
List newey_west_c(NumericVector y, NumericMatrix x, int h){
  NumericMatrix V;
  arma::mat G, M, xx, xx_one, yy, M1, M2, ga, g1, w, za, xpxi, emat, hhat;
  arma::vec w1, beta, resids;
  int nrow_hhat, a, p, nobs, num_exog, nlag;
  List ret(2);

  xx       = as<arma::mat>(x);
  xx_one   = arma::ones<arma::mat>(xx.n_rows, 1);
  xx.insert_cols(0, xx_one);

  yy       = as<arma::vec>(y);
  num_exog = xx.n_cols;
  nobs     = xx.n_rows;

  nlag     = h; // Based on Ramey and Zubairy (2018);
  xpxi     = inv(xx.t()*xx);
  beta     = xpxi*xx.t()*yy;
  resids   = yy - xx*beta;

  emat     = arma::zeros<arma::mat>(nobs, num_exog);
  emat.cols(0, num_exog-1).each_col()   = resids;
  emat     = emat.t();
  hhat     = emat%xx.t();

  w        = arma::zeros<arma::vec>(2*nlag + 1);
  G        = arma::zeros<arma::mat>(num_exog, num_exog);
  a        = 0;


  for (int i = 0; i < nlag; ++i){

    ga                 = arma::zeros<arma::mat>(num_exog, num_exog);
    w(nlag + a)        = (nlag + 1 - a)/double(nlag + 1);
    M                  = hhat;
    nrow_hhat          = M.n_rows;
    M1                 = M(arma::span(0, nrow_hhat - 1), arma::span(a, nobs - 1));
    M2                 = M(arma::span(0, nrow_hhat - 1), arma::span(0, nobs - 1 - a));
    M2                 = M2.t();
    za                 = M1*M2;

    if (a == 0){

      ga  = ga + za;

    } else {

      ga  = ga + za + za.t();

    }

    G  = G +  w(nlag + a , 0)*ga;

    a = a + 1;


  }
  V = wrap(xpxi*G*xpxi);

  ret[0]  = beta;
  ret[1]  = V;
  return (ret);

}
