#include <RcppArmadillo.h>
using namespace Rcpp;
//' @name newey_west_tsls
//' @title Compute 2SLS parameters and robust standard errors based on Newey-West
//' @description  Compute 2SLS parameters and robust standard errors based on Newey and West (1987).
//' Part of the function is based on the Matlab code by James P. LeSage.
//' @param y Numeric vector.
//' @param x Numeric matrix.
//' @param z Numeric matrix.
//' @param h Integer.
//' @return A list. The first element contains the estimated 2SLS parameters and the second element
//' the covariance matrix of these parameters.
//' @keywords internal
//' @references
//' Newey, W.K., and West, K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
//' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.
//' Wooldridge, J.M. (2002), Econometric Analysis of Cross Section and Panel Data, The MIT Press.
// [[Rcpp::export]]
List newey_west_tsls(NumericVector y, NumericMatrix x, NumericMatrix z, int h){
  NumericMatrix V;
  arma::mat G, M, xx, xx_one, yy, xx_hat, zz, M1, M2, ga, g1, w, za, xpxi, emat, hhat;
  arma::vec w1, beta_iv, resids;
  int nrow_hhat, a, nobs, num_exog, nlag;
  List ret(2);


  // 2SLS
  xx       = as<arma::mat>(x);
  xx_one   = arma::ones<arma::mat>(xx.n_rows, 1); // Insert ones for constant
  xx.insert_cols(0, xx_one);

  yy       = as<arma::vec>(y);

  zz       = as<arma::mat>(z);
  zz.insert_cols(0, xx_one);

  // Build x_hat matrix with instrument matrix
  xx_hat   = zz*inv(zz.t()*zz)*zz.t()*xx;

  // Estimate beta_iv
  xpxi     = inv(xx_hat.t()*xx_hat);
  beta_iv  = xpxi*xx_hat.t()*yy;

  // Estimate corrected residuals
  resids   = yy - xx*beta_iv;

  num_exog = xx.n_cols;
  nobs     = xx.n_rows;



  // Start Newey-West
  // Use original data for newey west estimator
  xpxi     = inv(xx.t()*xx);
  nlag     = h; // The lag increases with the horizons
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

  ret[0]  = beta_iv;
  ret[1]  = V;
  return (ret);

}
