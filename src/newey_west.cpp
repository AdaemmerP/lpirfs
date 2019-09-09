#include <RcppArmadillo.h>
using namespace Rcpp;
//' @name newey_west
//' @title Compute OLS parameters and robust standard errors based on Newey-West estimator
//' @description  Compute OLS parameters and robust standard errors based on Newey and West (1987).
//' The function is based on the Matlab code by James P. LeSage.
//' @param y Numeric vector.
//' @param x Numeric matrix.
//' @param h Integer.
//' @return A list. The first element contains the estimated OLS parameters, the second element
//' the Newey West covariance matrix, the third element the normal OLS covariance matrix and the fourth element the R^2.
//' @keywords internal
//' @references
//' Newey, W.K., and West, K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
//' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.
// [[Rcpp::export]]
List newey_west(NumericVector y, NumericMatrix x, int h){
  NumericMatrix V;
  arma::mat G, M, xx, xx_one, yy, M1, M2, ga, g1, w, za, xpxi, emat, hhat, beta_hat;
  arma::vec w1, beta, resids, resids_mean;
  int nrow_hhat, a, nobs, num_exog, nlag;
  double sigma_hat, y_hat, ssr, ssm, r_sq ;
  List ret(4);


  // OLS
  xx       = as<arma::mat>(x);
  xx_one   = arma::ones<arma::mat>(xx.n_rows, 1); // Insert ones for constant
  xx.insert_cols(0, xx_one);

  yy       = as<arma::vec>(y);
  num_exog = xx.n_cols;
  nobs     = xx.n_rows;

  xpxi     = inv(xx.t()*xx);
  beta     = xpxi*xx.t()*yy;
  resids   = yy - xx*beta;

  y_hat       = mean(y);
  resids_mean = y - y_hat;

  // Estimate sum of squared residuals
  ssr       = std::inner_product(resids.begin(),
                                 resids.end(), resids.begin(), 0.0);

  // Estimate sum of squared deviations from endogenous mean
  ssm       = std::inner_product(resids_mean.begin(),
                                 resids_mean.end(), resids_mean.begin(), 0.0);

  // Estimate R^2 of OLS model
  r_sq      = 1 - ssr/ssm;

  // Estimate normal Cov matrix of estimators
  sigma_hat = ssr/double(nobs - num_exog);
  beta_hat = sigma_hat*xpxi;


  // Start Newey-West
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

  ret[0]  = beta;
  ret[1]  = V;
  ret[2]  = beta_hat;
  ret[3]  = r_sq;
  return (ret);

}
