#include <RcppArmadillo.h>
using namespace Rcpp;
//' @name find_lag_c
//' @title Compute optimal lag length
//' @description Determine optimal lag length based on 'AICc', 'AIC', or 'BIC' criterion.
//'
//' @param y List with left (endogenous) variables
//' @param x List with right (exogenous) variables
//' @param lag_crit Integer: 'AICc'= 1, 'AIC' = 2, 'BIC' = 3.

// [[Rcpp::export]]
NumericVector find_lag_c(List y, List x, int lag_crit, int h, int k, int max_lags){

  arma::mat xx, xx_one, yy, xpxi, emat, hhat;
  arma::vec w1, beta, resids, resds_sq;
  int p, n, rstart_y, rend_y, rend_x;
  double ssr, var_eps, ll;
  double pi = 3.141593;
  NumericVector crit_val(max_lags);


  for (int i = 0; i < max_lags; i++){

    yy       = as<arma::mat>(y[i]);
    yy       = yy.col(k - 1);
    rstart_y = h - 1;
    rend_y   = yy.n_rows - 1;
    yy       = yy.rows(rstart_y, rend_y);


    xx       = as<arma::mat>(x[i]);
    xx_one   = arma::ones<arma::mat>(xx.n_rows, 1);
    xx.insert_cols(0, xx_one);
    rend_x   = rend_y - h + 1;
    xx       = xx.rows(0, rend_x);


    p        = xx.n_cols + 1;
    n        = rend_y + 1;
    xpxi     = inv(xx.t()*xx);
    beta     = xpxi*xx.t()*yy;
    resids   = yy - xx*beta;
    resds_sq = resids%resids;
    ssr      = sum(resds_sq);
    var_eps  = ssr/n;

    ll       =  - n/2 * log(2*pi) - n/2 * log(var_eps) - ssr/(2 * var_eps);


    if (lag_crit == 1){

      crit_val[i] = (2 * p - 2 * ll) + (2*pow(p,2) + 2*p)/(n-p-1);

         } else if(lag_crit == 2) {

      crit_val[i] = 2*p - 2*ll;

          } else {

      crit_val[i] = -2*ll + log(n)*p;
    }

  }

  return(crit_val);

}
