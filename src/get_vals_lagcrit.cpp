#include <RcppArmadillo.h>
using namespace Rcpp;
//' @name get_vals_lagcrit
//' @title Compute values for lag length criteria
//' @description Compute values for 'AICc', 'AIC', or 'BIC'.
//'
//' @param y List with left (endogenous) variables.
//' @param x List with right (exogenous) variables.
//' @param lag_crit Integer: 'AICc'= 1, 'AIC' = 2, 'BIC' = 3.
//' @param h Integer.
//' @param k Integer.
//' @param max_lags Integer.
//' @references
//'
//' Akaike, H. (1974). "A new look at the statistical model identification", \emph{IEEE Transactions on Automatic Control}, 19 (6): 716–723.

//' Hurvich, C. M., and Tsai, C.-L. (1989). "Regression and time series model selection
//' in small samples", Biometrika, 76(2): 297–307,
//'
//' Schwarz, Gideon E. (1978). "Estimating the dimension of a model", \emph{Annals of Statistics}, 6 (2): 461–464.
//'
//' @keywords internal
// [[Rcpp::export]]
NumericVector get_vals_lagcrit(List y, List x, int lag_crit, int h, int k, int max_lags,
                                 int n_obs){

  arma::mat xx, xx_one, yy, xpxi, emat, hhat;
  arma::vec w1, beta, resids, resids_sq;
  int rstart_y, rend_y, rend_x;
  double ssr, var_eps, ll, tp, n;
  double pi = 3.141593;
  NumericVector crit_val(max_lags);

  n          = n_obs;

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


    tp       = xx.n_cols + 1;    // Number of parameters to estimate
    xpxi     = inv(xx.t()*xx);
    beta     = xpxi*xx.t()*yy;
    resids   = yy - xx*beta;
    resids_sq = resids%resids;
    ssr      = sum(resids_sq);
    var_eps  = ssr/n;

    // Estimate log-likelihood
    ll       =  - n/2*log(2*pi) - n/2*log(var_eps) - ssr/(2*var_eps);

    if (lag_crit == 1){

      crit_val[i] = (-2*ll + 2*tp) + (2*pow(tp,2) + 2*tp)/(n-tp-1);

         } else if(lag_crit == 2) {

      crit_val[i] = -2*ll + 2*tp ;

          } else if(lag_crit == 3){

      crit_val[i] = -2*ll + log(n)*tp;
    }

  }

  return(crit_val);

}
