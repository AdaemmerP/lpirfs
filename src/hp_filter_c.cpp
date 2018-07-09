#include <RcppArmadillo.h>
using namespace Rcpp;
//' @name hp_filter_c
//' @title Detrend a times series via the Hodrick-Prescott filter
//' @description  Estimates cyclical and trend component with HP-filter by Hodrick and Prescott (1997).
//' The function is based on the R-code in the archived package "mFilter" by  Mehmet Balcilar.
//' @param x A something
//' @param lambda A something
//'  Hodrick, R.J., and Prescott, E. C. Prescott (1997) "Postwar U.S. Business Cycles: An Empirical Investigation."
//'  \emph{Journal of Money, Credit and Banking}, 29(1), 1-16.
// [[Rcpp::export]]
List hp_filter_c(NumericVector x, double lambda){

  arma::mat xx, ln, q, sigma_r, g, b_inv, b;
  arma::vec x_cycle, x_trend;
  int    n;
  List ret(2);

  xx        = as<arma::mat>(x);

  n         = xx.n_rows;
  arma::mat imat(n,n);
  imat      = imat.eye();

  arma::mat ln_1(xx.n_rows, 1);
  ln_1      = ln_1.fill(0);
  ln_1      = ln_1.t();

  arma::mat ln_2(n-1, n);
  ln_2      = ln_2.eye();

  ln        = join_cols(ln_1, ln_2);
  ln        = (imat - ln)*(imat - ln);

  q         = ln.rows(2,n-1);
  q         = q.t();

  sigma_r   = q.t()*q;
  arma::mat sigma_n(n-2, n-2);
  sigma_n   = sigma_n.eye();

  g         = q.t()*xx;
  b_inv     = inv(sigma_n + lambda*sigma_r);
  b         = b_inv*g;

  x_cycle   = lambda*q*b;
  x_trend   = xx - x_cycle;


  ret[0]  = wrap(x_cycle) ;
  ret[1]  = wrap(x_trend);
  return (ret);
}
