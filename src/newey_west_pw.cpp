#include <RcppArmadillo.h>
using namespace Rcpp;
//' @name newey_west_pw
//' @title Compute Newey-West estimator with prewhitened estimation functions
//' @description  Compute Newey-West estimator with prewhitened estimation functions.
//' The function is based on the Matlab code by James P. LeSage.
//' @param hhat_mat Matrix.
//' @param xpxi_mat Matrix.
//' @param D_mat Matrix.
//' @param h integer.
//' @return A list. The first element contains the pre-whitened Newey West covariance matrix.
//' @keywords internal
//' @references
//'
//' Andrews, D.W. and Monahan, J.C. (1992). An improved heteroskedasticity and
//' autocorrelation consistent covariance matrix estimator. \emph{Econometrica}, pp.953-966.
//'
//' Newey, W.K., and West, K.D. (1987). “A Simple, Positive-Definite, Heteroskedasticity and
//' Autocorrelation Consistent Covariance Matrix.” \emph{Econometrica}, 55, 703–708.
//'
//'
// [[Rcpp::export]]
List newey_west_pw(NumericMatrix hhat_mat, NumericMatrix xpxi_mat, NumericMatrix D_mat, int h){

  // Define classes
  NumericMatrix V;
  arma::mat G, M, M1, M2, ga, g1, za, hhat, D, xpxi, utu;
  arma::vec w1;
  int nrow_hhat, a, nobs, num_exog, nlag;
  double w;
  List ret(1);


  hhat      = as<arma::mat>(hhat_mat);
  xpxi      = as<arma::mat>(xpxi_mat);
  D         = as<arma::mat>(D_mat);
  num_exog  = hhat.n_cols;
  nobs      = hhat.n_rows;
  hhat      = hhat.t();

  nlag     = h;
  G        = arma::zeros<arma::mat>(num_exog, num_exog);
  a        = 0;


  // Loop to weight meat matrix
  for (int i = 0; i < nlag + 1; ++i){

      ga                 = arma::zeros<arma::mat>(num_exog, num_exog);
      w                  = 1 - i/double(nlag + 1);
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

    G  = G +  w*ga;

    a = a + 1;


  }


  G = D*G*D.t();

  V = wrap(xpxi*G*xpxi);

  ret[0]  = V;
  return (ret);

}

