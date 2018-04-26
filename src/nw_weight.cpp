#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector nw_weight(NumericMatrix hhat, int nobs, int num_exog, int nlag) {
  NumericMatrix za;
  arma::mat A, B, C, D, M, M1, M2;
  arma::vec w1;
  NumericMatrix G(num_exog, num_exog);
  int nrow_hhat, a, p;
  NumericVector w(2*nlag + 1);
  double L1;

  a = 0;

  for (int i = 0; i < nlag; ++i){

    NumericMatrix ga(num_exog, num_exog);
    w[nlag + a]        = (nlag+1-a)/double(nlag + 1);
    M                  = as<arma::mat>(hhat);
    nrow_hhat          = M.n_rows;
    M1                 = M(arma::span(0, nrow_hhat - 1), arma::span(a, nobs - 1));
    M2                 = M(arma::span(0, nrow_hhat - 1), arma::span(0, nobs - 1 - a));
    M2                 = M2.t();
    za                 = wrap(M1*M2) ;

    if (a == 0){

      A        = as<arma::mat>(ga);
      B        = as<arma::mat>(za);

      ga  = wrap(A + B);

    } else {

      C        = as<arma::mat>(ga);
      D        = as<arma::mat>(za);

      ga  = wrap(C +  D + D.t());

    }

    arma::vec w1 = as<arma::vec>(w);
    arma::mat G1 = as<arma::mat>(G);
    arma::mat g1 = as<arma::mat>(ga);

    G  = wrap(G1 +  w1(nlag + a , 0)*g1);

    a = a + 1;


  }

  return(G);

}
