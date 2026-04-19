#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
double HiFunction (double k, NumericVector hivw, NumericVector lovw, 
                   NumericVector hidiff, NumericVector lodiff, 
                   NumericVector loweight, NumericVector hiweight, 
                   double meantarget, int hilength, int lolength){
  double vwasum = 0.0;
  double vwbsum = 0.0;
  double n_a = 0.0;
  double n_b = 0.0;

  for(int i = 0; i < lolength; ++i) {
    double kpow = pow(k, lodiff[i]);
    vwasum += lovw[i] * kpow;
    n_a += loweight[i] * kpow;
  }
  for(int i = 0; i < hilength; ++i) {
    double kpow = pow(k, hidiff[i]);
    vwbsum += hivw[i] / kpow;
    n_b += hiweight[i] / kpow;
  }

  double out = (vwasum + vwbsum) / (n_a + n_b) - meantarget;
  return out;
}


// [[Rcpp::export]]
double LoFunction (double k, NumericVector hivw, NumericVector lovw, 
                   NumericVector hidiff, NumericVector lodiff, 
                   NumericVector loweight, NumericVector hiweight, 
                   double meantarget, int hilength, int lolength){
  double vwasum = 0.0;
  double vwbsum = 0.0;
  double n_a = 0.0;
  double n_b = 0.0;

  for(int i = 0; i < lolength; ++i) {
    double kpow = pow(k, lodiff[i]);
    vwasum += lovw[i] / kpow;
    n_a += loweight[i] / kpow;
  }
  for(int i = 0; i < hilength; ++i) {
    double kpow = pow(k, hidiff[i]);
    vwbsum += hivw[i] * kpow;
    n_b += hiweight[i] * kpow;
  }

  double out = (vwasum + vwbsum) / (n_a + n_b) - meantarget;
  return out;
}


// [[Rcpp::export]]
double LoZero ( double a, double b, double t, 
                NumericVector hivw, NumericVector lovw, 
                NumericVector hidiff, NumericVector lodiff, 
                NumericVector loweight, NumericVector hiweight, 
                double meantarget, int hilength, int lolength) {
  double c;
  double d;
  double e;
  double fa;
  double fb;
  double fc;
  double m;
  double macheps;
  double p;
  double q;
  double r;
  double s;
  double sa;
  double sb;
  double tol;
  //
  //  Make local copies of A and B.
  //
  sa = a;
  sb = b;
  fa = LoFunction( sa , hivw, lovw, 
                   hidiff,  lodiff, 
                   loweight, hiweight, 
                   meantarget, hilength, lolength);
  
  
  fb = LoFunction( sb , hivw, lovw, 
                   hidiff,  lodiff, 
                   loweight, hiweight, 
                   meantarget, hilength, lolength);
  
  c = sa;
  fc = fa;
  e = sb - sa;
  d = e;
  
  macheps = 2.220446049250313E-016;
  
  for ( ; ; )
  {
    if ( fabs ( fc ) < fabs ( fb ) )
    {
      sa = sb;
      sb = c;
      c = sa;
      fa = fb;
      fb = fc;
      fc = fa;
    }
    
    tol = 2.0 * macheps * fabs ( sb ) + t;
    m = 0.5 * ( c - sb );
    
    if ( fabs ( m ) <= tol || fb == 0.0 )
    {
      break;
    }
    
    if ( fabs ( e ) < tol || fabs ( fa ) <= fabs ( fb ) )
    {
      e = m;
      d = e;
    }
    else
    {
      s = fb / fa;
      
      if ( sa == c )
      {
        p = 2.0 * m * s;
        q = 1.0 - s;
      }
      else
      {
        q = fa / fc;
        r = fb / fc;
        p = s * ( 2.0 * m * q * ( q - r ) - ( sb - sa ) * ( r - 1.0 ) );
        q = ( q - 1.0 ) * ( r - 1.0 ) * ( s - 1.0 );
      }
      
      if ( 0.0 < p )
      {
        q = - q;
      }
      else
      {
        p = - p;
      }
      
      s = e;
      e = d;
      
      if ( 2.0 * p < 3.0 * m * q - fabs ( tol * q ) &&
           p < fabs ( 0.5 * s * q ) )
      {
        d = p / q;
      }
      else
      {
        e = m;
        d = e;
      }
    }
    sa = sb;
    fa = fb;
    
    if ( tol < fabs ( d ) )
    {
      sb = sb + d;
    }
    else if ( 0.0 < m )
    {
      sb = sb + tol;
    }
    else
    {
      sb = sb - tol;
    }
    
    fb = LoFunction( sb , hivw, lovw, 
                     hidiff,  lodiff, 
                     loweight, hiweight, 
                     meantarget, hilength, lolength);
    
    if ( ( 0.0 < fb && 0.0 < fc ) || ( fb <= 0.0 && fc <= 0.0 ) )
    {
      c = sa;
      fc = fa;
      e = sb - sa;
      d = e;
    }
  }
  return sb;
}

// [[Rcpp::export]]
double HiZero ( double a, double b, double t, 
                NumericVector hivw, NumericVector lovw, 
                NumericVector hidiff, NumericVector lodiff, 
                NumericVector loweight, NumericVector hiweight, 
                double meantarget, int hilength, int lolength) {
  double c;
  double d;
  double e;
  double fa;
  double fb;
  double fc;
  double m;
  double macheps;
  double p;
  double q;
  double r;
  double s;
  double sa;
  double sb;
  double tol;
  //
  //  Make local copies of A and B.
  //
  sa = a;
  sb = b;
  fa = HiFunction( sa , hivw, lovw, 
                   hidiff,  lodiff, 
                   loweight, hiweight, 
                   meantarget, hilength, lolength);
  
  
  fb = HiFunction( sb , hivw, lovw, 
                   hidiff,  lodiff, 
                   loweight, hiweight, 
                   meantarget, hilength, lolength);
  
  c = sa;
  fc = fa;
  e = sb - sa;
  d = e;
  
  macheps = 2.220446049250313E-016;
  
  for ( ; ; )
  {
    if ( fabs ( fc ) < fabs ( fb ) )
    {
      sa = sb;
      sb = c;
      c = sa;
      fa = fb;
      fb = fc;
      fc = fa;
    }
    
    tol = 2.0 * macheps * fabs ( sb ) + t;
    m = 0.5 * ( c - sb );
    
    if ( fabs ( m ) <= tol || fb == 0.0 )
    {
      break;
    }
    
    if ( fabs ( e ) < tol || fabs ( fa ) <= fabs ( fb ) )
    {
      e = m;
      d = e;
    }
    else
    {
      s = fb / fa;
      
      if ( sa == c )
      {
        p = 2.0 * m * s;
        q = 1.0 - s;
      }
      else
      {
        q = fa / fc;
        r = fb / fc;
        p = s * ( 2.0 * m * q * ( q - r ) - ( sb - sa ) * ( r - 1.0 ) );
        q = ( q - 1.0 ) * ( r - 1.0 ) * ( s - 1.0 );
      }
      
      if ( 0.0 < p )
      {
        q = - q;
      }
      else
      {
        p = - p;
      }
      
      s = e;
      e = d;
      
      if ( 2.0 * p < 3.0 * m * q - fabs ( tol * q ) &&
           p < fabs ( 0.5 * s * q ) )
      {
        d = p / q;
      }
      else
      {
        e = m;
        d = e;
      }
    }
    sa = sb;
    fa = fb;
    
    if ( tol < fabs ( d ) )
    {
      sb = sb + d;
    }
    else if ( 0.0 < m )
    {
      sb = sb + tol;
    }
    else
    {
      sb = sb - tol;
    }
    
    fb = HiFunction( sb , hivw, lovw, 
                     hidiff,  lodiff, 
                     loweight, hiweight, 
                     meantarget, hilength, lolength);
    
    if ( ( 0.0 < fb && 0.0 < fc ) || ( fb <= 0.0 && fc <= 0.0 ) )
    {
      c = sa;
      fc = fa;
      e = sb - sa;
      d = e;
    }
  }
  return sb;
}


double MeanFunctionDirect(double k,
                          NumericVector weight,
                          NumericVector var,
                          double meantarget,
                          bool raise_mean) {
  double logk = std::log(k);
  double numerator = 0.0;
  double denominator = 0.0;
  int n = var.size();

  for(int i = 0; i < n; ++i) {
    double diff = std::fabs(meantarget - var[i]) + 1.0;
    double factor = std::exp(logk * diff);
    bool below_target = var[i] < meantarget;
    double adjusted_weight;

    if(below_target == raise_mean) {
      adjusted_weight = weight[i] / factor;
    } else {
      adjusted_weight = weight[i] * factor;
    }

    numerator += adjusted_weight * var[i];
    denominator += adjusted_weight;
  }

  return (numerator / denominator) - meantarget;
}

double MeanZeroDirect(double a,
                      double b,
                      double t,
                      NumericVector weight,
                      NumericVector var,
                      double meantarget,
                      bool raise_mean) {
  double c;
  double d;
  double e;
  double fa;
  double fb;
  double fc;
  double m;
  double macheps;
  double p;
  double q;
  double r;
  double s;
  double sa;
  double sb;
  double tol;

  sa = a;
  sb = b;
  fa = MeanFunctionDirect(sa, weight, var, meantarget, raise_mean);
  fb = MeanFunctionDirect(sb, weight, var, meantarget, raise_mean);

  c = sa;
  fc = fa;
  e = sb - sa;
  d = e;
  macheps = 2.220446049250313E-016;

  for( ; ; ) {
    if (std::fabs(fc) < std::fabs(fb)) {
      sa = sb;
      sb = c;
      c = sa;
      fa = fb;
      fb = fc;
      fc = fa;
    }

    tol = 2.0 * macheps * std::fabs(sb) + t;
    m = 0.5 * (c - sb);

    if (std::fabs(m) <= tol || fb == 0.0) {
      break;
    }

    if (std::fabs(e) < tol || std::fabs(fa) <= std::fabs(fb)) {
      e = m;
      d = e;
    } else {
      s = fb / fa;

      if (sa == c) {
        p = 2.0 * m * s;
        q = 1.0 - s;
      } else {
        q = fa / fc;
        r = fb / fc;
        p = s * (2.0 * m * q * (q - r) - (sb - sa) * (r - 1.0));
        q = (q - 1.0) * (r - 1.0) * (s - 1.0);
      }

      if (0.0 < p) {
        q = -q;
      } else {
        p = -p;
      }

      s = e;
      e = d;

      if (2.0 * p < 3.0 * m * q - std::fabs(tol * q) &&
          p < std::fabs(0.5 * s * q)) {
        d = p / q;
      } else {
        e = m;
        d = e;
      }
    }

    sa = sb;
    fa = fb;

    if (tol < std::fabs(d)) {
      sb = sb + d;
    } else if (0.0 < m) {
      sb = sb + tol;
    } else {
      sb = sb - tol;
    }

    fb = MeanFunctionDirect(sb, weight, var, meantarget, raise_mean);

    if ((0.0 < fb && 0.0 < fc) || (fb <= 0.0 && fc <= 0.0)) {
      c = sa;
      fc = fa;
      e = sb - sa;
      d = e;
    }
  }

  return sb;
}



// [[Rcpp::export]]
NumericVector CWeightByMeanLinear(NumericVector weight, NumericVector var, double meantarget) {
  int n = var.size();
  double numerator = 0.0;
  double denominator = 0.0;

  for(int i = 0; i < n; ++i) {
    numerator += var[i] * weight[i];
    denominator += weight[i];
  }

  double currentmean = numerator / denominator;
  if(currentmean == meantarget) {
    return weight;
  }

  bool raise_mean = currentmean < meantarget;
  double k = MeanZeroDirect(1.0, 20.0, 1.490116e-08, weight, var, meantarget, raise_mean);
  double logk = std::log(k);

  for(int i = 0; i < n; ++i) {
    double diff = std::fabs(meantarget - var[i]) + 1.0;
    double factor = std::exp(logk * diff);
    bool below_target = var[i] < meantarget;

    if(below_target == raise_mean) {
      weight[i] = weight[i] / factor;
    } else {
      weight[i] = weight[i] * factor;
    }
  }

  return weight;
}



// [[Rcpp::export]]
bool anyIsNA(NumericVector x) {
  
  for(int i = 0; i < x.length(); ++i) {
    if(NumericVector::is_na(x[i])) {
      return true;  
    }
  }
  return false;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
*/
