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
  
  NumericVector kpowdifflo(lolength);
  for(int i = 0; i < lolength; ++i) {
    kpowdifflo[i] = pow(k, (lodiff[i]));  
  }
  NumericVector kpowdiffhi(hilength);
  for(int i = 0; i < hilength; ++i) {
    kpowdiffhi[i] = pow(k, (hidiff[i]));  
  }
  
  double vwasum = sum(lovw * kpowdifflo );
  double vwbsum = sum(hivw / kpowdiffhi );
  double n_a = sum(loweight * kpowdifflo );
  double n_b = sum(hiweight / kpowdiffhi );
  
  double out = (vwasum + vwbsum) / (n_a + n_b) - meantarget;
  return out;
}


// [[Rcpp::export]]
double LoFunction (double k, NumericVector hivw, NumericVector lovw, 
                   NumericVector hidiff, NumericVector lodiff, 
                   NumericVector loweight, NumericVector hiweight, 
                   double meantarget, int hilength, int lolength){
  
  NumericVector kpowdifflo(lolength);
  for(int i = 0; i < lolength; ++i) {
    kpowdifflo[i] = pow(k, (lodiff[i]));  
  }
  NumericVector kpowdiffhi(hilength);
  for(int i = 0; i < hilength; ++i) {
    kpowdiffhi[i] = pow(k, (hidiff[i]));  
  }
  
  double vwasum = sum(lovw / kpowdifflo );
  double vwbsum = sum(hivw * kpowdiffhi );
  double n_a = sum(loweight / kpowdifflo );
  double n_b = sum(hiweight * kpowdiffhi );
  
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



// [[Rcpp::export]]
NumericVector CWeightByMeanLinear(NumericVector weight, NumericVector var, double meantarget) {
  int size = var.length();
  NumericVector vw = var * weight;
  double vwsum = sum(vw);
  double wtsum = sum(weight);
  double currentmean = vwsum / wtsum;
  NumericVector diff(size);
  LogicalVector hilo(size);
  
  // diff = log(abs(meantarget - var) + 1);
  diff = abs(meantarget - var);
  hilo = var < meantarget;
  
  // this is an optimization that would work in R too:
  NumericVector lovw = vw[hilo];
  NumericVector hivw= vw[!hilo];
  NumericVector lodiff = diff[hilo];
  lodiff = lodiff + 1;
  NumericVector hidiff= diff[!hilo];
  hidiff = hidiff + 1;
  NumericVector loweight = weight[hilo];
  NumericVector hiweight = weight[!hilo];
  int lolength = lodiff.size();
  int hilength = hidiff.size();
  double k;
  NumericVector kpowdifflo(lolength);
  NumericVector kpowdiffhi(hilength);
  NumericVector newloweight(lolength);
  NumericVector newhiweight(hilength);
  
  
  if(currentmean<meantarget) {
    k= LoZero(1, 20, 1.490116e-08, 
                     hivw, lovw, 
                     hidiff,  lodiff, 
                     loweight, hiweight, 
                     meantarget, hilength, lolength);
    
    for(int i = 0; i < lolength; ++i) {
      kpowdifflo[i] = pow(k, (lodiff[i]));  
      newloweight[i] = loweight[i] / kpowdifflo[i];
    }
    //newloweight  = loweight / kpowdifflo;
    
    for(int i = 0; i < hilength; ++i) {
      kpowdiffhi[i] = pow(k, (hidiff[i]));  
      newhiweight[i] = hiweight[i] * kpowdiffhi[i];
    }
    
    weight[hilo] = newloweight;
    weight[!hilo] = newhiweight;
  } else {
    k= HiZero(1, 20, 1.490116e-08, 
                     hivw, lovw, 
                     hidiff,  lodiff, 
                     loweight, hiweight, 
                     meantarget, hilength, lolength);
    for(int i = 0; i < lolength; ++i) {
      kpowdifflo[i] = pow(k, (lodiff[i]));  
      newloweight[i] = loweight[i] * kpowdifflo[i];
    }
    //newloweight  = loweight * kpowdifflo;
    
    for(int i = 0; i < hilength; ++i) {
      kpowdiffhi[i] = pow(k, (hidiff[i]));  
      newhiweight[i] = hiweight[i] / kpowdiffhi[i];
    }
    
    
    newhiweight = hiweight / kpowdiffhi;
    
    
    weight[hilo] = newloweight;
    weight[!hilo] = newhiweight;
  }
  
  return  weight;
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
