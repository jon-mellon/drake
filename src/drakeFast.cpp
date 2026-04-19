#include <Rcpp.h>

using namespace Rcpp;

namespace {

inline bool is_valid_code(int code, int nlevels) {
  return code != NA_INTEGER && code > 0 && code <= nlevels;
}

NumericVector weighted_level_sums(IntegerVector codes, NumericVector weights, int nlevels) {
  NumericVector sums(nlevels);
  int n = codes.size();

  for (int i = 0; i < n; ++i) {
    int code = codes[i];
    if (is_valid_code(code, nlevels) && !NumericVector::is_na(weights[i])) {
      sums[code - 1] += weights[i];
    }
  }

  return sums;
}

} // namespace

// [[Rcpp::export]]
NumericVector CWeightByDiscreteCodes(IntegerVector codes, NumericVector weights, NumericVector targets) {
  int nlevels = targets.size();
  NumericVector sums = weighted_level_sums(codes, weights, nlevels);
  double total = std::accumulate(sums.begin(), sums.end(), 0.0);

  if (total <= 0) {
    return clone(weights);
  }

  NumericVector ratios(nlevels);
  for (int level = 0; level < nlevels; ++level) {
    double observed = sums[level] / total;
    ratios[level] = targets[level] / observed;
  }

  NumericVector out = clone(weights);
  int n = codes.size();
  for (int i = 0; i < n; ++i) {
    int code = codes[i];
    if (is_valid_code(code, nlevels) && !NumericVector::is_na(out[i])) {
      out[i] *= ratios[code - 1];
    }
  }

  return out;
}

// [[Rcpp::export]]
double CMaxAbsDiscreteDiff(IntegerVector codes, NumericVector weights, NumericVector targets) {
  int nlevels = targets.size();
  NumericVector sums = weighted_level_sums(codes, weights, nlevels);
  double total = std::accumulate(sums.begin(), sums.end(), 0.0);

  if (total <= 0) {
    return NA_REAL;
  }

  double max_diff = 0.0;
  for (int level = 0; level < nlevels; ++level) {
    double observed = sums[level] / total;
    double diff = std::fabs(observed - targets[level]);
    if (diff > max_diff) {
      max_diff = diff;
    }
  }

  return max_diff;
}

// [[Rcpp::export]]
NumericVector CWeightByDiscreteSubsetCodes(IntegerVector target_codes,
                                           IntegerVector strata_codes,
                                           NumericVector weights,
                                           NumericMatrix targets_by_strata) {
  int n_target = targets_by_strata.nrow();
  int n_strata = targets_by_strata.ncol();
  int n = weights.size();

  NumericMatrix cell_sums(n_target, n_strata);
  NumericVector strata_totals(n_strata);

  for (int i = 0; i < n; ++i) {
    int target_code = target_codes[i];
    int strata_code = strata_codes[i];

    if (is_valid_code(target_code, n_target) &&
        is_valid_code(strata_code, n_strata) &&
        !NumericVector::is_na(weights[i])) {
      cell_sums(target_code - 1, strata_code - 1) += weights[i];
      strata_totals[strata_code - 1] += weights[i];
    }
  }

  NumericMatrix ratios(n_target, n_strata);
  for (int strata = 0; strata < n_strata; ++strata) {
    double strata_total = strata_totals[strata];
    if (strata_total <= 0) {
      continue;
    }

    for (int target = 0; target < n_target; ++target) {
      double observed = cell_sums(target, strata) / strata_total;
      ratios(target, strata) = targets_by_strata(target, strata) / observed;
    }
  }

  NumericVector out = clone(weights);
  for (int i = 0; i < n; ++i) {
    int target_code = target_codes[i];
    int strata_code = strata_codes[i];

    if (is_valid_code(target_code, n_target) &&
        is_valid_code(strata_code, n_strata) &&
        !NumericVector::is_na(out[i])) {
      out[i] *= ratios(target_code - 1, strata_code - 1);
    }
  }

  return out;
}

// [[Rcpp::export]]
double CMaxAbsDiscreteSubsetDiff(IntegerVector target_codes,
                                 IntegerVector strata_codes,
                                 NumericVector weights,
                                 NumericMatrix targets_by_strata) {
  int n_target = targets_by_strata.nrow();
  int n_strata = targets_by_strata.ncol();
  int n = weights.size();

  NumericMatrix cell_sums(n_target, n_strata);
  NumericVector strata_totals(n_strata);

  for (int i = 0; i < n; ++i) {
    int target_code = target_codes[i];
    int strata_code = strata_codes[i];

    if (is_valid_code(target_code, n_target) &&
        is_valid_code(strata_code, n_strata) &&
        !NumericVector::is_na(weights[i])) {
      cell_sums(target_code - 1, strata_code - 1) += weights[i];
      strata_totals[strata_code - 1] += weights[i];
    }
  }

  double max_diff = 0.0;
  bool saw_valid = false;

  for (int strata = 0; strata < n_strata; ++strata) {
    double strata_total = strata_totals[strata];
    if (strata_total <= 0) {
      continue;
    }

    for (int target = 0; target < n_target; ++target) {
      double observed = cell_sums(target, strata) / strata_total;
      double diff = std::fabs(observed - targets_by_strata(target, strata));
      if (diff > max_diff) {
        max_diff = diff;
      }
      saw_valid = true;
    }
  }

  if (!saw_valid) {
    return NA_REAL;
  }

  return max_diff;
}
