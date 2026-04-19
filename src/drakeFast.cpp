#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

namespace {

inline bool is_valid_code(int code, int nlevels) {
  return code != NA_INTEGER && code > 0 && code <= nlevels;
}

inline bool is_finite_weight(double value) {
  return !NumericVector::is_na(value) && R_finite(value);
}

void clamp_weights_inplace(NumericVector weights, double max_weight, double min_weight) {
  int n = weights.size();
  for (int i = 0; i < n; ++i) {
    double value = weights[i];
    if (!is_finite_weight(value)) {
      continue;
    }
    if (value > max_weight) {
      weights[i] = max_weight;
    } else if (value < min_weight) {
      weights[i] = min_weight;
    }
  }
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

NumericVector weighted_basis_density(NumericVector weights, NumericMatrix basis) {
  int n = basis.nrow();
  int m = basis.ncol();

  if (weights.size() != n) {
    stop("weights and basis dimensions do not match");
  }

  NumericVector out(m);
  for (int j = 0; j < m; ++j) {
    double sum = 0.0;
    const double* col = &basis(0, j);
    for (int i = 0; i < n; ++i) {
      double weight = weights[i];
      if (is_finite_weight(weight)) {
        sum += weight * col[i];
      }
    }
    out[j] = sum;
  }

  return out;
}

} // namespace

// [[Rcpp::export]]
NumericMatrix CBuildGaussianBasis(NumericVector x, NumericVector xout, double bw) {
  int n = x.size();
  int m = xout.size();

  if (!R_finite(bw) || bw <= 0.0) {
    stop("bw must be a positive finite value");
  }

  NumericMatrix basis(n, m);
  double inv_two_bw_sq = 0.5 / (bw * bw);

  for (int j = 0; j < m; ++j) {
    double target = xout[j];
    double* col = &basis(0, j);
    for (int i = 0; i < n; ++i) {
      double diff = x[i] - target;
      col[i] = std::exp(-(diff * diff) * inv_two_bw_sq);
    }
  }

  return basis;
}

// [[Rcpp::export]]
NumericVector CWeightByContinuousBasis(NumericVector weights,
                                       NumericMatrix basis,
                                       IntegerVector match_index,
                                       NumericVector target_y) {
  int n = weights.size();
  int m = basis.ncol();

  if (basis.nrow() != n) {
    stop("weights and basis dimensions do not match");
  }
  if (match_index.size() != n) {
    stop("weights and match_index lengths do not match");
  }
  if (target_y.size() != m) {
    stop("target_y and basis dimensions do not match");
  }

  NumericVector sample_y = weighted_basis_density(weights, basis);
  double total = std::accumulate(sample_y.begin(), sample_y.end(), 0.0);

  if (!R_finite(total) || total <= 0.0) {
    return clone(weights);
  }

  NumericVector ratios(m);
  for (int j = 0; j < m; ++j) {
    ratios[j] = (sample_y[j] > 0.0) ? (target_y[j] * total / sample_y[j]) : 1.0;
  }

  NumericVector out = clone(weights);
  for (int i = 0; i < n; ++i) {
    int idx = match_index[i];
    if (is_valid_code(idx, m) && is_finite_weight(out[i])) {
      out[i] *= ratios[idx - 1];
    }
  }

  return out;
}

// [[Rcpp::export]]
double CContinuousBasisDiff(NumericVector weights,
                            NumericMatrix basis,
                            NumericVector target_y) {
  int n = weights.size();
  int m = basis.ncol();

  if (basis.nrow() != n) {
    stop("weights and basis dimensions do not match");
  }
  if (target_y.size() != m) {
    stop("target_y and basis dimensions do not match");
  }

  NumericVector sample_y = weighted_basis_density(weights, basis);
  double total = std::accumulate(sample_y.begin(), sample_y.end(), 0.0);

  if (!R_finite(total) || total <= 0.0) {
    return NA_REAL;
  }

  double diff_sum = 0.0;
  for (int j = 0; j < m; ++j) {
    diff_sum += std::fabs(target_y[j] - (sample_y[j] / total));
  }

  return diff_sum;
}

// [[Rcpp::export]]
NumericVector CWeightByDiscreteMany(List codes_list,
                                    NumericVector weights,
                                    List targets_list,
                                    bool cap_every_var = false,
                                    double max_weight = R_PosInf,
                                    double min_weight = 0.0) {
  int n_targets = codes_list.size();
  NumericVector out = clone(weights);

  if (targets_list.size() != n_targets) {
    stop("codes_list and targets_list lengths do not match");
  }

  for (int k = 0; k < n_targets; ++k) {
    IntegerVector codes = as<IntegerVector>(codes_list[k]);
    NumericVector targets = as<NumericVector>(targets_list[k]);
    int nlevels = targets.size();
    NumericVector sums = weighted_level_sums(codes, out, nlevels);
    double total = std::accumulate(sums.begin(), sums.end(), 0.0);

    if (total > 0.0) {
      NumericVector ratios(nlevels);
      for (int level = 0; level < nlevels; ++level) {
        double observed = sums[level] / total;
        ratios[level] = targets[level] / observed;
      }

      int n = codes.size();
      for (int i = 0; i < n; ++i) {
        int code = codes[i];
        if (is_valid_code(code, nlevels) && is_finite_weight(out[i])) {
          out[i] *= ratios[code - 1];
        }
      }
    }

    if (cap_every_var) {
      clamp_weights_inplace(out, max_weight, min_weight);
    }
  }

  return out;
}

// [[Rcpp::export]]
double CMaxAbsDiscreteDiffMany(List codes_list,
                               NumericVector weights,
                               List targets_list) {
  int n_targets = codes_list.size();
  double max_diff = 0.0;
  bool saw_valid = false;

  if (targets_list.size() != n_targets) {
    stop("codes_list and targets_list lengths do not match");
  }

  for (int k = 0; k < n_targets; ++k) {
    IntegerVector codes = as<IntegerVector>(codes_list[k]);
    NumericVector targets = as<NumericVector>(targets_list[k]);
    int nlevels = targets.size();
    NumericVector sums = weighted_level_sums(codes, weights, nlevels);
    double total = std::accumulate(sums.begin(), sums.end(), 0.0);

    if (total <= 0.0) {
      continue;
    }

    for (int level = 0; level < nlevels; ++level) {
      double observed = sums[level] / total;
      double diff = std::fabs(observed - targets[level]);
      if (diff > max_diff) {
        max_diff = diff;
      }
    }
    saw_valid = true;
  }

  if (!saw_valid) {
    return NA_REAL;
  }

  return max_diff;
}

// [[Rcpp::export]]
NumericVector CWeightByDiscreteSubsetMany(List target_code_list,
                                          List strata_code_list,
                                          NumericVector weights,
                                          List targets_by_strata_list,
                                          bool cap_every_var = false,
                                          double max_weight = R_PosInf,
                                          double min_weight = 0.0) {
  int n_targets = target_code_list.size();
  NumericVector out = clone(weights);

  if (strata_code_list.size() != n_targets || targets_by_strata_list.size() != n_targets) {
    stop("subset discrete inputs must have matching lengths");
  }

  for (int k = 0; k < n_targets; ++k) {
    IntegerVector target_codes = as<IntegerVector>(target_code_list[k]);
    IntegerVector strata_codes = as<IntegerVector>(strata_code_list[k]);
    NumericMatrix targets_by_strata = as<NumericMatrix>(targets_by_strata_list[k]);
    int n_target = targets_by_strata.nrow();
    int n_strata = targets_by_strata.ncol();
    int n = out.size();

    NumericMatrix cell_sums(n_target, n_strata);
    NumericVector strata_totals(n_strata);

    for (int i = 0; i < n; ++i) {
      int target_code = target_codes[i];
      int strata_code = strata_codes[i];

      if (is_valid_code(target_code, n_target) &&
          is_valid_code(strata_code, n_strata) &&
          is_finite_weight(out[i])) {
        cell_sums(target_code - 1, strata_code - 1) += out[i];
        strata_totals[strata_code - 1] += out[i];
      }
    }

    NumericMatrix ratios(n_target, n_strata);
    for (int strata = 0; strata < n_strata; ++strata) {
      double strata_total = strata_totals[strata];
      if (strata_total <= 0.0) {
        continue;
      }

      for (int target = 0; target < n_target; ++target) {
        double observed = cell_sums(target, strata) / strata_total;
        ratios(target, strata) = targets_by_strata(target, strata) / observed;
      }
    }

    for (int i = 0; i < n; ++i) {
      int target_code = target_codes[i];
      int strata_code = strata_codes[i];

      if (is_valid_code(target_code, n_target) &&
          is_valid_code(strata_code, n_strata) &&
          is_finite_weight(out[i])) {
        out[i] *= ratios(target_code - 1, strata_code - 1);
      }
    }

    if (cap_every_var) {
      clamp_weights_inplace(out, max_weight, min_weight);
    }
  }

  return out;
}

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
