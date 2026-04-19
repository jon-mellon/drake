withDrakeBenchmarkSeed <- function(seed, expr) {
  expr <- substitute(expr)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  if(had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  on.exit({
    if(had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  eval(expr, envir = parent.frame())
}

benchmarkScaleN <- function(n, scale, min_n = 128L) {
  as.integer(max(min_n, round(n * scale)))
}

ensureLevelCoverage <- function(x, levels) {
  x <- as.character(x)
  missing_levels <- setdiff(levels, unique(x))

  if(length(missing_levels) > 0L) {
    replace_idx <- seq_len(length(missing_levels))
    x[replace_idx] <- missing_levels
  }

  factor(x, levels = levels)
}

ensureStratifiedLevelCoverage <- function(data, target_var, strata_var, target_levels) {
  for(strata_level in levels(data[[strata_var]])) {
    idx <- which(data[[strata_var]] == strata_level)
    present <- unique(as.character(data[[target_var]][idx]))
    missing_levels <- setdiff(target_levels, present)

    if(length(missing_levels) > 0L) {
      replace_idx <- idx[seq_len(length(missing_levels))]
      data[[target_var]][replace_idx] <- missing_levels
    }
  }

  data[[target_var]] <- factor(data[[target_var]], levels = target_levels)
  data
}

makeSyntheticSurveyData <- function(n, seed) {
  region_levels <- c("North", "South", "East", "West")
  gender_levels <- c("F", "M")
  education_levels <- c("Low", "Mid", "High")
  turnout_levels <- c("Voted", "DNV")
  party_levels <- c("A", "B", "C")
  age_band_levels <- c("18-29", "30-44", "45-64", "65+")

  withDrakeBenchmarkSeed(seed, {
    region <- sample(region_levels, n, replace = TRUE,
                     prob = c(0.24, 0.31, 0.19, 0.26))
    region_effect <- c(North = 0.25, South = -0.20, East = 0.35, West = -0.10)[region]

    age <- pmin(pmax(stats::rnorm(n, mean = 45 + 8 * region_effect, sd = 13), 18), 85)

    gender_prob <- stats::plogis(0.15 - 0.25 * region_effect + 0.02 * (age - 45))
    gender <- ifelse(stats::rbinom(n, 1, gender_prob) == 1, "F", "M")

    education_score <- region_effect + (age - 45) / 15 + stats::rnorm(n, sd = 0.9)
    education <- cut(
      education_score,
      breaks = c(-Inf, -0.35, 0.75, Inf),
      labels = education_levels
    )

    turnout_prob <- stats::plogis(
      -0.8 +
        0.04 * (age - 45) +
        0.35 * (education == "High") +
        0.10 * (gender == "F") -
        0.25 * (region == "South")
    )
    turnout <- ifelse(stats::rbinom(n, 1, turnout_prob) == 1, "Voted", "DNV")

    ideology <- pmin(
      pmax(
        4 +
          0.45 * (region == "South") -
          0.35 * (education == "High") +
          0.15 * (gender == "M") +
          stats::rnorm(n, sd = 1.05),
        1
      ),
      7
    )

    household_size <- pmax(
      1L,
      pmin(
        6L,
        stats::rpois(
          n,
          lambda = 2.2 +
            0.35 * (region == "South") +
            0.20 * (education == "Low")
        ) + 1L
      )
    )

    party_signal <- 0.55 * (region == "South") -
      0.25 * (education == "High") +
      0.15 * (turnout == "Voted") +
      0.10 * (gender == "F") +
      stats::rnorm(n, sd = 0.9)
    party <- cut(
      party_signal,
      breaks = c(-Inf, -0.25, 0.55, Inf),
      labels = party_levels
    )

    age_band <- cut(
      age,
      breaks = c(18, 30, 45, 65, Inf),
      right = FALSE,
      labels = age_band_levels
    )

    data <- data.frame(
      age = age,
      gender = ensureLevelCoverage(gender, gender_levels),
      region = ensureLevelCoverage(region, region_levels),
      education = ensureLevelCoverage(education, education_levels),
      turnout = ensureLevelCoverage(turnout, turnout_levels),
      ideology = as.numeric(ideology),
      household_size = as.integer(household_size),
      party = ensureLevelCoverage(party, party_levels),
      age_band = ensureLevelCoverage(age_band, age_band_levels)
    )

    data
  })
}

makeNamedProportions <- function(x, levels = NULL) {
  if(is.null(levels)) {
    levels <- sort(unique(as.character(x)))
  }

  x <- factor(x, levels = levels)
  out <- prop.table(table(x))
  out <- as.numeric(out)
  names(out) <- levels
  out
}

makeDensityTarget <- function(x, n = 128L) {
  stats::density(x, n = n, from = min(x), to = max(x))
}

makeStratifiedDensityTarget <- function(data, value_var, strata_var, n = 128L) {
  strata_levels <- levels(data[[strata_var]])
  targets <- setNames(vector("list", length(strata_levels)), strata_levels)

  for(strata_level in strata_levels) {
    targets[[strata_level]] <- makeDensityTarget(
      data[data[[strata_var]] == strata_level, value_var],
      n = n
    )
  }

  out <- list(targets)
  names(out) <- strata_var
  out
}

makeSubsetTargets <- function(data, target_var, strata_var) {
  target_levels <- levels(data[[target_var]])
  strata_levels <- levels(data[[strata_var]])
  subset_targets <- setNames(vector("list", length(strata_levels)), strata_levels)

  for(strata_level in strata_levels) {
    subset_targets[[strata_level]] <- makeNamedProportions(
      data[data[[strata_var]] == strata_level, target_var],
      levels = target_levels
    )
  }

  by_strata <- list(subset_targets)
  names(by_strata) <- strata_var

  out <- list(by_strata)
  names(out) <- target_var
  out
}

newDrakeBenchmarkCase <- function(name, description, tags, args, runner = "drake") {
  if(identical(runner, "drake") && is.null(args$.cache.key)) {
    args$.cache.key <- paste0("benchmark:", name)
  }

  case <- list(
    name = name,
    description = description,
    tags = unique(as.character(tags)),
    runner = runner,
    args = args
  )
  class(case) <- "drakeBenchmarkCase"
  case
}

drakeBenchmarkCases <- function(profile = c("standard", "quick"), seed = 1L) {
  profile <- match.arg(profile)
  clearDrakePreparedCache()
  scale <- if(profile == "quick") 0.2 else 1
  density_n <- if(profile == "quick") 64L else 128L

  sample_n_discrete <- benchmarkScaleN(4000L, scale, min_n = 800L)
  pop_n_discrete <- benchmarkScaleN(18000L, scale, min_n = 3200L)
  discrete_sample <- makeSyntheticSurveyData(sample_n_discrete, seed + 1L)
  discrete_pop <- makeSyntheticSurveyData(pop_n_discrete, seed + 101L)

  sample_n_cont <- benchmarkScaleN(3200L, scale, min_n = 700L)
  pop_n_cont <- benchmarkScaleN(14000L, scale, min_n = 2800L)
  continuous_sample <- makeSyntheticSurveyData(sample_n_cont, seed + 2L)
  continuous_pop <- makeSyntheticSurveyData(pop_n_cont, seed + 102L)

  sample_n_mean <- benchmarkScaleN(2800L, scale, min_n = 600L)
  pop_n_mean <- benchmarkScaleN(12000L, scale, min_n = 2400L)
  mean_sample <- makeSyntheticSurveyData(sample_n_mean, seed + 3L)
  mean_pop <- makeSyntheticSurveyData(pop_n_mean, seed + 103L)

  sample_n_subset <- benchmarkScaleN(3600L, scale, min_n = 800L)
  pop_n_subset <- benchmarkScaleN(15000L, scale, min_n = 3000L)
  subset_sample <- makeSyntheticSurveyData(sample_n_subset, seed + 4L)
  subset_pop <- makeSyntheticSurveyData(pop_n_subset, seed + 104L)
  subset_sample <- ensureStratifiedLevelCoverage(subset_sample, "gender", "region", levels(subset_sample$gender))
  subset_pop <- ensureStratifiedLevelCoverage(subset_pop, "gender", "region", levels(subset_pop$gender))

  sample_n_mixed <- benchmarkScaleN(3800L, scale, min_n = 900L)
  pop_n_mixed <- benchmarkScaleN(18000L, scale, min_n = 3600L)
  mixed_sample <- makeSyntheticSurveyData(sample_n_mixed, seed + 5L)
  mixed_pop <- makeSyntheticSurveyData(pop_n_mixed, seed + 105L)

  sample_n_strat <- benchmarkScaleN(3600L, scale, min_n = 800L)
  pop_n_strat <- benchmarkScaleN(16000L, scale, min_n = 3200L)
  strat_sample <- makeSyntheticSurveyData(sample_n_strat, seed + 6L)
  strat_pop <- makeSyntheticSurveyData(pop_n_strat, seed + 106L)

  sample_n_select <- benchmarkScaleN(3600L, scale, min_n = 800L)
  pop_n_select <- benchmarkScaleN(16000L, scale, min_n = 3200L)
  selection_sample <- makeSyntheticSurveyData(sample_n_select, seed + 7L)
  selection_pop <- makeSyntheticSurveyData(pop_n_select, seed + 107L)

  cases <- list(
    discrete_only = newDrakeBenchmarkCase(
      name = "discrete_only",
      description = "Discrete margins only across multiple categorical targets.",
      tags = c("discrete"),
      args = list(
        sample = discrete_sample,
        continuous.targets = NULL,
        discrete.targets = list(
          gender = makeNamedProportions(discrete_pop$gender, levels(discrete_sample$gender)),
          region = makeNamedProportions(discrete_pop$region, levels(discrete_sample$region)),
          education = makeNamedProportions(discrete_pop$education, levels(discrete_sample$education)),
          turnout = makeNamedProportions(discrete_pop$turnout, levels(discrete_sample$turnout)),
          age_band = makeNamedProportions(discrete_pop$age_band, levels(discrete_sample$age_band))
        ),
        maxit = 80,
        check.convergence.every = 5,
        max.weights = 30,
        min.weights = 1 / 30
      )
    ),
    continuous_only = newDrakeBenchmarkCase(
      name = "continuous_only",
      description = "Single continuous density target without categorical margins.",
      tags = c("continuous"),
      args = list(
        sample = continuous_sample,
        continuous.targets = list(
          age = makeDensityTarget(continuous_pop$age, n = density_n)
        ),
        discrete.targets = list(),
        maxit = 70,
        check.convergence.every = 4,
        max.weights = 25,
        min.weights = 1 / 25
      )
    ),
    mean_only = newDrakeBenchmarkCase(
      name = "mean_only",
      description = "Mean target only to exercise the root-finding weight adjustment path.",
      tags = c("mean"),
      args = list(
        sample = mean_sample,
        continuous.targets = NULL,
        discrete.targets = list(),
        mean.targets = list(ideology = mean(mean_pop$ideology)),
        maxit = 50,
        check.convergence.every = 5,
        max.weights = 20,
        min.weights = 1 / 20
      )
    ),
    discrete_subset = newDrakeBenchmarkCase(
      name = "discrete_subset",
      description = "Marginal targets plus subset targets within region strata.",
      tags = c("discrete", "subset"),
      args = list(
        sample = subset_sample,
        continuous.targets = NULL,
        discrete.targets = list(
          region = makeNamedProportions(subset_pop$region, levels(subset_sample$region)),
          turnout = makeNamedProportions(subset_pop$turnout, levels(subset_sample$turnout))
        ),
        discrete.target.subset = makeSubsetTargets(subset_pop, "gender", "region"),
        maxit = 80,
        check.convergence.every = 5,
        max.weights = 30,
        min.weights = 1 / 30
      )
    ),
    mixed_targets = newDrakeBenchmarkCase(
      name = "mixed_targets",
      description = "Combined continuous, discrete, and mean targets.",
      tags = c("continuous", "discrete", "mean"),
      args = list(
        sample = mixed_sample,
        continuous.targets = list(
          age = makeDensityTarget(mixed_pop$age, n = density_n)
        ),
        discrete.targets = list(
          region = makeNamedProportions(mixed_pop$region, levels(mixed_sample$region)),
          education = makeNamedProportions(mixed_pop$education, levels(mixed_sample$education)),
          turnout = makeNamedProportions(mixed_pop$turnout, levels(mixed_sample$turnout))
        ),
        mean.targets = list(ideology = mean(mixed_pop$ideology)),
        maxit = 90,
        check.convergence.every = 5,
        max.weights = 25,
        min.weights = 1 / 25
      )
    ),
    stratified_continuous = newDrakeBenchmarkCase(
      name = "stratified_continuous",
      description = "Continuous density targets stratified by region, with region margins.",
      tags = c("continuous", "stratified", "discrete"),
      args = list(
        sample = strat_sample,
        continuous.targets = list(
          age = makeStratifiedDensityTarget(strat_pop, "age", "region", n = density_n)
        ),
        discrete.targets = list(
          region = makeNamedProportions(strat_pop$region, levels(strat_sample$region))
        ),
        maxit = 80,
        check.convergence.every = 4,
        max.weights = 25,
        min.weights = 1 / 25
      )
    ),
    selection_weighted = newDrakeBenchmarkCase(
      name = "selection_weighted",
      description = "Discrete targets with selection weights and RR floor enforcement.",
      tags = c("discrete", "selection"),
      args = list(
        sample = selection_sample,
        continuous.targets = NULL,
        discrete.targets = list(
          gender = makeNamedProportions(selection_pop$gender, levels(selection_sample$gender)),
          region = makeNamedProportions(selection_pop$region, levels(selection_sample$region)),
          turnout = makeNamedProportions(selection_pop$turnout, levels(selection_sample$turnout))
        ),
        initial.weights = selection_sample$household_size / mean(selection_sample$household_size),
        RR = 0.60,
        selection.weights = TRUE,
        maxit = 90,
        check.convergence.every = 5,
        max.weights = 25,
        min.weights = 1 / 25
      )
    )
  )

  cases
}

safeMetricMax <- function(x) {
  if(length(x) == 0L || all(is.na(x))) {
    return(0)
  }

  max(x, na.rm = TRUE)
}

collectDrakeBenchmarkWarnings <- function(expr) {
  warnings <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, warnings = unique(warnings))
}

runDrakeBenchmarkInvocation <- function(case) {
  runner <- get(case$runner, mode = "function")
  outcome <- tryCatch(
    collectDrakeBenchmarkWarnings(do.call(runner, case$args)),
    error = function(e) {
      structure(
        list(message = conditionMessage(e)),
        class = "drakeBenchmarkError"
      )
    }
  )

  outcome
}

computeDiscreteTargetDiffs <- function(sample, weights, discrete.targets) {
  if(is.null(discrete.targets) || length(discrete.targets) == 0L) {
    return(numeric(0))
  }

  diffs <- rep(NA_real_, length(discrete.targets))
  names(diffs) <- names(discrete.targets)

  for(var in names(discrete.targets)) {
    target <- discrete.targets[[var]]
    mask <- !is.na(weights) & !is.na(sample[[var]])
    counts <- wttabSlim(
      x = factor(sample[[var]][mask], levels = names(target)),
      weights = weights[mask],
      current.levels = names(target)
    )
    total <- sum(counts)

    if(total > 0) {
      diffs[var] <- max(abs((as.numeric(counts) / total) - as.numeric(target)))
    }
  }

  diffs
}

computeSubsetTargetDiffs <- function(sample, weights, discrete.target.subset) {
  if(is.null(discrete.target.subset) || length(discrete.target.subset) == 0L) {
    return(numeric(0))
  }

  diffs <- numeric(0)

  for(target_var in names(discrete.target.subset)) {
    for(strata_var in names(discrete.target.subset[[target_var]])) {
      for(strata_level in names(discrete.target.subset[[target_var]][[strata_var]])) {
        target <- discrete.target.subset[[target_var]][[strata_var]][[strata_level]]
        mask <- !is.na(weights) &
          !is.na(sample[[target_var]]) &
          !is.na(sample[[strata_var]]) &
          sample[[strata_var]] == strata_level

        counts <- wttabSlim(
          x = factor(sample[[target_var]][mask], levels = names(target)),
          weights = weights[mask],
          current.levels = names(target)
        )
        total <- sum(counts)
        label <- paste0(target_var, "[", strata_var, "=", strata_level, "]")

        diffs[label] <- if(total > 0) {
          max(abs((as.numeric(counts) / total) - as.numeric(target)))
        } else {
          NA_real_
        }
      }
    }
  }

  diffs
}

computeContinuousTargetDiffs <- function(sample, weights, continuous.targets) {
  if(is.null(continuous.targets) || length(continuous.targets) == 0L) {
    return(numeric(0))
  }

  data <- sample
  data[, ".benchmark_weights"] <- weights

  diffs <- rep(NA_real_, length(continuous.targets))
  names(diffs) <- names(continuous.targets)

  for(var in names(continuous.targets)) {
    diffs[var] <- tryCatch(
      checkContinuous(data, var, continuous.targets[[var]], weights = ".benchmark_weights"),
      error = function(e) NA_real_
    )
  }

  diffs
}

computeMeanTargetDiffs <- function(sample, weights, mean.targets) {
  if(is.null(mean.targets) || length(mean.targets) == 0L) {
    return(numeric(0))
  }

  diffs <- rep(NA_real_, length(mean.targets))
  names(diffs) <- names(mean.targets)

  for(var in names(mean.targets)) {
    mask <- !is.na(weights) & !is.na(sample[[var]])
    diffs[var] <- if(any(mask)) {
      abs(stats::weighted.mean(sample[[var]][mask], weights[mask]) - mean.targets[[var]])
    } else {
      NA_real_
    }
  }

  diffs
}

computeRepeatWeightDiff <- function(outputs) {
  if(length(outputs) <= 1L) {
    return(0)
  }

  reference <- outputs[[1]]
  diffs <- vapply(outputs[-1], function(current) {
    diff <- abs(current - reference)
    diff <- diff[is.finite(diff)]

    if(length(diff) == 0L) {
      0
    } else {
      max(diff)
    }
  }, numeric(1))

  safeMetricMax(diffs)
}

countSubsetConstraints <- function(discrete.target.subset) {
  if(is.null(discrete.target.subset) || length(discrete.target.subset) == 0L) {
    return(0L)
  }

  total <- 0L
  for(target_var in names(discrete.target.subset)) {
    for(strata_var in names(discrete.target.subset[[target_var]])) {
      total <- total + length(discrete.target.subset[[target_var]][[strata_var]])
    }
  }

  as.integer(total)
}

calculateDrakeBenchmarkMetrics <- function(case, weights, outputs) {
  sample <- case$args$sample
  discrete.targets <- case$args$discrete.targets
  discrete.target.subset <- case$args$discrete.target.subset
  continuous.targets <- case$args$continuous.targets
  mean.targets <- case$args$mean.targets

  valid_weights <- weights[!is.na(weights)]
  finite_weights <- all(is.finite(valid_weights))
  n_valid <- sum(!is.na(weights))
  sum_weights <- sum(weights, na.rm = TRUE)
  sum_error <- abs(sum_weights - n_valid)

  discrete_diffs <- computeDiscreteTargetDiffs(sample, weights, discrete.targets)
  subset_diffs <- computeSubsetTargetDiffs(sample, weights, discrete.target.subset)
  continuous_diffs <- computeContinuousTargetDiffs(sample, weights, continuous.targets)
  mean_diffs <- computeMeanTargetDiffs(sample, weights, mean.targets)

  if(length(valid_weights) > 0L) {
    efficiency_pct <- calc_efficiency(valid_weights)
    weight_cv <- stats::sd(valid_weights) / mean(valid_weights)
  } else {
    efficiency_pct <- NA_real_
    weight_cv <- NA_real_
  }

  near_upper_cap_share <- if(length(valid_weights) > 0L && !is.null(case$args$max.weights)) {
    mean(valid_weights >= (case$args$max.weights * 0.98))
  } else {
    NA_real_
  }

  near_lower_cap_share <- if(length(valid_weights) > 0L && !is.null(case$args$min.weights)) {
    mean(valid_weights <= (case$args$min.weights * 1.02))
  } else {
    NA_real_
  }

  rr_min_ratio_star <- NA_real_
  rr_floor_shortfall <- 0

  if(!is.null(case$args$RR) && isTRUE(case$args$selection.weights)) {
    initial_weights <- case$args$initial.weights
    mask <- !is.na(weights) & !is.na(initial_weights)
    ratio <- weights[mask] / initial_weights[mask]
    ratio_star <- ratio / mean(ratio)
    rr_min_ratio_star <- min(ratio_star)
    rr_floor_shortfall <- max(0, case$args$RR - rr_min_ratio_star)
  }

  summary <- data.frame(
    case_name = case$name,
    description = case$description,
    tags = paste(case$tags, collapse = ","),
    n_rows = nrow(sample),
    n_columns = ncol(sample),
    n_discrete_targets = if(is.null(discrete.targets)) 0L else length(discrete.targets),
    n_subset_constraints = countSubsetConstraints(discrete.target.subset),
    n_continuous_targets = if(is.null(continuous.targets)) 0L else length(continuous.targets),
    n_mean_targets = if(is.null(mean.targets)) 0L else length(mean.targets),
    n_valid = n_valid,
    finite_weights = finite_weights,
    sum_weights = sum_weights,
    sum_error = sum_error,
    min_weight = if(length(valid_weights) > 0L) min(valid_weights) else NA_real_,
    max_weight = if(length(valid_weights) > 0L) max(valid_weights) else NA_real_,
    weight_cv = weight_cv,
    efficiency_pct = efficiency_pct,
    near_upper_cap_share = near_upper_cap_share,
    near_lower_cap_share = near_lower_cap_share,
    max_abs_discrete_diff = safeMetricMax(discrete_diffs),
    max_abs_subset_diff = safeMetricMax(subset_diffs),
    max_continuous_diff = safeMetricMax(continuous_diffs),
    max_abs_mean_diff = safeMetricMax(mean_diffs),
    rr_min_ratio_star = rr_min_ratio_star,
    rr_floor_shortfall = rr_floor_shortfall,
    repeat_max_abs_diff = computeRepeatWeightDiff(outputs),
    stringsAsFactors = FALSE
  )

  list(
    summary = summary,
    details = list(
      discrete = discrete_diffs,
      subset = subset_diffs,
      continuous = continuous_diffs,
      mean = mean_diffs
    )
  )
}

runDrakeBenchmarkCase <- function(case, repetitions = 5L, warmup = 1L, gc.first = TRUE) {
  repetitions <- as.integer(repetitions)
  warmup <- as.integer(warmup)

  if(repetitions <= 0L) {
    stop("repetitions must be at least 1")
  }
  if(warmup < 0L) {
    stop("warmup must be non-negative")
  }

  warning_messages <- character(0)

  if(warmup > 0L) {
    for(ii in seq_len(warmup)) {
      if(gc.first) {
        invisible(gc())
      }

      warmup_outcome <- runDrakeBenchmarkInvocation(case)
      if(inherits(warmup_outcome, "drakeBenchmarkError")) {
        summary <- data.frame(
          case_name = case$name,
          description = case$description,
          tags = paste(case$tags, collapse = ","),
          n_rows = nrow(case$args$sample),
          n_columns = ncol(case$args$sample),
          n_discrete_targets = if(is.null(case$args$discrete.targets)) 0L else length(case$args$discrete.targets),
          n_subset_constraints = countSubsetConstraints(case$args$discrete.target.subset),
          n_continuous_targets = if(is.null(case$args$continuous.targets)) 0L else length(case$args$continuous.targets),
          n_mean_targets = if(is.null(case$args$mean.targets)) 0L else length(case$args$mean.targets),
          n_valid = NA_integer_,
          finite_weights = FALSE,
          sum_weights = NA_real_,
          sum_error = NA_real_,
          min_weight = NA_real_,
          max_weight = NA_real_,
          weight_cv = NA_real_,
          efficiency_pct = NA_real_,
          near_upper_cap_share = NA_real_,
          near_lower_cap_share = NA_real_,
          max_abs_discrete_diff = NA_real_,
          max_abs_subset_diff = NA_real_,
          max_continuous_diff = NA_real_,
          max_abs_mean_diff = NA_real_,
          rr_min_ratio_star = NA_real_,
          rr_floor_shortfall = NA_real_,
          repeat_max_abs_diff = NA_real_,
          repetition_count = repetitions,
          warmup_count = warmup,
          min_time_sec = NA_real_,
          median_time_sec = NA_real_,
          mean_time_sec = NA_real_,
          max_time_sec = NA_real_,
          sd_time_sec = NA_real_,
          warning_count = 0L,
          warning_messages = "",
          status = "error",
          error_message = warmup_outcome$message,
          stringsAsFactors = FALSE
        )

        return(list(
          case = list(name = case$name, description = case$description, tags = case$tags),
          summary = summary,
          timings = data.frame(),
          weights = NULL,
          warnings = character(0),
          details = list()
        ))
      }

      warning_messages <- c(warning_messages, warmup_outcome$warnings)
    }
  }

  timings <- rep(NA_real_, repetitions)
  outputs <- vector("list", repetitions)

  for(ii in seq_len(repetitions)) {
    if(gc.first) {
      invisible(gc())
    }

    start_time <- proc.time()[["elapsed"]]
    outcome <- runDrakeBenchmarkInvocation(case)
    timings[ii] <- proc.time()[["elapsed"]] - start_time

    if(inherits(outcome, "drakeBenchmarkError")) {
      summary <- data.frame(
        case_name = case$name,
        description = case$description,
        tags = paste(case$tags, collapse = ","),
        n_rows = nrow(case$args$sample),
        n_columns = ncol(case$args$sample),
        n_discrete_targets = if(is.null(case$args$discrete.targets)) 0L else length(case$args$discrete.targets),
        n_subset_constraints = countSubsetConstraints(case$args$discrete.target.subset),
        n_continuous_targets = if(is.null(case$args$continuous.targets)) 0L else length(case$args$continuous.targets),
        n_mean_targets = if(is.null(case$args$mean.targets)) 0L else length(case$args$mean.targets),
        n_valid = NA_integer_,
        finite_weights = FALSE,
        sum_weights = NA_real_,
        sum_error = NA_real_,
        min_weight = NA_real_,
        max_weight = NA_real_,
        weight_cv = NA_real_,
        efficiency_pct = NA_real_,
        near_upper_cap_share = NA_real_,
        near_lower_cap_share = NA_real_,
        max_abs_discrete_diff = NA_real_,
        max_abs_subset_diff = NA_real_,
        max_continuous_diff = NA_real_,
        max_abs_mean_diff = NA_real_,
        rr_min_ratio_star = NA_real_,
        rr_floor_shortfall = NA_real_,
        repeat_max_abs_diff = NA_real_,
        repetition_count = repetitions,
        warmup_count = warmup,
        min_time_sec = min(timings, na.rm = TRUE),
        median_time_sec = stats::median(timings, na.rm = TRUE),
        mean_time_sec = mean(timings, na.rm = TRUE),
        max_time_sec = max(timings, na.rm = TRUE),
        sd_time_sec = if(repetitions > 1L) stats::sd(timings, na.rm = TRUE) else 0,
        warning_count = length(unique(warning_messages)),
        warning_messages = paste(unique(warning_messages), collapse = " | "),
        status = "error",
        error_message = outcome$message,
        stringsAsFactors = FALSE
      )

      return(list(
        case = list(name = case$name, description = case$description, tags = case$tags),
        summary = summary,
        timings = data.frame(
          case_name = case$name,
          repetition = seq_len(repetitions),
          elapsed_sec = timings,
          stringsAsFactors = FALSE
        ),
        weights = NULL,
        warnings = unique(warning_messages),
        details = list()
      ))
    }

    outputs[[ii]] <- outcome$value
    warning_messages <- c(warning_messages, outcome$warnings)
  }

  metrics <- calculateDrakeBenchmarkMetrics(case, outputs[[1]], outputs)
  summary <- metrics$summary
  summary$repetition_count <- repetitions
  summary$warmup_count <- warmup
  summary$min_time_sec <- min(timings)
  summary$median_time_sec <- stats::median(timings)
  summary$mean_time_sec <- mean(timings)
  summary$max_time_sec <- max(timings)
  summary$sd_time_sec <- if(repetitions > 1L) stats::sd(timings) else 0
  summary$warning_count <- length(unique(warning_messages))
  summary$warning_messages <- paste(unique(warning_messages), collapse = " | ")
  summary$status <- "success"
  summary$error_message <- ""

  list(
    case = list(name = case$name, description = case$description, tags = case$tags),
    summary = summary,
    timings = data.frame(
      case_name = case$name,
      repetition = seq_len(repetitions),
      elapsed_sec = timings,
      stringsAsFactors = FALSE
    ),
    weights = outputs[[1]],
    warnings = unique(warning_messages),
    details = metrics$details
  )
}

runDrakeBenchmarkSuite <- function(cases = drakeBenchmarkCases(),
                                   case.names = names(cases),
                                   repetitions = 5L,
                                   warmup = 1L,
                                   gc.first = TRUE) {
  if(is.null(case.names)) {
    case.names <- names(cases)
  }

  missing_cases <- setdiff(case.names, names(cases))
  if(length(missing_cases) > 0L) {
    stop("Unknown benchmark cases: ", paste(missing_cases, collapse = ", "))
  }

  selected_cases <- cases[case.names]
  results <- lapply(selected_cases, runDrakeBenchmarkCase,
                    repetitions = repetitions, warmup = warmup, gc.first = gc.first)
  names(results) <- case.names

  summary <- do.call(rbind, lapply(results, function(x) x$summary))
  rownames(summary) <- NULL

  timings <- do.call(rbind, lapply(results, function(x) x$timings))
  if(is.null(timings)) {
    timings <- data.frame()
  } else {
    rownames(timings) <- NULL
  }

  out <- list(
    summary = summary,
    timings = timings,
    results = results
  )
  class(out) <- "drakeBenchmarkSuite"
  out
}

print.drakeBenchmarkSuite <- function(x, ...) {
  display <- x$summary[, c(
    "case_name",
    "tags",
    "n_rows",
    "median_time_sec",
    "max_abs_discrete_diff",
    "max_abs_subset_diff",
    "max_continuous_diff",
    "max_abs_mean_diff",
    "efficiency_pct",
    "status"
  )]

  numeric_cols <- vapply(display, is.numeric, logical(1))
  display[numeric_cols] <- lapply(display[numeric_cols], function(col) round(col, 6))
  print(display, row.names = FALSE)
  invisible(x)
}

saveDrakeBenchmarkSuite <- function(x, path) {
  saveRDS(x, path)
  invisible(path)
}

loadDrakeBenchmarkSuite <- function(path) {
  readRDS(path)
}

compareDrakeBenchmarkSuites <- function(current, baseline,
                                        tolerances = c(
                                          sum_error = 1e-6,
                                          max_abs_discrete_diff = 5e-4,
                                          max_abs_subset_diff = 5e-4,
                                          max_continuous_diff = 1e-2,
                                          max_abs_mean_diff = 1e-3,
                                          rr_floor_shortfall = 1e-8,
                                          repeat_max_abs_diff = 1e-10
                                        )) {
  merged <- merge(
    current$summary,
    baseline$summary,
    by = "case_name",
    suffixes = c(".current", ".baseline"),
    sort = FALSE
  )

  comparison <- data.frame(
    case_name = merged$case_name,
    current_status = merged$status.current,
    baseline_status = merged$status.baseline,
    median_time_ratio = merged$median_time_sec.current / merged$median_time_sec.baseline,
    n_valid_delta = merged$n_valid.current - merged$n_valid.baseline,
    finite_weights_regression = !merged$finite_weights.current & merged$finite_weights.baseline,
    stringsAsFactors = FALSE
  )

  correctness_regression <- comparison$finite_weights_regression |
    (comparison$current_status != comparison$baseline_status) |
    (comparison$n_valid_delta != 0)

  for(metric in names(tolerances)) {
    delta <- merged[[paste0(metric, ".current")]] - merged[[paste0(metric, ".baseline")]]
    comparison[[paste0(metric, "_delta")]] <- delta
    current_missing <- is.na(merged[[paste0(metric, ".current")]])
    baseline_missing <- is.na(merged[[paste0(metric, ".baseline")]])
    both_missing <- current_missing & baseline_missing
    metric_regression <- abs(delta) > tolerances[[metric]]
    metric_regression[both_missing] <- FALSE
    metric_regression[is.na(metric_regression)] <- TRUE
    correctness_regression <- correctness_regression | metric_regression
  }

  comparison$correctness_regression <- correctness_regression
  comparison
}
