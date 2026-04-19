# Usage examples:
# Rscript testing/run-drake-benchmarks.R
# Rscript testing/run-drake-benchmarks.R --profile=quick --cases=discrete_only,mixed_targets
# Rscript testing/run-drake-benchmarks.R --repetitions=7 --warmup=2 --save=/tmp/drake-bench.rds
# Rscript testing/run-drake-benchmarks.R --baseline=/tmp/drake-bench.rds

parseBenchmarkOption <- function(args, name, default = NULL) {
  prefix <- paste0("--", name, "=")
  matches <- grep(paste0("^", prefix), args, value = TRUE)

  if(length(matches) == 0L) {
    return(default)
  }

  sub(prefix, "", matches[[length(matches)]])
}

parseBenchmarkFlag <- function(args, name, default = FALSE) {
  flag <- paste0("--", name)

  if(flag %in% args) {
    return(TRUE)
  }

  default
}

resolveBenchmarkPath <- function(path, root_dir) {
  if(is.null(path) || identical(path, "")) {
    return(NULL)
  }

  if(grepl("^/", path)) {
    return(path)
  }

  file.path(root_dir, path)
}

formatBenchmarkComparison <- function(x) {
  out <- x
  numeric_cols <- vapply(out, is.numeric, logical(1))
  out[numeric_cols] <- lapply(out[numeric_cols], function(col) round(col, 6))
  out
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if(length(script_arg) == 0L) {
  stop("Unable to determine the script path.")
}

script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))
args <- commandArgs(trailingOnly = TRUE)

r_files <- list.files(file.path(root_dir, "R"), pattern = "\\.R$", full.names = TRUE)
for(file in r_files) {
  sys.source(file, envir = .GlobalEnv)
}

dynlib <- file.path(root_dir, "src", paste0("drake", .Platform$dynlib.ext))
if(file.exists(dynlib)) {
  try(dyn.load(dynlib), silent = TRUE)
}

profile <- parseBenchmarkOption(args, "profile", default = "standard")
repetitions <- as.integer(parseBenchmarkOption(args, "repetitions", default = "5"))
warmup <- as.integer(parseBenchmarkOption(args, "warmup", default = "1"))
gc_first <- !parseBenchmarkFlag(args, "no-gc", default = FALSE)
cases_arg <- parseBenchmarkOption(args, "cases", default = NULL)
save_path <- resolveBenchmarkPath(parseBenchmarkOption(args, "save", default = NULL), root_dir)
baseline_path <- resolveBenchmarkPath(parseBenchmarkOption(args, "baseline", default = NULL), root_dir)

cases <- drakeBenchmarkCases(profile = profile)
case_names <- if(is.null(cases_arg)) {
  names(cases)
} else {
  trimws(strsplit(cases_arg, ",", fixed = TRUE)[[1]])
}

suite <- runDrakeBenchmarkSuite(
  cases = cases,
  case.names = case_names,
  repetitions = repetitions,
  warmup = warmup,
  gc.first = gc_first
)

print(suite)

if(!is.null(save_path)) {
  dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
  saveDrakeBenchmarkSuite(suite, save_path)
  cat("\nSaved benchmark suite to:", save_path, "\n")
}

if(!is.null(baseline_path)) {
  baseline <- loadDrakeBenchmarkSuite(baseline_path)
  comparison <- compareDrakeBenchmarkSuites(suite, baseline)
  comparison <- formatBenchmarkComparison(comparison)

  cat("\nComparison against baseline:\n")
  print(comparison, row.names = FALSE)

  if(any(comparison$correctness_regression)) {
    quit(status = 1L)
  }
}
