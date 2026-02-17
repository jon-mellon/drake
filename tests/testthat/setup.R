# Load local R sources when not running in an installed package context.
setup_file <- sys.frame(1)$ofile
root_dir <- if (!is.null(setup_file)) {
  normalizePath(file.path(dirname(setup_file), "..", ".."))
} else {
  normalizePath(file.path(getwd(), "..", ".."))
}

if (!"drake" %in% loadedNamespaces()) {
  r_files <- list.files(file.path(root_dir, "R"), pattern = "\\.R$", full.names = TRUE)
  for (file in r_files) {
    sys.source(file, envir = .GlobalEnv)
  }
} else {
  ns <- asNamespace("drake")
  for (name in ls(ns, all.names = TRUE)) {
    if (!exists(name, envir = .GlobalEnv, inherits = FALSE)) {
      assign(name, get(name, ns), envir = .GlobalEnv)
    }
  }
}

# Load compiled code if a local shared library exists.
dynlib <- file.path(root_dir, "src", paste0("drake", .Platform$dynlib.ext))
if (file.exists(dynlib)) {
  try(dyn.load(dynlib), silent = TRUE)
}
