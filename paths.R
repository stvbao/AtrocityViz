viz_script_root <- function() {
  candidates <- c(".", "..", "../..")
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "paths.R")) &&
        dir.exists(file.path(candidate, "scripts")) &&
        dir.exists(file.path(candidate, "shiny_apps"))) {
      return(normalizePath(candidate, mustWork = FALSE))
    }
  }
  normalizePath(getwd(), mustWork = FALSE)
}

viz_project_root <- function() {
  override <- Sys.getenv("ATROCITYVIZ_PROJECT_ROOT", unset = "")
  if (nzchar(override)) {
    return(normalizePath(override, mustWork = FALSE))
  }

  candidates <- c(
    getwd(),
    ".",
    "..",
    "../..",
    "/Users/stvbao/Coding/AtrocityNet"
  )

  for (candidate in candidates) {
    has_data <- dir.exists(file.path(candidate, "01_raw"))
    has_outputs <- dir.exists(file.path(candidate, "02_tidy")) ||
      dir.exists(file.path(candidate, "04_results"))
    if (has_data || has_outputs) {
      return(normalizePath(candidate, mustWork = FALSE))
    }
  }

  normalizePath(getwd(), mustWork = FALSE)
}

viz_root <- viz_script_root()
viz_project <- viz_project_root()

viz_data_file <- function(filename) {
  override <- Sys.getenv("ATROCITYVIZ_DATA", unset = "")
  if (nzchar(override)) {
    return(override)
  }
  file.path(viz_project, "01_raw", filename)
}

viz_results_file <- function(filename) {
  out_dir <- file.path(viz_project, "04_results", "AtrocityViz")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(out_dir, filename)
}

viz_tidy_file <- function(filename) {
  out_dir <- file.path(viz_project, "02_tidy", "AtrocityViz")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(out_dir, filename)
}
