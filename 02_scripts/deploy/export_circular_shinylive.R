args <- commandArgs(trailingOnly = TRUE)
site_dir <- if (length(args) >= 1) args[[1]] else "_site"
app_dir <- file.path("02_scripts", "deploy", "shiny_circular_conflict")

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop(
    "The 'shinylive' package is required. Install it with ",
    "install.packages('shinylive').",
    call. = FALSE
  )
}

if (!dir.exists(app_dir)) {
  stop("App directory not found: ", app_dir, call. = FALSE)
}

if (dir.exists(site_dir)) {
  unlink(site_dir, recursive = TRUE, force = TRUE)
}

core_wasm_packages <- shinylive:::resolve_dependencies(
  c("shiny", "bslib", "renv"),
  local = FALSE
)

Sys.setenv(
  SHINYLIVE_DOWNLOAD_WASM_CORE_PACKAGES = paste(core_wasm_packages, collapse = ",")
)

shinylive::export(app_dir, site_dir, wasm_packages = TRUE)
message("Shinylive site exported to ", normalizePath(site_dir, mustWork = FALSE))
