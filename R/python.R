.cmems <- new.env(parent = emptyenv())

.has_py_require <- function() {
  exists("py_require", envir = asNamespace("reticulate"), inherits = FALSE)
}

.onLoad <- function(libname, pkgname) {
  # With reticulate >= 1.41, declare the Python requirement; reticulate then
  # provisions an environment with it automatically on first use.
  if (.has_py_require()) {
    try(get("py_require", envir = asNamespace("reticulate"))("copernicusmarine>=2"),
        silent = TRUE)
  }
  .cmems$cm <- reticulate::import("copernicusmarine", delay_load = TRUE)
  .cmems$helpers <- reticulate::import_from_path(
    "copernicopia_helpers",
    path = system.file("python", package = pkgname),
    delay_load = TRUE
  )
}

#' Install the Python backend
#'
#' Installs the `copernicusmarine` Python toolbox (version 2 or later) into a
#' dedicated Python environment. You only need this with reticulate older than
#' 1.41; newer versions set the environment up automatically the first time a
#' copernicopia function needs Python.
#'
#' If you already have a conda environment with `copernicusmarine`, you can use
#' it instead by running `reticulate::use_condaenv("cmems")` at the start of
#' your session, before calling any copernicopia function.
#'
#' @param envname Name of the environment to create or update.
#' @param method Passed to [reticulate::py_install()].
#' @param ... Further arguments passed to [reticulate::py_install()].
#' @return `NULL`, invisibly.
#' @export
cmems_install_python <- function(envname = "r-copernicopia", method = "auto", ...) {
  reticulate::py_install("copernicusmarine>=2", envname = envname,
                         method = method, pip = TRUE, ...)
  message("Installed. Restart R, then load copernicopia again.")
  invisible(NULL)
}

.ensure_python <- function() {
  if (isTRUE(.cmems$ready)) return(invisible(TRUE))
  if (!reticulate::py_available(initialize = FALSE) &&
      !.has_py_require() &&
      !nzchar(Sys.getenv("RETICULATE_PYTHON")) &&
      reticulate::virtualenv_exists("r-copernicopia")) {
    reticulate::use_virtualenv("r-copernicopia", required = TRUE)
  }
  if (!reticulate::py_module_available("copernicusmarine")) {
    stop("The Python package 'copernicusmarine' was not found.\n",
         "Run copernicopia::cmems_install_python(), restart R, and try again. Or, to use an ",
         "existing conda environment, run reticulate::use_condaenv(\"cmems\") ",
         "at the start of a fresh R session.", call. = FALSE)
  }
  version <- as.character(.cmems$cm$`__version__`)
  if (utils::compareVersion(version, "2.0.0") < 0) {
    stop("copernicusmarine ", version, " found; version 2 or later is needed. ",
         "Run copernicopia::cmems_install_python() to update it.", call. = FALSE)
  }
  .cmems$toolbox_version <- version
  .cmems$ready <- TRUE
  invisible(TRUE)
}

.cm <- function() {
  .ensure_python()
  .cmems$cm
}

.helpers <- function() {
  .ensure_python()
  .cmems$helpers
}

#' Log in to Copernicus Marine
#'
#' Checks for saved credentials and, if there are none (or they are invalid),
#' asks for your Copernicus Marine username and password. Credentials are
#' saved by the `copernicusmarine` toolbox, so you only need to log in once
#' per computer. Register for a free account at
#' <https://data.marine.copernicus.eu/register>.
#'
#' @param username,password Your Copernicus Marine credentials. Leave as
#'   `NULL` to be prompted (the password is entered without being shown).
#' @param ask If `TRUE`, prompt for missing credentials.
#' @return `TRUE`, invisibly, when logged in.
#' @export
cmems_login <- function(username = NULL, password = NULL, ask = interactive()) {
  cm <- .cm()
  restore <- .quiet_toolbox()
  on.exit(restore(), add = TRUE)
  if (is.null(username) && is.null(password)) {
    if (isTRUE(cm$login(check_credentials_valid = TRUE))) {
      message("Logged in to Copernicus Marine (saved credentials).")
      return(invisible(TRUE))
    }
    if (!ask) {
      stop("Not logged in. Run cmems_login() interactively, or ",
           "cmems_login(username, password).", call. = FALSE)
    }
    message("No valid Copernicus Marine credentials found. ",
            "Register for free at https://data.marine.copernicus.eu/register")
  }
  if (is.null(username)) username <- .ask_text("Copernicus Marine username: ")
  if (is.null(password)) password <- .ask_password("Copernicus Marine password: ")
  ok <- cm$login(username = username, password = password, force_overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop("Login failed. Check your username and password.", call. = FALSE)
  }
  message("Logged in. Your credentials are saved for future sessions.")
  invisible(TRUE)
}
