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
#' dedicated Python environment, `r-copernicopia`. You need this with
#' reticulate older than 1.41. Newer versions of reticulate set an environment
#' up automatically the first time a copernicopia function needs Python, so
#' there you only need this if that automatic setup does not work on your
#' computer.
#'
#' Once the `r-copernicopia` environment exists, copernicopia uses it, with
#' any version of reticulate, unless you have chosen another Python yourself
#' (with `reticulate::use_condaenv()`, `use_virtualenv()` or `use_python()`,
#' or the `RETICULATE_PYTHON` environment variable). To go back to
#' reticulate's automatic environment, remove it with
#' `reticulate::virtualenv_remove("r-copernicopia")`.
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

# Whether the user has already said which Python to use: with an environment
# variable, or with reticulate::use_python(), use_virtualenv() or
# use_condaenv() (which reticulate records in its own settings).
.python_already_chosen <- function() {
  if (nzchar(Sys.getenv("RETICULATE_PYTHON")) || nzchar(Sys.getenv("RETICULATE_PYTHON_ENV"))) {
    return(TRUE)
  }
  chosen <- tryCatch(
    get(".globals", envir = asNamespace("reticulate"))$required_python_version,
    error = function(e) NULL
  )
  !is.null(chosen)
}

.ensure_python <- function() {
  if (isTRUE(.cmems$ready)) return(invisible(TRUE))
  # The environment made by cmems_install_python() is used whenever it exists
  # (with any version of reticulate), unless Python is already running or the
  # user has chosen another one
  if (!reticulate::py_available(initialize = FALSE) &&
      !.python_already_chosen() &&
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
      .cmems$logged_in <- TRUE
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
  .cmems$logged_in <- TRUE
  invisible(TRUE)
}

# interactive(), as a function of the package's own so the tests can stand in
# for it (they may themselves be run in an interactive session)
.interactive <- function() interactive()

# Called before anything is downloaded or checked. Without saved credentials
# the toolbox would ask for a username itself, in the middle of a progress
# line or, in the separate download process, not at all: every file would then
# fail without saying why. So the credentials are checked first, once per
# session: in an interactive session the user is asked to log in, otherwise
# the download stops with a clear message.
#
# If the check itself cannot be made (for example the login server cannot be
# reached), the download goes ahead and reports whatever happens.
# options(copernicopia.check_login = FALSE) skips the check.
.require_login <- function() {
  if (!isTRUE(getOption("copernicopia.check_login", TRUE))) return(invisible(TRUE))
  if (isTRUE(.cmems$logged_in)) return(invisible(TRUE))
  cm <- .cm()
  restore <- .quiet_toolbox()
  on.exit(restore(), add = TRUE)
  ok <- tryCatch(isTRUE(cm$login(check_credentials_valid = TRUE)), error = function(e) NA)
  if (is.na(ok)) return(invisible(FALSE))
  if (ok) {
    .cmems$logged_in <- TRUE
    return(invisible(TRUE))
  }
  if (!.interactive() && !is.function(getOption("copernicopia.input"))) {
    stop("You are not logged in to Copernicus Marine, so nothing can be downloaded. ",
         "Run cmems_login() in an interactive R session, or cmems_login(username, password), ",
         "then try again.", call. = FALSE)
  }
  cmems_login(ask = TRUE)
}
