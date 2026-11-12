test_that("a Python chosen by the user is recognised", {
  chosen <- copernicopia:::.python_already_chosen
  withr::with_envvar(c(RETICULATE_PYTHON = "/some/python", RETICULATE_PYTHON_ENV = NA),
                     expect_true(chosen()))
  withr::with_envvar(c(RETICULATE_PYTHON = NA, RETICULATE_PYTHON_ENV = "/some/env"),
                     expect_true(chosen()))
  # Otherwise it depends on reticulate::use_python() and friends having been called
  withr::with_envvar(c(RETICULATE_PYTHON = NA, RETICULATE_PYTHON_ENV = NA), {
    expect_type(chosen(), "logical")
    expect_length(chosen(), 1)
  })
})

test_that("the environment made by cmems_install_python() is used with any reticulate", {
  used <- NULL
  local_mocked_bindings(.python_already_chosen = function() FALSE)
  testthat::local_mocked_bindings(
    py_available = function(...) FALSE,
    virtualenv_exists = function(envname, ...) identical(envname, "r-copernicopia"),
    use_virtualenv = function(virtualenv, required = NULL) used <<- c(virtualenv, required),
    py_module_available = function(module) FALSE,
    .package = "reticulate"
  )
  state <- copernicopia:::.cmems
  old <- state$ready
  withr::defer(assign("ready", old, envir = state))
  state$ready <- NULL
  # (py_module_available() is mocked to say no, so this stops after choosing)
  expect_error(copernicopia:::.ensure_python(), "cmems_install_python")
  expect_equal(used, c("r-copernicopia", "TRUE"))

  # Not when the user has chosen a Python themselves
  used <- NULL
  local_mocked_bindings(.python_already_chosen = function() TRUE)
  expect_error(copernicopia:::.ensure_python(), "cmems_install_python")
  expect_null(used)
})
