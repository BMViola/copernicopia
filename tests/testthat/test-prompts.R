test_that("number lists are parsed", {
  expect_equal(copernicopia:::.parse_numbers("1, 3 5-7"), c(1L, 3L, 5L, 6L, 7L))
  expect_equal(copernicopia:::.parse_numbers("4"), 4L)
  expect_null(copernicopia:::.parse_numbers("nrt"))
  expect_null(copernicopia:::.parse_numbers(""))
})

test_that("prompts re-ask until the answer is valid", {
  old <- options(copernicopia.input = script_input(c("abc", "9", "2")))
  on.exit(options(old))
  out <- capture.output(n <- copernicopia:::.ask_numbers("Pick: ", 3, multiple = FALSE))
  expect_equal(n, 2L)
  old2 <- options(copernicopia.input = script_input(c("", "maybe", "n")))
  on.exit(options(old2), add = TRUE)
  capture.output(yn1 <- copernicopia:::.ask_yes_no("OK?", TRUE))
  capture.output(yn2 <- copernicopia:::.ask_yes_no("OK?", TRUE))
  expect_true(yn1)
  expect_false(yn2)
})

test_that("step labels read naturally", {
  expect_equal(copernicopia:::.step_label(c(1, 3, 24, 24 * 8, 24 * 30.4, 24 * 365, NA)),
               c("hourly", "3-hourly", "daily", "8-day", "monthly", "yearly", "unknown step"))
})
