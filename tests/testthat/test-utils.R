test_that("check for number works", {
  expect_false(is_number(0))
  expect_true(is_number(1))
  expect_false(is_number(0.5))
  expect_false(is_number("1"))
})

test_that("try an expression silently works", {
  expect_equal(try_silent(1 + 1), 2)
  expect_s3_class(try_silent(1 + "1"), "fail")
})

test_that("interruption of long evaluations works", {
  foo <- function(x) {
    for (i in 1:10) Sys.sleep(x / 10)
    return(x)
  }
  expect_null(timed(foo(1.5), 1))
})

test_that("interruption of silently tried evaluations works", {
  foo <- function(x, y) {
    for (i in 1:10) Sys.sleep(x / 10)
    return(x + y)
  }
  expect_equal(try_silent_timed(foo(0.5, 1), 1), 1.5)
  expect_s3_class(try_silent_timed(foo(0.5, "1"), 1), "fail")
  expect_null(try_silent_timed(foo(1.5, 1), 1))
})

test_that("measure computation time works", {
  what <- function(s) {
    Sys.sleep(s)
    return(s)
  }
  args <- list(s = 1)
  out <- do.call_timed(what = what, args = args)
  expect_type(out, "list")
  expect_length(out, 2)
  expect_equal(out[["res"]], 1)
  expect_type(out[["time"]], "double")
  expect_true(abs(out[["time"]] - 2) < 1)
})
