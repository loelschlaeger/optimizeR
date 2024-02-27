test_that("simple minimization works", {
  objective <- function(x) x^2
  out <- Optimizer$new("stats::nlm")$
    minimize(objective, initial = 2)
  expect_type(
    out,
    "list"
  )
  expect_named(
    out,
    c("value", "parameter", "seconds", "initial", "error", "gradient", "code", "iterations")
  )
})

test_that("simple maximization works", {
  objective <- function(x) -x^2
  out <- Optimizer$new("stats::nlm")$
    maximize(objective, initial = 2)
  expect_type(
    out,
    "list"
  )
  expect_named(
    out,
    c("value", "parameter", "seconds", "initial", "error", "gradient", "code", "iterations")
  )
})

test_that("minimization with additional arguments works", {
  objective <- function(x, a = 1) x^2 + a
  out <- Optimizer$new("stats::nlm")$
    minimize(objective, initial = 2, "a" = 10)
  expect_type(
    out,
    "list"
  )
  expect_named(
    out,
    c("value", "parameter", "seconds", "initial", "error", "gradient", "code", "iterations")
  )
})

test_that("error can be detected", {
  error_optimizer <- define_optimizer(
    .optimizer = function(f, p, ...) {
      if (identical(p, 1:2)) stop("error message")
      list(v = f(p), z = 1:2)
    },
    .objective = "f", .initial = "p", .value = "v",
    .parameter = "z", .direction = "min"
  )
  out <- error_optimizer$
    minimize(objective = TestFunctions::TF_ackley, initial = 1:2)
  expect_true(out$error)
  expect_equal(out$error_message, "error message")
})

test_that("exceed of time limit can be detected", {
  skip_if_not(.Platform$OS.type == "windows")
  slow_optimizer <- define_optimizer(
    .optimizer = function(f, p, ...) {
      Sys.sleep(2)
      stats::nlm(f = f, p = p)
    },
    .objective = "f", .initial = "p", .value = "v",
    .parameter = "z", .direction = "min"
  )
  slow_optimizer$seconds <- 1
  out <- slow_optimizer$
    minimize(objective = TestFunctions::TF_ackley, initial = 1:2)
  expect_true(out$error)
  expect_equal(out$error_message, "time limit exceeded")
})

test_that("fixed argument that is NULL can be passed", {
  f <- function(x, a, b, ind) {
    if (is.null(ind)) {
      (x[1]^2 + x[2] + a)^2 + (x[1] + x[2]^2 + b)^2 + (x[3] - 1)^2
    }
  }
  expect_false(
    optimizer_nlm()$minimize(
      objective = f,
      initial = c(0, 0, 0),
      a = -11, b = -7, ind = NULL
    )$error
  )
  expect_false(
    Optimizer$new("stats::nlm")$minimize(
      objective = f,
      initial = c(0, 0, 0),
      a = -11, b = -7, ind = NULL
    )$error
  )
})
