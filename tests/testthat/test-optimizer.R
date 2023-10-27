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
    c("value", "parameter", "seconds", "initial", "gradient", "code", "iterations")
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
    c("value", "parameter", "seconds", "initial", "gradient", "code", "iterations")
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
    c("value", "parameter", "seconds", "initial", "gradient", "code", "iterations")
  )
})







