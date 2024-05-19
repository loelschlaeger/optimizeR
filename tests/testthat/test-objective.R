test_that("objective with one target argument can be evaluated", {
  f <- function(x, a, b = 0) (x + a)^2 + b
  objective <- Objective$new(f = f, target = "x", npar = 1, a = -2)
  objective$verbose <- FALSE
  expect_equal(
    objective$get_argument("b"),
    0
  )
  expect_equal(
    objective$evaluate(2),
    0
  )
  expect_silent(
    objective$remove_argument("a")
  )
  expect_error(
    objective$evaluate(2),
    "Function evaluation threw an error"
  )
  expect_silent(
    objective$set_argument("a" = -2)
  )
  expect_error(
    objective$set_argument("a" = -2, overwrite = FALSE),
    "already exists"
  )
  expect_silent(
    objective$set_argument("a" = -2, overwrite = TRUE)
  )
  expect_equal(
    objective$evaluate(2),
    0
  )
  expect_snapshot(
    print(objective)
  )
  expect_equal(
    objective$fixed_arguments,
    c("b", "a")
  )
})

test_that("objective with more than one target argument can be evaluated", {
  llk <- function(mu, sd, lambda, data) {
    sd <- exp(sd)
    lambda <- plogis(lambda)
    sum(log(lambda * dnorm(data, mu[1], sd[1]) + (1 - lambda) * dnorm(data, mu[2], sd[2])))
  }
  objective <- Objective$new(
    f = llk, target = c("mu", "sd", "lambda"), npar = c(2, 2, 1),
    data = faithful$eruptions
  )
  objective$verbose <- FALSE
  expect_equal(
    objective$get_argument("data"),
    faithful$eruptions
  )
  expect_snapshot(
    print(objective)
  )
  expect_equal(
    objective$evaluate(1:5),
    llk(mu = 1:2, sd = 3:4, lambda = 5, data = faithful$eruptions)
  )
  expect_equal(
    objective$evaluate(1:5, .negate = TRUE),
    -llk(mu = 1:2, sd = 3:4, lambda = 5, data = faithful$eruptions)
  )
})

test_that("objective with NULL argument can be evaluated", {
  f <- function(x, a, b, ind) {
    if (is.null(ind)) {
      (x[1]^2 + x[2] + a)^2 + (x[1] + x[2]^2 + b)^2 + (x[3] - 1)^2
    }
  }
  obj <- Objective$new(
    f = f,
    target = "x",
    npar = 3
  )
  obj$verbose <- FALSE
  checkmate::expect_number(
    obj$evaluate(.at = c(0, 0, 0), a = -11, b = -7, ind = NULL)
  )
  expect_null(
    obj$evaluate(.at = c(0, 0, 0), a = -11, b = -7, ind = TRUE)
  )
  obj$set_argument("a" = -11, "b" = -7, "ind" = NULL)
  checkmate::expect_number(
    obj$evaluate(.at = c(0, 0, 0))
  )
})
