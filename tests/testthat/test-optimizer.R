test_that("optimizer can be defined", {
  opt <- Optimizer$new(
    which = "custom", .verbose = FALSE
  )


})

test_that("optimizer can be validated", {
  opt <- Optimizer$new(
    which = "custom", .verbose = FALSE
  )


})

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
  skip_if_not(.Platform$OS.type %in% c("unix", "windows"))
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

test_that("parameter bounds can be used", {

  ### objective definition
  himmelblau_modified <- function(x) {
    if (abs(x[1]) > 5 || abs(x[2]) > 5) {
      -100
    } else {
      (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
    }
  }

  ### optimizer definition
  optim_opt <- Optimizer$new("stats::optim", method = "L-BFGS-B")

  ### minimization with bounds
  out <- optim_opt$minimize(
    objective = himmelblau_modified,
    initial = c(-10, -10),
    lower = -5,
    upper = 5
  )
  expect_true(
    all(abs(out$parameter) < 5)
  )

  ### minimization without bounds
  out <- optim_opt$minimize(
    objective = himmelblau_modified,
    initial = c(-10, -10)
  )
  expect_equal(out$value, -100)

})

test_that("gradient and hessian can be used", {

  ### objective definition with gradient and Hessian
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  himmelblau_gradient <- function(x) {
    #warning("gradient is used")
    c(
      4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
      2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
    )
  }
  himmelblau_hessian <- function(x) {
    #warning("hessian is used")
    matrix(
      c(
        12 * x[1]^2 + 4 * x[2] - 42, 4 * x[1] + 4 * x[2],
        4 * x[1] + 4 * x[2], 12 * x[2]^2 + 4 * x[1] - 26
      ),
      nrow = 2
    )
  }
  himmelblau_objective <- Objective$
    new(f = himmelblau, target = "x", npar = 2)$
    set_gradient(himmelblau_gradient, .verbose = FALSE)$
    set_hessian(himmelblau_hessian, .verbose = FALSE)

  ### using gradient and Hessian via attribute
  nlm_opt <- Optimizer$new("stats::nlm")
  out <- nlm_opt$optimize(
    objective = himmelblau_objective,
    initial = c(1, 2)
  )
  expect_false(out$error)

  ### using gradient and Hessian via argument
  nlminb_opt <- Optimizer$new("stats::nlminb")
  out <- nlminb_opt$optimize(
    objective = himmelblau_objective,
    initial = c(1, 2)
  )
  expect_false(out$error)

  ### using gradient and Hessian via argument but Hessian cannot be used
  optim_opt <- Optimizer$new("stats::optim", method = "BFGS")
  expect_warning(
    out <- optim_opt$optimize(
      objective = himmelblau_objective,
      initial = c(1, 2)
    ),
    "The optimizer does not support custom Hessian function."
  )
  expect_false(out$error)
})

