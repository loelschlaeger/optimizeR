### function that is busy for 'seconds' (Sys.sleep() cannot be interrupted)
busy_function <- function(seconds) {
  force(seconds)
  function(x) {
    end <- proc.time()[["elapsed"]] + seconds
    while (proc.time()[["elapsed"]] < end) NULL
    sum((x - 1)^2)
  }
}

### optimizer that evaluates the objective 'evaluations' times
repeating_optimizer <- function(evaluations, catch_errors = FALSE) {
  Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) {
        for (i in seq_len(evaluations)) {
          if (catch_errors) try(f(p), silent = TRUE) else f(p)
        }
        list(v = f(p), z = p)
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = "v",
      out_parameter = "z",
      direction = "min"
    )
}

test_that("expression can be evaluated with a time limit", {
  f <- busy_function(seconds = 2)

  ### time limit is not reached
  expect_equal(
    with_time_limit(1 + 1, seconds = Inf, on_time_out = "time out"),
    2
  )
  expect_equal(
    with_time_limit(1 + 1, seconds = 10, on_time_out = "time out"),
    2
  )

  ### time limit is reached
  expect_equal(
    with_time_limit(f(1), seconds = 0.05, on_time_out = "time out"),
    "time out"
  )

  ### errors are passed on
  expect_error(
    with_time_limit(stop("my error"), seconds = 10, on_time_out = "time out"),
    "my error"
  )
  expect_error(
    with_time_limit(stop("my error"), seconds = Inf, on_time_out = "time out"),
    "my error"
  )

  ### no time limit afterwards
  expect_equal(time_limit$deadline, Inf)
  expect_equal(busy_function(seconds = 0.1)(1), 0)
})

test_that("time limit can be checked", {
  expect_true(check_time_limit())
  out <- with_time_limit(
    {
      try(busy_function(seconds = 2)(1), silent = TRUE)
      ### R rounds its time limit to milliseconds
      Sys.sleep(0.01)
      check_time_limit()
      "time limit not reached"
    },
    seconds = 0.05,
    on_time_out = "time out"
  )
  expect_equal(out, "time out")
  expect_true(check_time_limit())
})

test_that("time limits can be nested", {
  f <- busy_function(seconds = 2)

  ### inner time limit is reached first
  out <- with_time_limit(
    c(
      with_time_limit(f(1), seconds = 0.05, on_time_out = "inner"),
      "continued"
    ),
    seconds = 10,
    on_time_out = "outer"
  )
  expect_equal(out, c("inner", "continued"))

  ### outer time limit is reached first
  out <- with_time_limit(
    c(
      with_time_limit(f(1), seconds = 10, on_time_out = "inner"),
      "continued"
    ),
    seconds = 0.05,
    on_time_out = "outer"
  )
  expect_equal(out, "outer")

  ### outer time limit holds after inner computations
  out <- with_time_limit(
    {
      with_time_limit(f(1), seconds = 0.05, on_time_out = "inner")
      with_time_limit(1 + 1, seconds = 0.05, on_time_out = "inner")
      with_time_limit(1 + 1, seconds = Inf, on_time_out = "inner")
      f(1)
    },
    seconds = 0.2,
    on_time_out = "outer"
  )
  expect_equal(out, "outer")
  expect_equal(time_limit$deadline, Inf)
})

test_that("optimization with time limit works", {
  f <- busy_function(seconds = 0.02)
  opt <- repeating_optimizer(evaluations = 100)
  opt$seconds <- 0.1
  out <- opt$minimize(objective = f, initial = c(5, 5))
  expect_true(out$error)
  expect_true(out$time_out)
  expect_equal(out$error_message, "time limit exceeded")
  expect_identical(out$value, NA_real_)
  expect_identical(out$parameter, c(NA_real_, NA_real_))
  expect_identical(out$seconds, NA_real_)
  expect_equal(out$initial, c(5, 5))
  expect_equal(time_limit$deadline, Inf)

  ### maximization with optimizer from dictionary
  nlm_opt <- Optimizer$new("stats::nlm")
  nlm_opt$seconds <- 0.05
  out <- nlm_opt$maximize(objective = function(x) -f(x), initial = c(5, 5))
  expect_true(out$time_out)

  ### time limit is not reached
  opt <- repeating_optimizer(evaluations = 3)
  opt$seconds <- 60
  out <- opt$minimize(objective = function(x) sum(x^2), initial = c(5, 5))
  expect_false(out$error)
  expect_null(out$time_out)
  expect_equal(out$value, 50)

  ### time limit of zero seconds
  opt$seconds <- 0
  out <- opt$minimize(objective = function(x) sum(x^2), initial = c(5, 5))
  expect_true(out$time_out)
})

test_that("time out does not depend on the language", {
  local_reproducible_output(lang = "de")
  f <- busy_function(seconds = 0.02)
  opt <- repeating_optimizer(evaluations = 100)
  opt$seconds <- 0.1
  out <- opt$minimize(objective = f, initial = c(5, 5))
  expect_true(out$time_out)
  expect_equal(out$error_message, "time limit exceeded")
})

test_that("time out can be detected if optimizer catches errors", {
  f <- busy_function(seconds = 0.02)
  opt <- repeating_optimizer(evaluations = 100, catch_errors = TRUE)
  opt$seconds <- 0.1
  out <- opt$minimize(objective = f, initial = c(5, 5))
  expect_true(out$time_out)
})

test_that("time out can be detected after code that cannot be interrupted", {
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) {
        Sys.sleep(0.3)
        list(v = f(p), z = p)
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = "v",
      out_parameter = "z",
      direction = "min"
    )
  opt$seconds <- 0.1
  out <- opt$minimize(objective = function(x) sum(x^2), initial = c(5, 5))
  expect_true(out$time_out)
})

test_that("time limit holds for nested computations", {
  f <- busy_function(seconds = 0.02)

  ### objective with own time limit
  objective <- Objective$new(f = f, npar = 2)
  objective$seconds <- 30
  opt <- repeating_optimizer(evaluations = 100)
  opt$seconds <- 0.1
  out <- opt$minimize(objective = objective, initial = c(5, 5))
  expect_true(out$time_out)

  ### optimizer that solves sub-problems with another optimizer
  nested_opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) {
        inner_opt <- Optimizer$new("stats::optim", control = list(maxit = 10))
        for (i in 1:20) {
          p <- inner_opt$minimize(function(x) f(x), p)$parameter
        }
        list(v = f(p), z = p)
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = "v",
      out_parameter = "z",
      direction = "min"
    )
  nested_opt$seconds <- 0.2
  out <- nested_opt$minimize(objective = f, initial = c(5, 5))
  expect_true(out$time_out)
  expect_equal(time_limit$deadline, Inf)

  ### time limit of inner optimization only ends this optimization
  outer_opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) {
        inner_opt <- repeating_optimizer(evaluations = 100)
        inner_opt$seconds <- 0.05
        list(v = f(p), z = p, inner = inner_opt$minimize(function(x) f(x), p))
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = "v",
      out_parameter = "z",
      direction = "min"
    )
  outer_opt$seconds <- 30
  out <- outer_opt$minimize(objective = f, initial = c(5, 5))
  expect_false(out$error)
  expect_equal(out$value, 32)
  expect_true(out$inner$time_out)
})

test_that("time limit from outside is not taken over", {
  objective <- Objective$new(f = busy_function(seconds = 2), npar = 1)
  evaluate_with_time_limit <- function() {
    setTimeLimit(elapsed = 0.1, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = TRUE))
    objective$evaluate(1)
  }
  expect_error(
    evaluate_with_time_limit(),
    "Function evaluation threw an error"
  )
})
