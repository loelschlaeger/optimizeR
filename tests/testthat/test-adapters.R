test_that("ucminf and lbfgsb3c can be used as optimizer", {
  skip_if_not_installed("ucminf")
  skip_if_not_installed("lbfgsb3c")
  rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  for (which in c("ucminf::ucminf", "lbfgsb3c::lbfgsb3c")) {
    out <- Optimizer$new(which)$minimize(rosenbrock, c(-1.2, 1))
    expect_false(out$error)
    expect_equal(out$value, 0, tolerance = 1e-4)
    expect_equal(out$parameter, c(1, 1), tolerance = 1e-2)
  }
})

test_that("finite bounds can be required", {
  expect_true(
    require_bounds(c(-1, -1), c(1, 1), 2, "optimizer")
  )
  expect_error(
    require_bounds(NULL, NULL, 2, "optimizer"),
    "The optimizer \"optimizer\" searches between bounds"
  )
  expect_error(
    require_bounds(c(-1, -1), NULL, 2, "optimizer"),
    "needs finite"
  )
  expect_error(
    require_bounds(-1, 1, 2, "optimizer"),
    "needs finite"
  )
  expect_error(
    require_bounds(c(-Inf, -1), c(1, 1), 2, "optimizer"),
    "needs finite"
  )
})

test_that("ao can be used as optimizer", {
  skip_if_not_installed("ao")
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  ao_opt <- Optimizer$new("ao::ao")
  out <- suppressMessages(
    ao_opt$minimize(himmelblau, c(0, 0))
  )
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-4)
  expect_equal(himmelblau(out$parameter), out$value)

  ### minimization with bounds
  out <- suppressMessages(
    ao_opt$minimize(himmelblau, c(-1, -1), lower = c(-5, -5), upper = c(0, 0))
  )
  expect_true(all(out$parameter <= 0))
  expect_equal(out$value, 0, tolerance = 1e-4)
})

test_that("nloptr can be used as optimizer", {
  skip_if_not_installed("nloptr")
  rosenbrock <- function(x, shift = 0) {
    100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 + shift
  }
  rosenbrock_gradient <- function(x, shift = 0) {
    c(
      -400 * x[1] * (x[2] - x[1]^2) - 2 * (1 - x[1]),
      200 * (x[2] - x[1]^2)
    )
  }
  nloptr_opt <- Optimizer$new("nloptr::nloptr", opts = list(maxeval = 5000))
  out <- nloptr_opt$minimize(rosenbrock, c(-1.2, 1), shift = 2)
  expect_false(out$error)
  expect_equal(out$value, 2, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 1), tolerance = 1e-3)
  expect_named(
    out,
    c(
      "value", "parameter", "seconds", "initial", "error", "status", "message",
      "iterations"
    )
  )
  expect_true(out$status > 0)

  ### using gradient
  objective <- Objective$
    new(f = rosenbrock, npar = 2)$
    set_gradient(rosenbrock_gradient, .verbose = FALSE)
  out <- nloptr_opt$minimize(objective, c(-1.2, 1))
  expect_false(out$error)
  expect_equal(out$parameter, c(1, 1), tolerance = 1e-3)

  ### minimization with bounds
  out <- nloptr_opt$minimize(
    rosenbrock, c(-1.2, 1), lower = c(-2, -2), upper = c(0.5, 2)
  )
  expect_equal(out$parameter, c(0.5, 0.25), tolerance = 1e-3)

  ### maximization with another algorithm
  nelder_mead_opt <- Optimizer$new(
    "nloptr::nloptr",
    opts = list(algorithm = "NLOPT_LN_NELDERMEAD", maxeval = 5000)
  )
  out <- nelder_mead_opt$maximize(function(x) -rosenbrock(x), c(-1.2, 1))
  expect_equal(out$value, 0, tolerance = 1e-6)

  ### algorithm that requires gradient
  lbfgs_opt <- Optimizer$new(
    "nloptr::nloptr", opts = list(algorithm = "NLOPT_LD_LBFGS")
  )
  out <- lbfgs_opt$minimize(rosenbrock, c(-1.2, 1))
  expect_true(out$error)
  expect_match(out$error_message, "gradient")
})

test_that("vntrs can be used as optimizer", {
  skip_if_not_installed("vntrs")
  camel <- function(x) {
    (4 - 2.1 * x[1]^2 + x[1]^4 / 3) * x[1]^2 + x[1] * x[2] +
      (-4 + 4 * x[2]^2) * x[2]^2
  }
  vntrs_opt <- Optimizer$new("vntrs::vntrs")
  set.seed(1)
  out <- vntrs_opt$minimize(
    camel, c(0, 0), lower = c(-3, -2), upper = c(3, 2)
  )
  expect_false(out$error)
  expect_equal(out$value, -1.031628, tolerance = 1e-5)
  expect_equal(abs(out$parameter), c(0.0898, 0.7127), tolerance = 1e-3)
  checkmate::expect_data_frame(out$optima)

  ### maximization without bounds
  rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  set.seed(1)
  out <- vntrs_opt$maximize(function(x) -rosenbrock(x), c(0, 0))
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 1), tolerance = 1e-3)
})

test_that("optimx can be used as optimizer", {
  skip_if_not_installed("optimx")
  rosenbrock <- function(x, shift = 0) {
    100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 + shift
  }
  rosenbrock_gradient <- function(x, shift = 0) {
    c(
      -400 * x[1] * (x[2] - x[1]^2) - 2 * (1 - x[1]),
      200 * (x[2] - x[1]^2)
    )
  }
  optimr_opt <- Optimizer$new("optimx::optimr")
  out <- optimr_opt$minimize(rosenbrock, c(a = -1.2, b = 1), shift = 2)
  expect_false(out$error)
  expect_equal(out$value, 2, tolerance = 1e-6)
  expect_equal(out$parameter, c(a = 1, b = 1), tolerance = 1e-2)
  expect_null(attributes(out$value))
  expect_equal(out$convergence, 0)

  ### using gradient with another method
  objective <- Objective$
    new(f = rosenbrock, npar = 2)$
    set_gradient(rosenbrock_gradient, .verbose = FALSE)
  out <- Optimizer$new("optimx::optimr", method = "nvm")$
    minimize(objective, c(-1.2, 1))
  expect_false(out$error)
  expect_equal(out$parameter, c(1, 1), tolerance = 1e-4)

  ### minimization with bounds
  out <- optimr_opt$minimize(
    rosenbrock, c(-1.2, 1), lower = c(-2, -2), upper = c(0.5, 2)
  )
  expect_false(out$error)
  expect_equal(out$parameter, c(0.5, 0.25), tolerance = 1e-3)
  expect_match(out$message, "CONVERGENCE")
  out <- Optimizer$new("optimx::optimr", method = "L-BFGS-B")$
    minimize(rosenbrock, c(-1.2, 1), lower = c(-2, -2), upper = c(0.5, 2))
  expect_equal(out$parameter, c(0.5, 0.25), tolerance = 1e-3)

  ### maximization
  out <- optimr_opt$maximize(function(x) -rosenbrock(x), c(-1.2, 1))
  expect_equal(out$value, 0, tolerance = 1e-6)

  ### method that cannot be applied
  out <- Optimizer$new("optimx::optimr", method = "nvm")$
    maximize(function(x) -rosenbrock(x), c(-1.2, 1))
  expect_true(out$error)
  expect_identical(out$value, NA_real_)
  expect_identical(out$parameter, c(NA_real_, NA_real_))
  expect_match(out$error_message, "The method 'nvm' failed")
  expect_match(out$error_message, "gradient")
})

test_that("DEoptim can be used as optimizer", {
  skip_if_not_installed("DEoptim")
  camel <- function(x) {
    (4 - 2.1 * x[1]^2 + x[1]^4 / 3) * x[1]^2 + x[1] * x[2] +
      (-4 + 4 * x[2]^2) * x[2]^2
  }
  deoptim_opt <- Optimizer$new(
    "DEoptim::DEoptim", control = list(itermax = 100)
  )
  set.seed(1)
  expect_silent(
    out <- deoptim_opt$minimize(
      camel, c(0, 0), lower = c(-3, -2), upper = c(3, 2)
    )
  )
  expect_false(out$error)
  expect_equal(out$value, -1.031628, tolerance = 1e-5)
  expect_equal(abs(out$parameter), c(0.0898, 0.7127), tolerance = 1e-2)
  expect_identical(out$iterations, 100L)
  expect_true(out$evaluations > 100)

  ### maximization
  set.seed(1)
  out <- deoptim_opt$maximize(
    function(x) -camel(x), c(0, 0), lower = c(-3, -2), upper = c(3, 2)
  )
  expect_equal(out$value, 1.031628, tolerance = 1e-5)

  ### control via DEoptim::DEoptim.control()
  deoptim_opt <- Optimizer$new(
    "DEoptim::DEoptim",
    control = DEoptim::DEoptim.control(itermax = 20, trace = FALSE)
  )
  set.seed(1)
  out <- deoptim_opt$minimize(camel, c(0, 0), lower = -3, upper = 3)
  expect_false(out$error)
  expect_identical(out$iterations, 20L)

  ### bounds are required
  error_message <- paste(
    "The optimizer \"DEoptim::DEoptim\" searches between bounds and needs",
    "finite `lower` and `upper` bounds."
  )
  out <- deoptim_opt$minimize(camel, c(0, 0))
  expect_true(out$error)
  expect_equal(out$error_message, error_message)
  out <- deoptim_opt$minimize(camel, c(0, 0), lower = -Inf, upper = Inf)
  expect_true(out$error)
  expect_equal(out$error_message, error_message)
})

test_that("GenSA can be used as optimizer", {
  skip_if_not_installed("GenSA")
  rastrigin <- function(x) 10 * length(x) + sum(x^2 - 10 * cos(2 * pi * x))
  gensa_opt <- Optimizer$new("GenSA::GenSA", control = list(max.call = 5000))
  set.seed(1)
  out <- gensa_opt$minimize(
    rastrigin, c(3.3, -2.2), lower = c(-5.12, -5.12), upper = c(5.12, 5.12)
  )
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(0, 0), tolerance = 1e-4)
  expect_true(out$evaluations > 100)
  expect_null(out$trace.mat)

  ### bounds are required
  error_message <- paste(
    "The optimizer \"GenSA::GenSA\" searches between bounds and needs",
    "finite `lower` and `upper` bounds."
  )
  out <- gensa_opt$minimize(rastrigin, c(3.3, -2.2))
  expect_true(out$error)
  expect_equal(out$error_message, error_message)
  out <- gensa_opt$minimize(rastrigin, c(3.3, -2.2), lower = -5.12)
  expect_true(out$error)
  expect_equal(out$error_message, error_message)
})
