test_that("optimizer object exceptions can be detected", {

  ### objective definition
  objective <- function(x) sum(x^2)

  ### test bad output
  bad_output_opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, x, ...) 1,
      arg_objective = "f",
      arg_initial = "x",
      out_value = "value",
      out_parameter = "parameter",
      direction = "min"
    )
  expect_error(
    bad_output_opt$minimize(objective = objective, initial = 1:2),
    "The optimizer output is bad: Must have names"
  )
  bad_output_opt$algorithm <- function(f, x, ...) list(f(x), x)
  expect_error(
    bad_output_opt$minimize(objective = objective, initial = 1:2),
    "The optimizer output is bad: Must have names"
  )
  bad_output_opt$algorithm <- function(f, x, ...) list(val = f(x), par = x)
  expect_error(
    bad_output_opt$minimize(objective = objective, initial = 1:2),
    "The optimizer output is bad: Names must include the elements"
  )

  ### test error
  error_opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) {
        if (identical(p, 1:2)) stop("error message")
        list(v = f(p), z = 1:2)
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = "v",
      out_parameter = "z",
      direction = "min"
    )
  out <- error_opt$
    minimize(objective = objective, initial = 1:2)
  expect_true(out$error)
  expect_equal(out$error_message, "error message")
  expect_identical(out$value, NA_real_)
  expect_identical(out$parameter, c(NA_real_, NA_real_))
  expect_identical(out$seconds, NA_real_)
  expect_identical(out$initial, 1:2)

  ### test error of objective function
  out <- Optimizer$new("stats::nlm")$
    minimize(objective = function(x) stop("no value"), initial = 1:2)
  expect_true(out$error)
  expect_identical(
    out$error_message, "Function evaluation threw an error: no value"
  )
})

test_that("error messages in output are plain text", {
  local_reproducible_output(crayon = TRUE)
  objective <- function(x) sum(x^2)
  error_opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) cli::cli_abort("Bad {.val value}."),
      arg_objective = "f",
      arg_initial = "p",
      out_value = "v",
      out_parameter = "z",
      direction = "min"
    )
  out <- error_opt$minimize(objective = objective, initial = 1:2)
  expect_equal(out$error_message, "Bad \"value\".")
  error_opt$algorithm <- function(f, p, ...) stop("failed\n")
  out <- error_opt$minimize(objective = objective, initial = 1:2)
  expect_equal(out$error_message, "failed")
})

test_that("custom optimizer must be defined before use", {
  objective <- function(x) sum(x^2)
  expect_message(
    opt <- Optimizer$new("custom"),
    "to define a custom optimizer"
  )
  expect_equal(opt$label, "unlabeled")
  expect_error(
    opt$minimize(objective, c(3, 3)),
    "The optimizer is not defined yet"
  )
  evaluate_once <- function(f, p, ...) list(v = f(p), z = p)
  expect_identical(
    opt$definition(
      algorithm = evaluate_once,
      arg_objective = "f",
      arg_initial = "p",
      out_value = "v",
      out_parameter = "z",
      direction = "min"
    ),
    opt
  )
  expect_equal(opt$label, "evaluate_once")
  out <- opt$minimize(objective, c(3, 3))
  expect_false(out$error)
  expect_equal(out$value, 18)
  expect_equal(out$parameter, c(3, 3))
  expect_equal(opt$maximize(objective, c(3, 3))$value, 18)
})

test_that("invalid optimizer definitions can be detected", {
  opt <- Optimizer$new("custom", .verbose = FALSE)
  algorithm <- function(f, p) list(v = f(p), z = p)
  expect_error(
    opt$definition(),
    "Input `algorithm` is bad: Argument needs a value"
  )
  expect_error(
    opt$definition(algorithm = "algorithm"),
    "Input `algorithm` is bad: Must be a function"
  )
  expect_error(
    opt$definition(algorithm = algorithm),
    "Input `arg_objective` is bad: Argument needs a value"
  )
  expect_error(
    opt$definition(algorithm = algorithm, arg_objective = "f"),
    "Input `arg_initial` is bad: Argument needs a value"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "p"
    ),
    "Input `out_value` is bad: Argument needs a value"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "p",
      out_value = "v"
    ),
    "Input `out_parameter` is bad: Argument needs a value"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "p",
      out_value = "v", out_parameter = "z"
    ),
    "Input `direction` is bad: Argument needs a value"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "p",
      out_value = "v", out_parameter = "z", direction = "up"
    ),
    "Input `direction` is bad"
  )

  ### argument names
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "fn", arg_initial = "p",
      out_value = "v", out_parameter = "z", direction = "min"
    ),
    "Input `algorithm` is bad: Must have formal arguments: fn"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "x0",
      out_value = "v", out_parameter = "z", direction = "min"
    ),
    "Input `algorithm` is bad: Must have formal arguments: x0"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = NA,
      arg_lower = "lower", arg_upper = "upper",
      out_value = "v", out_parameter = "z", direction = "min"
    ),
    "Input `algorithm` is bad: Must have formal arguments: lower,upper"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "p",
      arg_gradient = "gr", arg_hessian = "hess",
      out_value = "v", out_parameter = "z", direction = "min"
    ),
    "Input `algorithm` is bad: Must have formal arguments: gr,hess"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = NA, arg_initial = "p",
      out_value = "v", out_parameter = "z", direction = "min"
    ),
    "Input `arg_objective` is bad"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = TRUE,
      out_value = "v", out_parameter = "z", direction = "min"
    ),
    "Input `arg_initial` is bad"
  )

  ### gradient and Hessian as attribute
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "p",
      gradient_as_attribute = TRUE,
      out_value = "v", out_parameter = "z", direction = "min"
    ),
    "Input `arg_gradient` is bad: Must be the attribute name"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "p",
      hessian_as_attribute = TRUE,
      out_value = "v", out_parameter = "z", direction = "min"
    ),
    "Input `arg_hessian` is bad: Must be the attribute name"
  )

  ### output elements
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "p",
      out_value = 1, out_parameter = "z", direction = "min"
    ),
    "Input `out_value` is bad"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "p",
      out_value = "v", out_parameter = character(), direction = "min"
    ),
    "Input `out_parameter` is bad"
  )
  expect_error(
    opt$definition(
      algorithm = algorithm, arg_objective = "f", arg_initial = "p",
      out_value = c("v", NA), out_parameter = "z", direction = "min"
    ),
    "Input `out_value` is bad"
  )
})

test_that("optimizer without ... argument can be defined", {
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p) {
        out <- stats::optim(par = p, fn = f, method = "BFGS")
        list(v = out$value, z = out$par)
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = "v",
      out_parameter = "z",
      direction = "min"
    )
  objective <- function(x, a, b = 0) sum((x - a)^2) + b
  out <- opt$minimize(objective = objective, initial = c(3, 3), a = 1)
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 1), tolerance = 1e-3)
  expect_named(out, c("value", "parameter", "seconds", "initial", "error"))

  ### maximization and fixed arguments
  out <- opt$maximize(
    objective = function(x, a, b) -objective(x, a, b), initial = c(3, 3),
    a = 1:2, b = 5
  )
  expect_equal(out$value, -5, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 2), tolerance = 1e-3)
  out <- opt$minimize(
    objective = Objective$new(f = objective, npar = 2, a = 2, b = 1),
    initial = c(3, 3)
  )
  expect_equal(out$value, 1, tolerance = 1e-6)
  expect_equal(out$parameter, c(2, 2), tolerance = 1e-3)

  ### optimizer arguments must be arguments of the algorithm
  opt$set_arguments(maxit = 5)
  out <- opt$minimize(objective = objective, initial = c(3, 3), a = 1)
  expect_true(out$error)
  expect_match(out$error_message, "maxit")
})

test_that("objective is passed without ... argument if possible", {
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) {
        list(v = f(p, ...), z = p, arguments = names(formals(f)))
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = "v",
      out_parameter = "z",
      direction = "min"
    )
  objective <- function(x) sum(x^2)
  out <- opt$minimize(objective = objective, initial = c(3, 3))
  expect_false(out$error)
  expect_equal(out$arguments, ".at")
  opt$set_arguments(unknown = 1)
  out <- opt$minimize(objective = objective, initial = c(3, 3))
  expect_false(out$error)
  expect_equal(out$arguments, c(".at", "..."))
  expect_equal(out$value, 18)
})

test_that("optimizer without initial values can be defined", {

  ### grid search between bounds
  grid_search <- function(fn, lower, upper) {
    grid <- as.matrix(expand.grid(Map(seq, lower, upper, length.out = 11)))
    values <- apply(grid, 1, fn)
    list(fmin = min(values), xmin = unname(grid[which.min(values), ]))
  }
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = grid_search,
      arg_objective = "fn",
      arg_initial = NA,
      arg_lower = "lower",
      arg_upper = "upper",
      out_value = "fmin",
      out_parameter = "xmin",
      direction = "min"
    )
  expect_identical(opt$arg_initial, NA)
  objective <- function(x) sum((x - 2)^2)
  out <- opt$minimize(
    objective = objective, initial = c(0, 0), lower = -5, upper = 5
  )
  expect_false(out$error)
  expect_equal(out$value, 0)
  expect_equal(out$parameter, c(2, 2))
  expect_equal(out$initial, c(0, 0))
  expect_named(out, c("value", "parameter", "seconds", "initial", "error"))
  out <- opt$maximize(
    objective = function(x) -objective(x), initial = c(a = 0, b = 0, c = 0),
    lower = c(-5, 0, 0), upper = 5
  )
  expect_equal(out$value, 0)
  expect_equal(out$parameter, c(2, 2, 2))
  expect_equal(out$initial, c(a = 0, b = 0, c = 0))
  out <- opt$optimize(
    objective = objective, initial = 0, lower = -5, upper = 5,
    direction = "min"
  )
  expect_equal(out$parameter, 2)

  ### initial values are still checked
  expect_error(
    opt$minimize(objective = objective, lower = -5, upper = 5),
    "Input `initial` is bad: Argument needs a value"
  )
  expect_error(
    opt$minimize(objective = objective, initial = "a", lower = -5, upper = 5),
    "Input `initial` is bad: Must be of type 'numeric'"
  )
  expect_error(
    opt$minimize(
      objective = Objective$new(f = objective, npar = 2), initial = 0,
      lower = -5, upper = 5
    ),
    "Input `initial` is bad: Must have length 2"
  )

  ### only the argument for the objective function is reserved
  opt$set_arguments(fn = objective)
  expect_error(
    opt$minimize(
      objective = objective, initial = c(0, 0), lower = -5, upper = 5
    ),
    "The optimizer argument `fn` cannot be set"
  )

  ### stats::optimize
  optimize_opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = stats::optimize,
      arg_objective = "f",
      arg_initial = NA,
      arg_lower = "lower",
      arg_upper = "upper",
      out_value = "objective",
      out_parameter = "minimum",
      direction = "min"
    )
  out <- optimize_opt$minimize(
    objective = function(x) (x - 1)^2, initial = 0, lower = -5, upper = 5
  )
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, 1, tolerance = 1e-4)
  expect_equal(out$initial, 0)
  out <- optimize_opt$maximize(
    objective = function(x, a) -(x - a)^2 + 3, initial = 0, lower = -5,
    upper = 5, a = 2
  )
  expect_equal(out$value, 3, tolerance = 1e-6)
  expect_equal(out$parameter, 2, tolerance = 1e-4)
})

test_that("optimizer with nested output can be defined", {
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) {
        out <- stats::optim(par = p, fn = f, method = "BFGS")
        list(
          "solution" = list("optimum" = out$value, "argument" = out$par),
          "counts" = out$counts
        )
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = c("solution", "optimum"),
      out_parameter = c("solution", "argument"),
      direction = "min"
    )
  expect_equal(opt$out_value, c("solution", "optimum"))
  expect_equal(opt$out_parameter, c("solution", "argument"))
  objective <- function(x) sum((x - 1:2)^2)
  out <- opt$minimize(objective = objective, initial = c(0, 0))
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 2), tolerance = 1e-3)
  expect_named(
    out,
    c(
      "value", "parameter", "seconds", "initial", "error", "solution",
      "counts"
    )
  )
  expect_identical(out$solution$argument, out$parameter)
  out <- opt$maximize(
    objective = function(x) 1 - objective(x), initial = c(0, 0)
  )
  expect_equal(out$value, 1, tolerance = 1e-6)
  expect_equal(out$solution$optimum, -1, tolerance = 1e-6)

  ### elements can be ignored
  opt$output_ignore <- "solution"
  expect_named(
    opt$minimize(objective = objective, initial = c(0, 0)),
    c("value", "parameter", "seconds", "initial", "error", "counts")
  )

  ### test bad output
  opt$out_value <- c("solution", "minimum")
  expect_error(
    opt$minimize(objective = objective, initial = c(0, 0)),
    "The optimizer output is bad: Names must include the elements"
  )
  opt$out_value <- c("solution", "optimum", "minimum")
  expect_error(
    opt$minimize(objective = objective, initial = c(0, 0)),
    "The optimizer output is bad: Must have names"
  )
  opt$out_value <- c("solution", "optimum")
  opt$out_parameter <- c("result", "argument")
  expect_error(
    opt$minimize(objective = objective, initial = c(0, 0)),
    "The optimizer output is bad: Names must include the elements"
  )
})

test_that("optimizer with S4 output can be defined", {
  methods::setClass(
    "optimizeR_test_output",
    slots = c(optimum = "numeric", argument = "numeric", details = "list"),
    where = globalenv()
  )
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) {
        out <- stats::optim(par = p, fn = f, method = "BFGS")
        methods::new(
          "optimizeR_test_output",
          optimum = out$value, argument = out$par,
          details = list("counts" = out$counts)
        )
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = "optimum",
      out_parameter = "argument",
      direction = "min"
    )
  objective <- function(x) sum((x - 1:2)^2)
  out <- opt$minimize(objective = objective, initial = c(0, 0))
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 2), tolerance = 1e-3)
  expect_named(
    out, c("value", "parameter", "seconds", "initial", "error", "output")
  )
  checkmate::expect_class(out$output, "optimizeR_test_output")
  expect_identical(out$output@argument, out$parameter)

  ### slots can be nested and extracted via functions
  opt$out_value <- function(output) output@optimum
  opt$out_parameter <- c("details", "counts")
  out <- opt$maximize(objective = function(x) -objective(x), initial = c(0, 0))
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_identical(out$parameter, out$output@details$counts)

  ### output can be ignored
  opt$output_ignore <- "output"
  expect_named(
    opt$minimize(objective = objective, initial = c(0, 0)),
    c("value", "parameter", "seconds", "initial", "error")
  )

  ### test bad output
  opt$out_value <- "value"
  expect_error(
    opt$minimize(objective = objective, initial = c(0, 0)),
    "The optimizer output is bad: Names must include the elements"
  )
  methods::removeClass("optimizeR_test_output", where = globalenv())
})

test_that("optimizer with atomic output can be defined", {
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) {
        out <- stats::optim(par = p, fn = f, method = "BFGS")
        c(out$value, out$par)
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = function(output) output[1],
      out_parameter = function(output) output[-1],
      direction = "min"
    )
  objective <- function(x) sum((x - 1:2)^2)
  out <- opt$minimize(objective = objective, initial = c(0, 0))
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 2), tolerance = 1e-3)
  expect_named(
    out, c("value", "parameter", "seconds", "initial", "error", "output")
  )
  expect_identical(out$output, c(out$value, out$parameter))
  out <- opt$maximize(objective = function(x) -objective(x), initial = c(0, 0))
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_identical(out$output, c(-out$value, out$parameter))

  ### names of the output can be used
  opt$algorithm <- function(f, p, ...) {
    out <- stats::optim(par = p, fn = f, method = "BFGS")
    c("optimum" = out$value, "x1" = out$par[1], "x2" = out$par[2])
  }
  opt$out_value <- "optimum"
  opt$out_parameter <- function(output) unname(output[c("x1", "x2")])
  out <- opt$minimize(objective = objective, initial = c(0, 0))
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 2), tolerance = 1e-3)
  expect_named(out$output, c("optimum", "x1", "x2"))

  ### test bad output
  opt$out_value <- "value"
  expect_error(
    opt$minimize(objective = objective, initial = c(0, 0)),
    "The optimizer output is bad: Names must include the elements"
  )
})

test_that("optimizer output can be extracted via functions", {
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) {
        out <- stats::optim(par = p, fn = f, method = "BFGS")
        list("values" = c(f(p), out$value), "estimate" = as.list(out$par))
      },
      arg_objective = "f",
      arg_initial = "p",
      out_value = function(output) output$values[length(output$values)],
      out_parameter = function(output) unlist(output$estimate),
      direction = "min"
    )
  expect_true(is.function(opt$out_value))
  expect_true(is.function(opt$out_parameter))
  objective <- function(x) sum((x - 1:2)^2)
  out <- opt$minimize(objective = objective, initial = c(0, 0))
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 2), tolerance = 1e-3)
  expect_named(
    out,
    c(
      "value", "parameter", "seconds", "initial", "error", "values",
      "estimate"
    )
  )
  expect_equal(out$values[1], 5)
  out <- opt$maximize(objective = function(x) -objective(x), initial = c(0, 0))
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$values[1], 5)

  ### functions and names can be combined
  opt$out_parameter <- "estimate"
  out <- opt$minimize(objective = objective, initial = c(0, 0))
  expect_equal(out$parameter, list(1, 2), tolerance = 1e-3)
  expect_named(
    out, c("value", "parameter", "seconds", "initial", "error", "values")
  )

  ### test error
  opt$out_value <- function(output) stop("no {value} available")
  expect_error(
    opt$minimize(objective = objective, initial = c(0, 0)),
    "The optimizer output is bad: `out_value` threw an error: no {value}",
    fixed = TRUE
  )
  opt$out_value <- function(output) output$minimum
  expect_error(
    opt$minimize(objective = objective, initial = c(0, 0)),
    "The optimizer output is bad: `out_value` returned `NULL`"
  )
  opt$out_value <- function(output) output$values[2]
  opt$out_parameter <- function(output) stop("not available")
  expect_error(
    opt$minimize(objective = objective, initial = c(0, 0)),
    "The optimizer output is bad: `out_parameter` threw an error"
  )

  ### failure of the optimizer is still detected
  opt$algorithm <- function(f, p, ...) stop("error message")
  out <- opt$minimize(objective = objective, initial = c(0, 0))
  expect_true(out$error)
  expect_equal(out$error_message, "error message")
  expect_identical(out$value, NA_real_)
  expect_identical(out$parameter, c(NA_real_, NA_real_))
})

test_that("pracma::steep_descent can be defined as optimizer", {
  skip_if_not_installed("pracma")
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = pracma::steep_descent,
      arg_objective = "f",
      arg_initial = "x0",
      arg_gradient = "g",
      out_value = "fmin",
      out_parameter = "xmin",
      direction = "min"
    )
  objective <- function(x, shift) sum((x - shift)^2)
  out <- opt$minimize(objective = objective, initial = c(0, 0), shift = 1:2)
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 2), tolerance = 1e-3)
  expect_named(
    out, c("value", "parameter", "seconds", "initial", "error", "niter")
  )
  out <- opt$maximize(
    objective = function(x, shift) -objective(x, shift), initial = c(0, 0),
    shift = 1:2
  )
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 2), tolerance = 1e-3)

  ### gradient function
  gradient_used <- FALSE
  objective_object <- Objective$
    new(f = objective, npar = 2, shift = 1:2)$
    set_gradient(
      function(x, shift) {
        gradient_used <<- TRUE
        2 * (x - shift)
      },
      .verbose = FALSE
    )
  opt$set_arguments(maxiter = 500)
  out <- opt$minimize(objective = objective_object, initial = c(0, 0))
  expect_true(gradient_used)
  expect_equal(out$parameter, c(1, 2), tolerance = 1e-3)
})

test_that("DEoptim::DEoptim can be defined as optimizer", {
  skip_if_not_installed("DEoptim")
  camel <- function(x) {
    (4 - 2.1 * x[1]^2 + x[1]^4 / 3) * x[1]^2 + x[1] * x[2] +
      (-4 + 4 * x[2]^2) * x[2]^2
  }
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = DEoptim::DEoptim,
      arg_objective = "fn",
      arg_initial = NA,
      arg_lower = "lower",
      arg_upper = "upper",
      out_value = c("optim", "bestval"),
      out_parameter = c("optim", "bestmem"),
      direction = "min"
    )
  opt$set_arguments(
    control = DEoptim::DEoptim.control(itermax = 100, trace = FALSE)
  )
  set.seed(1)
  out <- opt$minimize(
    objective = camel, initial = c(0, 0), lower = c(-3, -2), upper = c(3, 2)
  )
  expect_false(out$error)
  expect_equal(out$value, -1.031628, tolerance = 1e-5)
  expect_equal(
    abs(unname(out$parameter)), c(0.0898, 0.7127), tolerance = 1e-2
  )
  expect_equal(out$initial, c(0, 0))
  expect_named(
    out,
    c("value", "parameter", "seconds", "initial", "error", "optim", "member")
  )
  expect_identical(out$optim$iter, 100L)

  ### same result as via the dictionary
  set.seed(1)
  out_dictionary <- Optimizer$new(
    "DEoptim::DEoptim", control = list(itermax = 100)
  )$minimize(
    objective = camel, initial = c(0, 0), lower = c(-3, -2), upper = c(3, 2)
  )
  expect_identical(out$value, out_dictionary$value)
  expect_identical(unname(out$parameter), out_dictionary$parameter)

  ### maximization
  opt$output_ignore <- c("optim", "member")
  set.seed(1)
  out <- opt$maximize(
    objective = function(x) -camel(x), initial = c(0, 0), lower = c(-3, -2),
    upper = c(3, 2)
  )
  expect_equal(out$value, 1.031628, tolerance = 1e-5)
  expect_named(out, c("value", "parameter", "seconds", "initial", "error"))
})

test_that("GenSA::GenSA can be defined as optimizer", {
  skip_if_not_installed("GenSA")
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = GenSA::GenSA,
      arg_objective = "fn",
      arg_initial = NA,
      arg_lower = "lower",
      arg_upper = "upper",
      out_value = "value",
      out_parameter = "par",
      direction = "min"
    )
  opt$set_arguments(control = list(max.call = 5000))
  opt$output_ignore <- "trace.mat"
  rastrigin <- function(x) 10 * length(x) + sum(x^2 - 10 * cos(2 * pi * x))
  set.seed(1)
  out <- opt$minimize(
    objective = rastrigin, initial = c(3.3, -2.2), lower = -5.12,
    upper = 5.12
  )
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(0, 0), tolerance = 1e-4)
  expect_named(
    out, c("value", "parameter", "seconds", "initial", "error", "counts")
  )

  ### with initial values
  opt$arg_initial <- "par"
  set.seed(1)
  out <- opt$maximize(
    objective = function(x) -rastrigin(x), initial = c(3.3, -2.2),
    lower = -5.12, upper = 5.12
  )
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(0, 0), tolerance = 1e-4)
})

test_that("nloptr::nloptr can be defined as optimizer", {
  skip_if_not_installed("nloptr")
  rosenbrock <- function(x, shift = 0) {
    100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 + shift
  }
  rosenbrock_gradient <- function(x) {
    c(
      -400 * x[1] * (x[2] - x[1]^2) - 2 * (1 - x[1]),
      200 * (x[2] - x[1]^2)
    )
  }
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = nloptr::nloptr,
      arg_objective = "eval_f",
      arg_initial = "x0",
      arg_lower = "lb",
      arg_upper = "ub",
      arg_gradient = "eval_grad_f",
      out_value = "objective",
      out_parameter = "solution",
      direction = "min"
    )
  opt$set_arguments(
    opts = list(algorithm = "NLOPT_LN_BOBYQA", xtol_rel = 1e-8, maxeval = 5000)
  )
  out <- opt$minimize(objective = rosenbrock, initial = c(-1.2, 1), shift = 2)
  expect_false(out$error)
  expect_equal(out$value, 2, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 1), tolerance = 1e-3)

  ### same result as via the dictionary
  out_dictionary <- Optimizer$new(
    "nloptr::nloptr", opts = list(maxeval = 5000)
  )$minimize(objective = rosenbrock, initial = c(-1.2, 1), shift = 2)
  expect_identical(out$value, out_dictionary$value)
  expect_identical(out$parameter, out_dictionary$parameter)
  expect_identical(out$iterations, out_dictionary$iterations)

  ### gradient function, bounds, and maximization
  objective <- Objective$
    new(f = function(x) -rosenbrock(x), npar = 2)$
    set_gradient(function(x) -rosenbrock_gradient(x), .verbose = FALSE)
  opt$set_arguments(
    opts = list(algorithm = "NLOPT_LD_LBFGS", xtol_rel = 1e-8, maxeval = 5000)
  )
  out <- opt$maximize(
    objective = objective, initial = c(-1.2, 1), lower = c(-2, -2),
    upper = c(0.5, 2)
  )
  expect_false(out$error)
  expect_equal(out$value, -0.25, tolerance = 1e-6)
  expect_equal(out$parameter, c(0.5, 0.25), tolerance = 1e-3)
})

test_that("nloptr::direct can be defined as optimizer", {
  skip_if_not_installed("nloptr")
  camel <- function(x) {
    (4 - 2.1 * x[1]^2 + x[1]^4 / 3) * x[1]^2 + x[1] * x[2] +
      (-4 + 4 * x[2]^2) * x[2]^2
  }
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = nloptr::direct,
      arg_objective = "fn",
      arg_initial = NA,
      arg_lower = "lower",
      arg_upper = "upper",
      out_value = "value",
      out_parameter = "par",
      direction = "min"
    )
  out <- opt$minimize(
    objective = camel, initial = c(0, 0), lower = c(-3, -2), upper = c(3, 2)
  )
  expect_false(out$error)
  expect_equal(out$value, -1.031628, tolerance = 1e-5)
  expect_equal(abs(out$parameter), c(0.0898, 0.7127), tolerance = 1e-2)
  expect_named(
    out,
    c(
      "value", "parameter", "seconds", "initial", "error", "iter",
      "convergence", "message"
    )
  )
  out <- opt$maximize(
    objective = function(x) -camel(x), initial = c(0, 0), lower = c(-3, -2),
    upper = c(3, 2)
  )
  expect_equal(out$value, 1.031628, tolerance = 1e-5)
})

test_that("optimizers that call the objective in their own way work", {
  rosenbrock <- function(x) {
    100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  }

  ### optimizer passes an argument of its own to the objective
  nlm_passing_on <- function(f, p) {
    stats::nlm(f = f, p = p, info = "passed on by the optimizer")
  }
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = nlm_passing_on,
      arg_objective = "f",
      arg_initial = "p",
      out_value = "minimum",
      out_parameter = "estimate",
      direction = "min"
    )
  out <- opt$minimize(objective = rosenbrock, initial = c(-1.2, 1))
  expected <- stats::nlm(f = rosenbrock, p = c(-1.2, 1))
  expect_false(out$error)
  expect_identical(out$value, expected$minimum)
  expect_identical(out$parameter, expected$estimate)

  ### optimizer names the parameter argument
  optim_naming <- function(objective, initial) {
    stats::optim(par = initial, fn = function(theta) objective(theta = theta))
  }
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = optim_naming,
      arg_objective = "objective",
      arg_initial = "initial",
      out_value = "value",
      out_parameter = "par",
      direction = "min"
    )
  out <- opt$minimize(objective = rosenbrock, initial = c(-1.2, 1))
  expected <- stats::optim(par = c(-1.2, 1), fn = rosenbrock)
  expect_false(out$error)
  expect_identical(out$value, expected$value)
  expect_identical(out$parameter, expected$par)

  ### error of an optimizer that fails before it evaluates the objective
  failing <- function(f, p) {
    stop("failed")
  }
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = failing,
      arg_objective = "f",
      arg_initial = "p",
      out_value = "value",
      out_parameter = "parameter",
      direction = "min"
    )
  out <- opt$minimize(objective = rosenbrock, initial = c(-1.2, 1))
  expect_true(out$error)
  expect_identical(out$error_message, "failed")
})

test_that("optimx::optimx can be defined as optimizer", {
  skip_if_not_installed("optimx")
  rosenbrock <- function(x, shift = 0) {
    100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 + shift
  }
  rosenbrock_gradient <- function(x) {
    c(
      -400 * x[1] * (x[2] - x[1]^2) - 2 * (1 - x[1]),
      200 * (x[2] - x[1]^2)
    )
  }
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = optimx::optimx,
      arg_objective = "fn",
      arg_initial = "par",
      arg_lower = "lower",
      arg_upper = "upper",
      arg_gradient = "gr",
      arg_hessian = "hess",
      out_value = "value",
      out_parameter = function(output) {
        unlist(output[1, seq_len(attr(output, "npar"))])
      },
      direction = "min"
    )
  opt$set_arguments(method = "BFGS")
  opt$hide_warnings <- TRUE
  out <- opt$minimize(objective = rosenbrock, initial = c(-1.2, 1), shift = 2)
  expect_false(out$error)
  expect_equal(out$value, 2, tolerance = 1e-6)
  expect_equal(unname(out$parameter), c(1, 1), tolerance = 1e-3)
  expect_named(
    out, c("value", "parameter", "seconds", "initial", "error", "output")
  )
  expect_s3_class(out$output, "data.frame")
  expect_equal(out$output$convcode, 0)

  ### gradient function and maximization
  objective <- Objective$
    new(f = function(x) -rosenbrock(x), npar = 2)$
    set_gradient(function(x) -rosenbrock_gradient(x), .verbose = FALSE)
  out <- opt$maximize(objective = objective, initial = c(-1.2, 1))
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(unname(out$parameter), c(1, 1), tolerance = 1e-3)
})

test_that("ucminf and lbfgsb3c can be defined via functions", {
  skip_if_not_installed("ucminf")
  skip_if_not_installed("lbfgsb3c")
  rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  for (algorithm in list(ucminf::ucminf, lbfgsb3c::lbfgsb3c)) {
    opt <- Optimizer$new("custom", .verbose = FALSE)$
      definition(
        algorithm = algorithm,
        arg_objective = "fn",
        arg_initial = "par",
        arg_gradient = "gr",
        out_value = "value",
        out_parameter = "par",
        direction = "min"
      )
    out <- opt$minimize(objective = rosenbrock, initial = c(-1.2, 1))
    expect_equal(out$value, 0, tolerance = 1e-4)
    expect_equal(out$parameter, c(1, 1), tolerance = 1e-2)
    opt$out_value <- function(output) output$value
    opt$out_parameter <- function(output) output$par
    out_functions <- opt$minimize(objective = rosenbrock, initial = c(-1.2, 1))
    expect_identical(out_functions$value, out$value)
    expect_identical(out_functions$parameter, out$parameter)
    expect_identical(out_functions$message, out$message)
    expect_setequal(names(out_functions), c(names(out), "par"))
  }
})

test_that("optimizer arguments can be set", {
  opt <- Optimizer$new("stats::optim", method = "BFGS")
  expect_identical(opt$arguments, list(method = "BFGS"))
  expect_identical(opt$set_arguments(hessian = TRUE), opt)
  expect_identical(opt$arguments, list(method = "BFGS", hessian = TRUE))
  opt$set_arguments(method = "CG")
  expect_identical(opt$arguments, list(method = "CG", hessian = TRUE))
  expect_identical(opt$set_arguments(), opt)
  objective <- function(x) sum((x - 1)^2)
  out <- Optimizer$new("stats::nlm")$
    set_arguments(iterlim = 1)$
    minimize(objective, c(3, 3))
  expect_identical(out$iterations, 1L)

  ### arguments of custom optimizer
  custom <- Optimizer$new("custom", maxit = 5, .verbose = FALSE)
  expect_identical(custom$arguments, list(maxit = 5))

  ### arguments must be named
  expect_error(
    Optimizer$new("stats::nlm", 100),
    "Input `...` is bad: Must have names"
  )
  expect_error(
    opt$set_arguments(1, a = 2),
    "Input `...` is bad: Must have names"
  )

  ### arguments for objective and initial values cannot be set
  expect_error(
    Optimizer$new("stats::optim", par = 1)$minimize(objective, c(3, 3)),
    "The optimizer argument `par` cannot be set"
  )

  ### bounds in call have priority over optimizer arguments
  bounded <- Optimizer$new(
    "stats::optim", method = "L-BFGS-B", lower = c(2, 2)
  )
  expect_equal(bounded$minimize(objective, c(3, 3))$parameter, c(2, 2))
  expect_equal(
    bounded$minimize(objective, c(3, 3), lower = 1.5)$parameter, c(1.5, 1.5)
  )
})

test_that("optimizer fields can be accessed", {
  opt <- Optimizer$new("stats::nlm")
  expect_equal(opt$label, "stats::nlm")
  expect_identical(opt$algorithm, stats::nlm)
  expect_equal(opt$arg_objective, "f")
  expect_equal(opt$arg_initial, "p")
  expect_identical(opt$arg_lower, NA)
  expect_identical(opt$arg_upper, NA)
  expect_equal(opt$arg_gradient, "gradient")
  expect_equal(opt$arg_hessian, "hessian")
  expect_true(opt$gradient_as_attribute)
  expect_true(opt$hessian_as_attribute)
  expect_equal(opt$out_value, "minimum")
  expect_equal(opt$out_parameter, "estimate")
  expect_equal(opt$direction, "min")
  expect_identical(opt$arguments, list())
  expect_identical(opt$seconds, Inf)
  expect_false(opt$hide_warnings)
  expect_identical(opt$output_ignore, character())

  ### input checks
  expect_error(opt$label <- 1, "Input `label` is bad")
  expect_error(opt$algorithm <- "nlm", "Input `algorithm` is bad")
  expect_error(opt$arg_objective <- NA, "Input `arg_objective` is bad")
  expect_error(opt$arg_initial <- 1, "Input `arg_initial` is bad")
  expect_error(opt$out_value <- NULL, "Input `out_value` is bad")
  expect_error(opt$out_parameter <- 1:2, "Input `out_parameter` is bad")
  expect_error(opt$direction <- "up", "Input `direction` is bad")
  expect_error(opt$arguments <- list(1), "Input `arguments` is bad")
  expect_error(opt$seconds <- -1, "Input `seconds` is bad")
  expect_error(opt$hide_warnings <- "yes", "Input `hide_warnings` is bad")
  expect_error(opt$output_ignore <- 1, "Input `output_ignore` is bad")

  ### ignore output elements
  opt$output_ignore <- c("gradient", "code")
  expect_named(
    opt$minimize(function(x) sum(x^2), c(3, 3)),
    c("value", "parameter", "seconds", "initial", "error", "iterations")
  )
})

test_that("input checks for optimization work", {
  opt <- Optimizer$new("stats::optim", method = "L-BFGS-B")
  f <- function(x) sum(x^2)
  objective <- Objective$new(f, npar = 2)
  expect_error(
    opt$minimize(initial = 1:2),
    "Input `objective` is bad: Argument needs a value"
  )
  expect_error(
    opt$maximize(f),
    "Input `initial` is bad: Argument needs a value"
  )
  expect_error(
    opt$minimize(1, 1:2),
    "Input `objective` is bad: Must be a function or an object of class"
  )
  expect_error(
    opt$minimize(f, "a"),
    "Input `initial` is bad: Must be of type 'numeric'"
  )
  expect_error(
    opt$minimize(objective, "a"),
    "Input `initial` is bad: Must be of type 'numeric'"
  )
  expect_error(
    opt$minimize(f, c(1, NA)),
    "Input `initial` is bad: Contains missing values"
  )
  expect_error(
    opt$minimize(f, numeric()),
    "Input `initial` is bad: Must have length >= 1"
  )
  expect_error(
    opt$minimize(objective, 1:3),
    "Input `initial` is bad: Must have length 2"
  )
  expect_error(
    opt$minimize(f, 1:2, lower = 1:3),
    "Input `lower` is bad: Must have length 2"
  )
  expect_error(
    opt$minimize(f, 1:2, upper = c(NA, 1)),
    "Input `upper` is bad: Contains missing values"
  )
  expect_error(
    opt$optimize(f, 1:2, direction = "up"),
    "Input `direction` is bad"
  )
  expect_error(
    opt$minimize(f, 1:2, 0, 5, 10),
    "Input `...` is bad: Must have names"
  )
})

test_that("simple minimization works", {
  objective <- function(x) x^2
  opt_nlm <- Optimizer$new("stats::nlm")
  expect_snapshot(
    print(opt_nlm)
  )
  out <- opt_nlm$minimize(objective, initial = 2)
  expect_type(
    out,
    "list"
  )
  expect_named(
    out,
    c(
      "value", "parameter", "seconds", "initial",
      "error", "gradient", "code", "iterations"
    )
  )
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, 0, tolerance = 1e-3)
  expect_true(out$seconds >= 0)
  expect_equal(out$initial, 2)
  expect_false(out$error)
  expect_identical(
    capture.output({
      print(opt_nlm)
      cat("next line")
    }),
    c("<optimizer 'stats::nlm'>", "next line")
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
    c(
      "value", "parameter", "seconds", "initial",
      "error", "gradient", "code", "iterations"
    )
  )
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, 0, tolerance = 1e-3)
})

test_that("optimization with one and more parameters works", {
  objective <- function(x) (x - 1)^2
  rosenbrock <- function(x, shift = 0) {
    100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 + shift
  }
  for (which in c("stats::nlm", "stats::nlminb", "stats::optim")) {
    opt <- Optimizer$new(which)
    opt$hide_warnings <- TRUE
    out <- opt$minimize(objective, 3)
    expect_false(out$error)
    expect_equal(out$value, 0, tolerance = 1e-6)
    expect_equal(out$parameter, 1, tolerance = 1e-3)
    out <- opt$optimize(function(x) -objective(x), 3, direction = "max")
    expect_equal(out$value, 0, tolerance = 1e-6)
    expect_equal(out$parameter, 1, tolerance = 1e-3)
    out <- opt$minimize(rosenbrock, c(-1.2, 1), shift = 2)
    expect_equal(out$value, 2, tolerance = 1e-6)
    expect_equal(out$parameter, c(1, 1), tolerance = 1e-2)
    out <- opt$maximize(function(x) -rosenbrock(x), c(a = -1.2, b = 1))
    expect_equal(out$value, 0, tolerance = 1e-6)
    expect_equal(out$initial, c(a = -1.2, b = 1))
  }
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
    c(
      "value", "parameter", "seconds", "initial",
      "error", "gradient", "code", "iterations"
    )
  )
  expect_equal(out$value, 10, tolerance = 1e-6)

  ### additional arguments have priority over fixed arguments
  objective_object <- Objective$new(objective, npar = 1, a = 3)
  opt_nlm <- Optimizer$new("stats::nlm")
  expect_equal(
    opt_nlm$minimize(objective_object, initial = 2)$value, 3,
    tolerance = 1e-6
  )
  expect_equal(
    opt_nlm$minimize(objective_object, initial = 2, a = 10)$value, 10,
    tolerance = 1e-6
  )
})

test_that("call-time arguments are passed only to the objective", {
  seen <- new.env(parent = emptyenv())
  objective <- function(x, control, for_f) {
    seen$control <- control
    seen$for_f <- for_f
    sum(x^2)
  }

  expect_warning(
    out <- Optimizer$new("stats::optim")$minimize(
      objective = objective,
      initial = 1:3,
      control = list("something" = 1),
      for_f = "hi"
    ),
    NA
  )
  expect_false(out$error)
  expect_identical(seen$control, list("something" = 1))
  expect_identical(seen$for_f, "hi")
})

test_that("fixed argument that is NULL can be passed", {
  f <- function(x, a, b, ind) {
    if (is.null(ind)) {
      (x[1]^2 + x[2] + a)^2 + (x[1] + x[2]^2 + b)^2 + (x[3] - 1)^2
    }
  }
  expect_false(
    Optimizer$new("stats::nlm")$minimize(
      objective = f,
      initial = c(0, 0, 0),
      a = -11, b = -7, ind = NULL
    )$error
  )
})

test_that("argument that is an expression can be passed", {
  f <- function(x, expr) {
    if (is.call(expr)) sum(x^2) else stop("expression was evaluated")
  }
  out <- Optimizer$new("stats::nlm")$
    minimize(objective = f, initial = c(1, 1), expr = quote(a + b))
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
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
  out <- optim_opt$minimize(
    objective = himmelblau_modified,
    initial = c(-10, -10),
    lower = NA,
    upper = NA_real_
  )
  expect_equal(out$value, -100)

  ### bounds on one side, as vector, and for one parameter
  shifted <- function(x) sum((x - 1)^2)
  out <- optim_opt$minimize(shifted, c(3, 3), lower = c(2, -Inf))
  expect_equal(out$parameter, c(2, 1), tolerance = 1e-4)
  out <- optim_opt$minimize(shifted, 0, lower = NA, upper = 0.5)
  expect_equal(out$parameter, 0.5)
  out <- optim_opt$maximize(function(x) -shifted(x), 0, upper = 0.5)
  expect_equal(out$parameter, 0.5)
  expect_equal(out$value, -0.25)

  ### warning if optimizer does not support bounds
  nlm_opt <- Optimizer$new("stats::nlm")
  expect_warning(
    nlm_opt$minimize(
      objective = himmelblau_modified,
      initial = c(-10, -10),
      lower = -5
    ),
    "The optimizer does not support lower parameter bounds."
  )
  expect_warning(
    nlm_opt$minimize(
      objective = himmelblau_modified,
      initial = c(-10, -10),
      upper = -5
    ),
    "The optimizer does not support upper parameter bounds."
  )

  ### no warning if warnings are hidden
  nlm_opt$hide_warnings <- TRUE
  expect_warning(
    out <- nlm_opt$minimize(shifted, c(3, 3), lower = 2, upper = 5),
    NA
  )
  expect_equal(out$parameter, c(1, 1), tolerance = 1e-4)

})

test_that("warnings during optimization can be hidden", {
  objective <- function(x) {
    warning("careful")
    sum(x^2)
  }
  opt <- Optimizer$new("custom", .verbose = FALSE)$
    definition(
      algorithm = function(f, p, ...) list(v = f(p), z = p),
      arg_objective = "f",
      arg_initial = "p",
      out_value = "v",
      out_parameter = "z",
      direction = "min"
    )
  expect_warning(
    opt$minimize(objective, c(1, 1)),
    "careful"
  )
  opt$hide_warnings <- TRUE
  expect_warning(
    out <- opt$minimize(objective, c(1, 1)),
    NA
  )
  expect_equal(out$value, 2)
})

test_that("gradient and hessian can be used", {

  ### objective definition with gradient and Hessian
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  himmelblau_gradient <- function(x) {
    # warning("gradient is used")
    c(
      4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
      2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
    )
  }
  himmelblau_hessian <- function(x) {
    # warning("hessian is used")
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
  expect_equal(out$value, 0, tolerance = 1e-6)

  ### using gradient and Hessian via argument
  nlminb_opt <- Optimizer$new("stats::nlminb")
  out <- nlminb_opt$optimize(
    objective = himmelblau_objective,
    initial = c(1, 2)
  )
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)

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
  expect_equal(out$value, 0, tolerance = 1e-6)

  ### using gradient and Hessian for maximization
  calls <- new.env(parent = emptyenv())
  himmelblau_negative <- Objective$
    new(f = function(x) -himmelblau(x), npar = 2)$
    set_gradient(
      function(x) {
        calls$gradient <- calls$gradient + 1
        -himmelblau_gradient(x)
      },
      .verbose = FALSE
    )$
    set_hessian(
      function(x) {
        calls$hessian <- calls$hessian + 1
        -himmelblau_hessian(x)
      },
      .verbose = FALSE
    )
  for (which in c("stats::nlm", "stats::nlminb")) {
    calls$gradient <- calls$hessian <- 0
    out <- Optimizer$new(which)$maximize(himmelblau_negative, c(1, 2))
    expect_false(out$error)
    expect_equal(out$value, 0, tolerance = 1e-6)
    expect_equal(out$parameter, c(3, 2), tolerance = 1e-3)
    expect_true(calls$gradient > 0)
    expect_true(calls$hessian > 0)
  }
})

test_that("gradient and hessian can be used for one parameter", {
  quartic <- function(x) (x - 1)^4 + (x - 1)^2
  quartic_gradient <- function(x) 4 * (x - 1)^3 + 2 * (x - 1)
  quartic_hessians <- list(
    "number" = function(x) 12 * (x - 1)^2 + 2,
    "matrix" = function(x) matrix(12 * (x - 1)^2 + 2)
  )
  for (quartic_hessian in quartic_hessians) {
    calls <- new.env(parent = emptyenv())
    objective <- Objective$
      new(f = quartic, npar = 1)$
      set_gradient(quartic_gradient, .verbose = FALSE)$
      set_hessian(
        function(x) {
          calls$hessian <- TRUE
          quartic_hessian(x)
        },
        .verbose = FALSE
      )
    objective_negative <- Objective$
      new(f = function(x) -quartic(x), npar = 1)$
      set_gradient(function(x) -quartic_gradient(x), .verbose = FALSE)$
      set_hessian(function(x) -quartic_hessian(x), .verbose = FALSE)
    for (which in c("stats::nlm", "stats::nlminb")) {
      opt <- Optimizer$new(which)
      calls$hessian <- FALSE
      out <- opt$minimize(objective, 3)
      expect_false(out$error)
      expect_true(calls$hessian)
      expect_equal(out$value, 0, tolerance = 1e-6)
      expect_equal(out$parameter, 1, tolerance = 1e-3)
      out <- opt$maximize(objective_negative, 3)
      expect_false(out$error)
      expect_equal(out$value, 0, tolerance = 1e-6)
      expect_equal(out$parameter, 1, tolerance = 1e-3)
    }
  }
})

test_that("gradient as attribute can be used for maximization", {
  objective <- function(x) {
    structure(-sum((x - 1)^2), gradient = -2 * (x - 1))
  }
  out <- Optimizer$new("stats::nlm")$
    maximize(objective = objective, initial = c(5, -3))
  expect_false(out$error)
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 1), tolerance = 1e-4)
})
