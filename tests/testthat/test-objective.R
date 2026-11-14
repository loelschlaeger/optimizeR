# self <- objective
# private <- self$.__enclos_env__$private

cli::test_that_cli("test various cli messages", {

   ### objective definition
   f <- function(x, a, b = 0) (x + a)^2 + b
   f_gradient <- function(x, a, b = 0) 2 * x + 2 * a
   f_hessian <- function(x, a, b = 0) 2
   objective <- Objective$new(f = f, npar = 1)
   objective$verbose <- TRUE

   ### setting argument
   expect_snapshot(
     objective$set_argument("a" = -2)
   )

   ### overwriting argument
   expect_error(
     objective$set_argument("a" = -3, .overwrite = FALSE),
     "already exists"
   )
   expect_snapshot(
     objective$set_argument("a" = -3)
   )

   ### getting argument
   expect_snapshot(
     objective$get_argument("a")
   )

   ### remove argument
   expect_snapshot(
     objective$remove_argument("a")
   )

   ### set argument again
   expect_snapshot(
     objective$set_argument("a" = -2)
   )

   ### set gradient
   expect_snapshot(
     objective$set_gradient(f_gradient)
   )

   ### set gradient
   expect_snapshot(
     objective$set_hessian(f_hessian)
   )

   ### synchronizing arguments for gradient and Hessian
   objective$remove_argument("a", .verbose = FALSE)
   expect_snapshot(
     objective$set_argument("a" = 1)
   )

})

test_that("objective fields can be accessed", {

  ### objective definition
  f <- function(x, a, b = 0) (x + a)^2 + b
  f_gradient <- function(x, a, b = 0) 2 * x + 2 * a
  f_hessian <- function(x, a, b = 0) 2
  objective <- Objective$new(f = f, npar = 1, "a" = 2)
  objective$verbose <- FALSE
  objective$
    set_gradient(f_gradient)$
    set_hessian(f_hessian)

  ### objective_name
  expect_equal(
    objective$objective_name,
    "f"
  )

  ### fixed_arguments
  expect_error(
    objective$fixed_arguments <- c("this", "that"),
    "read-only"
  )

  ### hide_warnings
  expect_false(
    objective$hide_warnings
  )
  objective$hide_warnings <- TRUE
  expect_true(
    objective$hide_warnings
  )

  ### npar
  expect_error(
    objective$npar <- 1,
    "read-only"
  )

  ### target
  expect_error(
    objective$target <- "y",
    "read-only"
  )

  ### gradient specified
  expect_true(
    objective$gradient_specified
  )
  expect_error(
    objective$gradient_specified <- FALSE,
    "read-only"
  )

  ### hessian specified
  expect_true(
    objective$hessian_specified
  )
  expect_error(
    objective$hessian_specified <- FALSE,
    "read-only"
  )

})

test_that("objective with one target argument can be evaluated", {

  ### objective definition
  f <- function(x, a, b = 0) (x + a)^2 + b
  objective <- Objective$new(f = f, npar = 1, a = -2)
  objective$verbose <- FALSE

  ### evaluations
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
    "is required but not specified yet"
  )
  expect_silent(
    objective$set_argument("a" = -2)
  )
  expect_error(
    objective$set_argument("a" = -2, .overwrite = FALSE),
    "already exists"
  )
  expect_silent(
    objective$set_argument("a" = -2, .overwrite = TRUE)
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
  expect_error(
    objective$evaluate_gradient(2),
    "Gradient function is required"
  )
  expect_error(
    objective$evaluate_hessian(2),
    "Hessian function is required"
  )
})

test_that("objective can be evaluated with a time limit", {
  f <- function(x, a, b = 0) {
    end <- proc.time()[["elapsed"]] + 2
    while (proc.time()[["elapsed"]] < end) NULL
    (x + a)^2 + b
  }
  objective <- Objective$new(f = f, npar = 1, a = -2)
  objective$verbose <- FALSE
  objective$seconds <- 0.5
  expect_equal(
    objective$evaluate(1),
    "time limit reached"
  )
  expect_equal(
    objective$evaluate(1, .negate = TRUE),
    "time limit reached"
  )
  expect_equal(time_limit$deadline, Inf)

  ### time limit does not depend on the language
  local_reproducible_output(lang = "de")
  expect_equal(
    objective$evaluate(1),
    "time limit reached"
  )

  ### time limit is not reached
  objective <- Objective$new(f = function(x) sum(x), npar = 1)
  objective$seconds <- 10
  expect_equal(
    objective$evaluate(1),
    1
  )
  objective$seconds <- Inf
  expect_equal(
    objective$evaluate(1),
    1
  )
})

test_that("objective with more than one target argument can be evaluated", {
  llk <- function(mu, sd, lambda, data) {
    sd <- exp(sd)
    lambda <- plogis(lambda)
    sum(log(
      lambda * dnorm(data, mu[1], sd[1]) +
        (1 - lambda) * dnorm(data, mu[2], sd[2])
    ))
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
  expect_snapshot(
    print(obj)
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

test_that("gradient and hessian can be specified and evaluated", {

  ### define objective
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  himmelblau_objective <- Objective$new(f = himmelblau, npar = 2)
  himmelblau_objective$verbose <- FALSE
  himmelblau_gradient <- function(x) {
    c(
      4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
      2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
    )
  }
  himmelblau_objective$set_gradient(himmelblau_gradient)

  ### evaluate
  expect_equal(
    himmelblau_objective$evaluate_gradient(c(3, 2)),
    himmelblau_gradient(c(3, 2))
  )
  himmelblau_hessian <- function(x) {
    matrix(
      c(
        12 * x[1]^2 + 4 * x[2] - 42, 4 * x[1] + 4 * x[2],
        4 * x[1] + 4 * x[2], 12 * x[2]^2 + 4 * x[1] - 26
      ),
      nrow = 2
    )
  }
  himmelblau_objective$set_hessian(himmelblau_hessian)
  expect_equal(
    himmelblau_objective$evaluate_hessian(c(3, 2)),
    himmelblau_hessian(c(3, 2))
  )

  ### values as attributes
  expect_identical(
    himmelblau_objective$evaluate(
      .at = c(1, 2),
      .negate = TRUE,
      .gradient_as_attribute = TRUE,
      .gradient_attribute_name = "gradient",
      .hessian_as_attribute = TRUE,
      .hessian_attribute_name = "hessian"
    ),
    structure(
      -68,
      gradient = c(36, 32),
      hessian = structure(c(22, -12, -12, -26), dim = c(2L, 2L))
    )
  )
})

test_that("numerical gradient and hessian can be calculated", {
  himmelblau <- function(x, a, b) (x[1]^2 + x[2] - a)^2 + (x[1] + x[2]^2 - b)^2
  himmelblau_gradient <- function(x, a, b) {
    c(
      4 * x[1] * (x[1]^2 + x[2] - a) + 2 * (x[1] + x[2]^2 - b),
      2 * (x[1]^2 + x[2] - a) + 4 * x[2] * (x[1] + x[2]^2 - b)
    )
  }
  himmelblau_hessian <- function(x, a, b) {
    matrix(
      c(
        12 * x[1]^2 + 4 * x[2] - 4 * a + 2, 4 * x[1] + 4 * x[2],
        4 * x[1] + 4 * x[2], 12 * x[2]^2 + 4 * x[1] - 4 * b + 2
      ),
      nrow = 2
    )
  }
  objective <- Objective$new(f = himmelblau, npar = 2, a = 11, b = 7)
  expect_equal(
    objective$evaluate_gradient_numeric(1:2),
    himmelblau_gradient(1:2, a = 11, b = 7)
  )
  expect_equal(
    objective$evaluate_hessian_numeric(1:2),
    himmelblau_hessian(1:2, a = 11, b = 7)
  )
})

test_that("invalid numerical derivatives return NA structures", {
  normal_mixture_llk <- function(mu, sigma, lambda, data) {
    sigma <- exp(sigma)
    lambda <- plogis(lambda)
    sum(log(
      lambda * dnorm(data, mu[1], sigma[1]) +
        (1 - lambda) * dnorm(data, mu[2], sigma[2])
    ))
  }
  objective <- Objective$new(
    f = normal_mixture_llk,
    target = c("mu", "sigma", "lambda"),
    npar = c(2, 2, 1),
    data = datasets::faithful$eruptions
  )
  bad <- c(-2L, -3L, -5L, -3L, -1L)

  expect_identical(
    objective$evaluate_gradient_numeric(.at = bad),
    rep(NA_real_, length(bad))
  )
  expect_identical(
    objective$evaluate_hessian_numeric(.at = bad),
    matrix(NA_real_, nrow = length(bad), ncol = length(bad))
  )
})

test_that("input checks for objective work", {
  f <- function(x, y) sum(x) + sum(y)
  expect_error(
    Objective$new(npar = 1),
    "Input `f` is bad: Argument needs a value"
  )
  expect_error(
    Objective$new(f = 1, npar = 1),
    "Input `f` is bad: Must be a function"
  )
  expect_error(
    Objective$new(f = f, target = "z", npar = 1),
    "Input `f` is bad: Must have formal arguments"
  )
  expect_error(
    Objective$new(f = f),
    "Input `npar` is bad: Argument needs a value"
  )
  expect_error(
    Objective$new(f = f, npar = 1.5),
    "Input `npar` is bad: Must be of type 'integerish'"
  )
  expect_error(
    Objective$new(f = f, target = c("x", "y"), npar = 1),
    "Input `npar` is bad: Must have length 2"
  )
  expect_error(
    Objective$new(f = f, npar = 0),
    "Input `npar` is bad"
  )
  objective <- Objective$new(f = f, npar = 2, y = 1)
  expect_error(
    objective$evaluate(1),
    "Input `.at` is bad: Must have length 2"
  )
  expect_error(
    objective$evaluate(c(1, NA)),
    "Input `.at` is bad: Contains missing values"
  )
  expect_error(
    objective$seconds <- "a",
    "Input `seconds` is bad"
  )
  expect_error(
    objective$hide_warnings <- 1,
    "Input `hide_warnings` is bad"
  )
  expect_error(
    objective$objective_name <- 1,
    "Input `objective_name` is bad"
  )
})

test_that("target argument cannot be fixed", {
  f <- function(x, a) sum(x) + a
  expect_error(
    Objective$new(f = f, npar = 2, x = c(1, 1), a = 1),
    "Input `...` is bad: Must not contain a target argument"
  )
  objective <- Objective$new(f = f, npar = 2, a = 1)
  expect_error(
    objective$set_argument(x = c(1, 1)),
    "Input `...` is bad: Must not contain a target argument"
  )
  expect_equal(
    objective$fixed_arguments,
    "a"
  )
  expect_equal(
    objective$evaluate(c(1, 1)),
    3
  )
})

test_that("arguments are only overwritten on request", {
  f <- function(x, a, b) x + a + b
  objective <- Objective$new(f = f, npar = 1, b = 1)
  expect_error(
    objective$set_argument(a = 2, b = 3, .overwrite = FALSE),
    "Argument `b` already exists"
  )
  expect_equal(
    objective$fixed_arguments,
    "b"
  )
  expect_equal(
    objective$get_argument("b"),
    1
  )
  objective$set_argument(a = 2, .overwrite = FALSE)
  expect_error(
    objective$set_argument(a = 4, b = 3, .overwrite = FALSE),
    "Arguments `a` and `b` already exist"
  )
  objective$set_argument(a = 4, b = 3)
  expect_equal(
    objective$evaluate(1),
    8
  )
})

test_that("objective can be cloned", {
  objective <- Objective$new(f = function(x, a) (x - a)^2, npar = 1, a = 1)
  objective$set_gradient(function(x, a) 2 * (x - a))
  objective_clone <- objective$clone(deep = TRUE)
  objective_clone$set_argument(a = 5)
  expect_equal(
    objective_clone$evaluate_gradient(0),
    -10
  )
  expect_equal(
    objective$evaluate_gradient(0),
    -2
  )
})

test_that("objective with default arguments can be evaluated", {

  ### default value for target argument
  objective <- Objective$new(
    f = function(x = c(0, 0), a = 1) sum(x) + a, npar = 2
  )
  expect_equal(
    objective$fixed_arguments,
    "a"
  )
  expect_equal(
    objective$evaluate(c(1, 2)),
    4
  )
  out <- Optimizer$new("stats::nlm")$
    minimize(objective = function(x = 0) (x - 1)^2, initial = 3)
  expect_false(out$error)
  expect_equal(out$parameter, 1, tolerance = 1e-4)

  ### default value that is an expression
  objective <- Objective$new(
    f = function(x, shift = c(1, 2), scale = -1) sum((x - shift)^2) * scale,
    npar = 2
  )
  expect_equal(
    objective$get_argument("shift"),
    c(1, 2)
  )
  expect_equal(
    objective$get_argument("scale"),
    -1
  )
  expect_equal(
    objective$evaluate(c(1, 0)),
    -4
  )

  ### default value that depends on another argument
  x <- 1:10
  objective <- Objective$new(
    f = function(x, n = length(x), a) sum(x) / n + a, npar = 2, a = 1
  )
  expect_equal(
    objective$fixed_arguments,
    "a"
  )
  expect_equal(
    objective$evaluate(c(1, 3)),
    3
  )
  expect_equal(
    objective$evaluate(c(1, 3), n = 4),
    2
  )
  expect_error(
    objective$get_argument("n"),
    "is required but not specified yet"
  )

  ### default value from the environment of the function
  build_f <- function() {
    offset <- 5
    function(x, a = offset) sum(x) + a
  }
  objective <- Objective$new(f = build_f(), npar = 1)
  expect_equal(
    objective$get_argument("a"),
    5
  )
  expect_equal(
    objective$evaluate(1),
    6
  )
})

test_that("objective with expression argument can be evaluated", {
  f <- function(x, expr, label = NULL) {
    if (is.call(expr)) sum(x) else stop("expression was evaluated")
  }
  objective <- Objective$new(f = f, npar = 2, expr = quote(a + b))
  expect_identical(
    objective$get_argument("expr"),
    quote(a + b)
  )
  expect_equal(
    objective$evaluate(c(1, 2)),
    3
  )
  expect_equal(
    objective$evaluate(c(1, 2), expr = quote(c * d)),
    3
  )
  objective$set_argument(expr = quote(e / f))
  expect_equal(
    objective$evaluate(c(1, 2)),
    3
  )
  objective$set_gradient(function(x, expr) {
    if (is.call(expr)) rep(1, 2) else stop("expression was evaluated")
  })
  expect_equal(
    objective$evaluate_gradient(c(1, 2)),
    c(1, 1)
  )
  expect_equal(
    objective$evaluate_gradient_numeric(c(1, 2)),
    c(1, 1)
  )
})

test_that("errors of objective can be detected", {
  objective <- Objective$new(
    f = function(x) stop("bad {value} with {.emph braces}"), npar = 1
  )
  expect_error(
    objective$evaluate(1),
    "Function evaluation threw an error: bad {value} with {.emph braces}",
    fixed = TRUE
  )
  out <- Optimizer$new("stats::nlm")$minimize(objective, 1)
  expect_equal(
    out$error_message,
    "Function evaluation threw an error: bad {value} with {.emph braces}"
  )
})

test_that("warnings of objective can be hidden", {
  f <- function(x) {
    warning("careful")
    sum(x)
  }
  objective <- Objective$new(f = f, npar = 1)
  expect_warning(
    objective$evaluate(1),
    "careful"
  )
  objective$hide_warnings <- TRUE
  expect_warning(
    out <- objective$evaluate(1),
    NA
  )
  expect_equal(out, 1)
})

test_that("objective can be printed", {
  objective <- Objective$new(f = function(x) x, npar = 1)
  expect_output(
    out <- print(objective, digits = 3),
    "Targets"
  )
  expect_identical(
    out,
    objective
  )
})

test_that("arguments are synchronized with gradient and hessian", {
  f <- function(x, a) sum((x - a)^2) * a
  f_gradient <- function(x, a) 2 * (x - a) * a
  f_hessian <- function(x, a) diag(2 * a, 2)

  ### only gradient specified
  objective <- Objective$new(f = f, npar = 2, a = 1)
  objective$set_gradient(f_gradient)
  objective$set_argument(a = 2)
  expect_true(
    objective$gradient_specified
  )
  expect_false(
    objective$hessian_specified
  )
  expect_equal(
    objective$evaluate_gradient(c(0, 0)),
    c(-8, -8)
  )
  expect_error(
    objective$evaluate_hessian(c(0, 0)),
    "Hessian function is required"
  )
  out <- Optimizer$new("stats::nlminb")$minimize(objective, c(0, 0))
  expect_false(out$error)
  expect_equal(out$parameter, c(2, 2), tolerance = 1e-4)

  ### only Hessian specified
  objective <- Objective$new(f = f, npar = 2, a = 1)
  objective$set_hessian(f_hessian)
  objective$set_argument(a = 3)
  expect_false(
    objective$gradient_specified
  )
  expect_identical(
    objective$evaluate_hessian(c(0, 0)),
    diag(6, 2)
  )
  objective$remove_argument("a")
  expect_identical(
    objective$evaluate_hessian(c(0, 0), a = 4),
    diag(8, 2)
  )
})

test_that("input checks for gradient and hessian work", {
  objective <- Objective$new(f = function(x, a) sum(x^2) + a, npar = 2, a = 1)
  expect_error(
    objective$set_gradient(),
    "Input `gradient` is bad: Argument needs a value"
  )
  expect_error(
    objective$set_gradient(1),
    "Input `gradient` is bad: Must be a function"
  )
  expect_error(
    objective$set_gradient(function(y, a) 2 * y),
    "Input `gradient` is bad: Must have formal arguments"
  )
  expect_error(
    objective$set_hessian(),
    "Input `hessian` is bad: Argument needs a value"
  )
  expect_error(
    objective$set_hessian("hessian"),
    "Input `hessian` is bad: Must be a function"
  )
  expect_false(
    objective$gradient_specified
  )
  expect_false(
    objective$hessian_specified
  )
})

test_that("gradient and hessian with fewer arguments can be evaluated", {
  f <- function(x, a, b = 0) sum((x - a)^2) + b
  objective <- Objective$new(f = f, npar = 2, a = 1)
  objective$set_gradient(function(x, a) 2 * (x - a))
  objective$set_hessian(function(x) diag(2, 2))
  expect_equal(
    objective$evaluate_gradient(c(0, 0)),
    c(-2, -2)
  )
  expect_identical(
    objective$evaluate_hessian(c(0, 0)),
    diag(2, 2)
  )
  expect_equal(
    objective$evaluate_gradient(c(0, 0), b = 5),
    c(-2, -2)
  )
  objective$set_argument(b = 3)
  expect_equal(
    objective$evaluate_gradient(c(0, 0)),
    c(-2, -2)
  )
  out <- Optimizer$new("stats::nlminb")$minimize(objective, c(0, 0), a = 2)
  expect_false(out$error)
  expect_equal(out$parameter, c(2, 2), tolerance = 1e-6)
  expect_equal(out$value, 3, tolerance = 1e-6)

  ### all arguments via '...'
  objective$set_gradient(function(x, ...) 2 * (x - list(...)$a))
  expect_equal(
    objective$evaluate_gradient(c(0, 0)),
    c(-2, -2)
  )

  ### unknown argument for the function
  objective$set_argument(unknown = 1)
  expect_error(
    objective$evaluate(c(0, 0)),
    "Function evaluation threw an error"
  )
})

test_that("invalid derivatives return NA structures", {
  objective <- Objective$new(f = function(x) sum(x^2), npar = 2)
  objective$set_gradient(function(x) stop("no gradient"))
  objective$set_hessian(function(x) stop("no Hessian"))
  expect_identical(
    objective$evaluate_gradient(c(1, 2)),
    c(NA_real_, NA_real_)
  )
  expect_identical(
    objective$evaluate_hessian(c(1, 2)),
    matrix(NA_real_, nrow = 2, ncol = 2)
  )

  ### wrong format
  objective$set_gradient(function(x) 2 * x[1])
  objective$set_hessian(function(x) diag(2, 3))
  expect_identical(
    objective$evaluate_gradient(c(1, 2)),
    c(NA_real_, NA_real_)
  )
  expect_identical(
    objective$evaluate_hessian(c(1, 2)),
    matrix(NA_real_, nrow = 2, ncol = 2)
  )

  ### Hessian as number or matrix for one parameter
  objective <- Objective$new(f = function(x) x^2, npar = 1)
  objective$set_gradient(function(x) 2 * x)
  objective$set_hessian(function(x) 2)
  expect_equal(
    objective$evaluate_gradient(3),
    6
  )
  expect_equal(
    objective$evaluate_hessian(3),
    2
  )
  objective$set_hessian(function(x) matrix(2))
  expect_identical(
    objective$evaluate_hessian(3),
    matrix(2)
  )
  expect_identical(
    objective$evaluate_hessian(3, .negate = TRUE),
    matrix(-2)
  )
  objective$set_hessian(function(x) "a")
  expect_identical(
    objective$evaluate_hessian(3),
    NA_real_
  )
  expect_equal(
    objective$evaluate_gradient_numeric(3),
    6
  )
  expect_equal(
    objective$evaluate_hessian_numeric(3),
    matrix(2)
  )
})

test_that("derivatives as attributes can be negated", {
  f <- function(x) {
    structure(sum(x^2), gradient = 2 * x, hessian = diag(2, 2), note = "a")
  }
  objective <- Objective$new(f = f, npar = 2)
  expect_identical(
    objective$evaluate(c(1, 2)),
    structure(5, gradient = c(2, 4), hessian = diag(2, 2), note = "a")
  )
  expect_identical(
    objective$evaluate(c(1, 2), .negate = TRUE),
    structure(-5, gradient = c(-2, -4), hessian = diag(-2, 2), note = "a")
  )

  ### other attribute names
  objective_renamed <- Objective$new(
    f = function(x) structure(sum(x^2), g = 2 * x, gradient = 2 * x),
    npar = 2
  )
  expect_identical(
    objective_renamed$evaluate(
      c(1, 2), .negate = TRUE, .gradient_attribute_name = "g",
      .hessian_attribute_name = NA_character_
    ),
    structure(-5, g = c(-2, -4), gradient = c(2, 4))
  )

  ### numerical derivatives
  expect_equal(
    objective$evaluate(
      c(1, 2), .negate = TRUE, .gradient_as_attribute = TRUE,
      .gradient_numeric = TRUE, .hessian_as_attribute = TRUE,
      .hessian_numeric = TRUE
    ),
    structure(-5, gradient = c(-2, -4), hessian = diag(-2, 2), note = "a")
  )
})
