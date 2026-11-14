test_that("optimizer constructor works", {
  rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  opt_nlm <- optimizer("stats::nlm", iterlim = 200)
  checkmate::expect_r6(opt_nlm, "Optimizer")
  expect_equal(opt_nlm$label, "stats::nlm")
  expect_equal(opt_nlm$arguments, list(iterlim = 200))
  out <- opt_nlm$minimize(rosenbrock, initial = c(-1.2, 1))
  expect_equal(out$value, 0, tolerance = 1e-6)
  expect_equal(out$parameter, c(1, 1), tolerance = 1e-3)
  expect_equal(
    out[c("value", "parameter")],
    Optimizer$new("stats::nlm", iterlim = 200)$
      minimize(rosenbrock, initial = c(-1.2, 1))[c("value", "parameter")]
  )

  ### custom optimizer
  expect_message(
    optimizer("custom"),
    "to define a custom optimizer"
  )
  expect_silent(
    opt <- optimizer("custom", .verbose = FALSE)
  )
  checkmate::expect_r6(opt, "Optimizer")
  expect_equal(opt$label, "unlabeled")

  ### input checks
  expect_error(
    optimizer(),
    "Input `which` is bad: Argument needs a value"
  )
  expect_error(
    optimizer("not an optimizer"),
    "Input `which` is bad"
  )
  expect_error(
    optimizer(1),
    "Input `which` is bad"
  )
})

test_that("objective constructor works", {
  f <- function(x, shift) sum((x - shift)^2)
  obj <- objective(f, npar = 2, shift = 1)
  checkmate::expect_r6(obj, "Objective")
  expect_equal(obj$evaluate(c(1, 3)), 4)
  expect_equal(obj$fixed_arguments, "shift")
  expect_equal(obj$objective_name, "f")
  expect_equal(Objective$new(f, npar = 2)$objective_name, "f")

  ### target arguments
  obj <- objective(
    function(a, x, y) a * sum(x) + sum(y), target = c("x", "y"),
    npar = c(2, 1), a = 2
  )
  expect_equal(obj$target, c("x", "y"))
  expect_equal(obj$evaluate(1:3), 9)

  ### input checks
  expect_error(
    objective(npar = 1),
    "Input `f` is bad: Argument needs a value"
  )
  expect_error(
    objective(f),
    "Input `npar` is bad: Argument needs a value"
  )
})
