test_that("construct optimizer works", {
  expect_error(
    define_optimizer(),
    "specify argument"
  )
  expect_error(
    define_optimizer(optimizer = stats::nlm),
    "specify argument"
  )
  expect_error(
    define_optimizer(optimizer = stats::nlm, objective = "f"),
    "specify argument"
  )
  expect_error(
    define_optimizer(optimizer = stats::nlm, objective = "f", initial = "p"),
    "specify argument"
  )
  expect_error(
    define_optimizer(
      optimizer = stats::nlm, objective = "f", initial = "p", value = "v"
    ),
    "specify argument"
  )
  expect_error(
    define_optimizer(
      optimizer = stats::nlm, objective = "f", initial = "p", parameter = "z"
    ),
    "specify argument"
  )
  optimizer <- new_optimizer(
    optimizer = pracma::nelder_mead,
    optimizer_name = "nelder_mead",
    optimizer_add = list("tol" = 1e-10),
    objective = "fn",
    initial = "x0",
    value = "fmin",
    parameter = "xmin",
    output_ignore = "restarts"
  )
  expect_s3_class(optimizer, "optimizer")
  optimizer <- validate_optimizer(optimizer)
  expect_s3_class(optimizer, "optimizer")
})

test_that("optimizier validation works", {
  expect_error(
    define_optimizer(
      optimizer = function(fn, x) stop("error"),
      objective = "fn",
      initial = "x",
      value = "fmin",
      parameter = "xmin",
      validate = TRUE
    ),
    "Optimizer test run failed"
  )
  expect_error(
    define_optimizer(
      optimizer = stats::nlm,
      objective = "f",
      initial = "p",
      value = "estimate",
      parameter = "value",
      validate = TRUE
    ),
    "The optimal function value is not a single"
  )
  expect_warning(
    define_optimizer(
      optimizer = function(f, p) Sys.sleep(10),
      objective = "f",
      initial = "p",
      value = "minimum",
      parameter = "estimate",
      validate = TRUE,
      validation_settings = list(
        check_seconds = 1
      )
    ),
    "Optimizer test run cannot be validated."
  )
  expect_error(
    define_optimizer(
      optimizer = function(f, p) {
        return(p)
      },
      objective = "f",
      initial = "p",
      value = "minimum",
      parameter = "estimate",
      validate = TRUE
    ),
    "Optimizer output is not a"
  )
  expect_error(
    define_optimizer(
      optimizer = stats::nlm,
      objective = "f",
      initial = "p",
      value = "bad_name",
      parameter = "estimate",
      validate = TRUE
    ),
    "is not contained in the optimizer output."
  )
  expect_error(
    define_optimizer(
      optimizer = function(f, p) list("minimum" = "not_a_numeric"),
      objective = "f",
      initial = "p",
      value = "minimum",
      parameter = "estimate",
      validate = TRUE
    ),
    "The optimal function value is not a single"
  )
  expect_error(
    define_optimizer(
      optimizer = stats::nlm,
      objective = "f",
      initial = "p",
      value = "minimum",
      parameter = "bad_name",
      validate = TRUE
    ),
    "is not contained in the optimizer output."
  )
  expect_error(
    define_optimizer(
      optimizer = function(f, p) {
        list("minimum" = 0, "estimate" = "not_a_numeric")
      },
      objective = "f",
      initial = "p",
      value = "minimum",
      parameter = "estimate",
      validate = TRUE
    ),
    "The optimum is not a"
  )
  expect_s3_class(
    validate_optimizer(x = optimizer_nlm(), validate = TRUE),
    "optimizer"
  )
})

test_that("unnamed optimizer can be specified", {
  opt <- define_optimizer(
    optimizer = function(f, p) {
      list("minimum" = 0, "parameter" = p)
    },
    objective = "f",
    initial = "p",
    value = "minimum",
    parameter = "parameter"
  )
  expect_equal(opt$optimizer_name, "unnamed_optimizer")
})

test_that("printing optimizer object works", {
  expect_error(print.optimizer("not_an_optimizer"))
  expect_snapshot(optimizer_nlm())
  expect_snapshot(optimizer_optim())
})

test_that("pre-specified nlm optimizer works", {
  expect_s3_class(optimizer_nlm(), "optimizer")
})

test_that("pre-specified optim optimizer works", {
  expect_s3_class(optimizer_optim(), "optimizer")
})

test_that("optimization works", {
  f_ackley <- function(x) {
    stopifnot(is.numeric(x), length(x) == 2)
    -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) -
      exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
  }
  out <- apply_optimizer(optimizer_nlm(), f_ackley, c(-2, 2))
  expect_type(out, "list")
  expect_type(out$value, "double")
  expect_type(out$parameter, "double")
})
