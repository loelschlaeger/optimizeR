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

   ### validate
   suppressMessages(
     expect_error(
       objective$validate(1:2),
       "of length 1"
     )
   )
   expect_error(
     objective$validate(),
     "Please specify the argument `.at`"
   )
   suppressMessages(
     expect_error(
       objective$validate(1),
       "does not have the expected structure"
     )
   )
   suppressMessages(
     expect_message(
        objective$validate(1, output_template_hessian = numeric(1))
     )
   )
   objective$remove_argument("a", .verbose = FALSE)
   suppressMessages(
     expect_error(
       objective$validate(1),
       "Function argument `a` is required but not specified"
     )
   )

   ### synchronizing arguments for gradient and Hessian
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
    "is missing, with no default"
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
  skip_if_not(.Platform$OS.type %in% c("unix", "windows"))
  f <- function(x, a, b = 0) {
    Sys.sleep(1)
    (x + a)^2 + b
  }
  objective <- Objective$new(f = f, npar = 1, a = -2)
  objective$verbose <- FALSE
  objective$seconds <- 0.5
  expect_equal(
    objective$evaluate(1),
    "time limit reached"
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
  expect_silent(
    himmelblau_objective$validate(c(3, 2), .verbose = FALSE)
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
