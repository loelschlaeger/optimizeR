test_that("optimizer dictionary can be accessed", {
  expect_setequal(
    optimizer_dictionary$keys,
    c(
      "lbfgsb3c::lbfgsb3c", "lbfgsb3c::lbfgsb3", "lbfgsb3c::lbfgsb3f",
      "lbfgsb3c::lbfgsb3x", "stats::nlm", "stats::nlminb", "stats::optim",
      "ucminf::ucminf", "nloptr::nloptr", "vntrs::vntrs", "ao::ao",
      "optimx::optimr", "DEoptim::DEoptim", "GenSA::GenSA"
    )
  )
  expect_output(
    print(optimizer_dictionary),
    "stats::nlm"
  )
  for (which in optimizer_dictionary$keys) {
    algorithm <- strsplit(
      optimizer_dictionary$get(which, "algorithm"), "::", fixed = TRUE
    )[[1]]
    expect_length(algorithm, 2)
    if (algorithm[1] == "optimizeR") {
      expect_true(is.function(get(algorithm[2])))
      expect_identical(
        adapter_packages[[algorithm[2]]],
        strsplit(which, "::", fixed = TRUE)[[1]][1]
      )
    }
  }
  expect_error(
    optimizer_dictionary$add(
      "label" = "custom", "algorithm" = "stats::optim", "arg_objective" = "fn",
      "arg_initial" = "par", "arg_lower" = NA, "arg_upper" = NA,
      "arg_gradient" = NA, "arg_hessian" = NA, "gradient_as_attribute" = FALSE,
      "hessian_as_attribute" = FALSE, "out_value" = "value",
      "out_parameter" = "par", "direction" = "min"
    )
  )
})

test_that("optimizers from dictionary work", {
  f_1 <- function(x) (x - 1)^2
  f_2 <- function(x) sum((x - c(1, 2))^2)
  for (which in optimizer_dictionary$keys) {
    package <- strsplit(which, "::", fixed = TRUE)[[1]][1]
    if (!requireNamespace(package, quietly = TRUE)) {
      next
    }
    opt <- Optimizer$new(which)
    opt$hide_warnings <- TRUE
    expect_equal(opt$label, which)

    ### one parameter
    set.seed(1)
    out <- suppressMessages(
      opt$minimize(f_1, 3, lower = -5, upper = 5)
    )
    expect_false(out$error)
    expect_equal(out$value, 0, tolerance = 1e-4)
    expect_equal(out$parameter, 1, tolerance = 1e-2)

    ### two named parameters and maximization
    set.seed(1)
    out <- suppressMessages(
      opt$maximize(
        function(x) -f_2(x), c(a = 0, b = 0), lower = -5, upper = c(5, 5)
      )
    )
    expect_false(out$error)
    expect_equal(out$value, 0, tolerance = 1e-4)
    expect_equal(unname(out$parameter), c(1, 2), tolerance = 1e-2)
    expect_equal(out$initial, c(a = 0, b = 0))
    expect_true(out$seconds >= 0)
  }
})

test_that("optimizers can be added to the dictionary", {
  nelder_mead <- function(fn, par, ...) stats::optim(par = par, fn = fn, ...)
  optimizer_dictionary$add(
    "label" = "nelder_mead", "algorithm" = nelder_mead, "arg_objective" = "fn",
    "arg_initial" = "par", "arg_lower" = NA, "arg_upper" = NA,
    "arg_gradient" = NA, "arg_hessian" = NA, "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE, "out_value" = "value",
    "out_parameter" = "par", "direction" = "min"
  )
  optimizer_dictionary$add(
    "label" = "bfgs", "algorithm" = "stats::optim", "arg_objective" = "fn",
    "arg_initial" = "par", "arg_lower" = NA, "arg_upper" = NA,
    "arg_gradient" = "gr", "arg_hessian" = NA, "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE, "out_value" = "value",
    "out_parameter" = "par", "direction" = "min"
  )
  optimizer_dictionary$add(
    "label" = "no_package", "algorithm" = "optim", "arg_objective" = "fn",
    "arg_initial" = "par", "arg_lower" = NA, "arg_upper" = NA,
    "arg_gradient" = NA, "arg_hessian" = NA, "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE, "out_value" = "value",
    "out_parameter" = "par", "direction" = "min"
  )
  expect_true(
    all(c("nelder_mead", "bfgs", "no_package") %in% optimizer_dictionary$keys)
  )
  f <- function(x) sum((x - c(1, 2))^2)
  out <- Optimizer$new("nelder_mead")$minimize(f, c(0, 0))
  expect_equal(out$value, 0, tolerance = 1e-4)
  out <- Optimizer$new("bfgs", method = "BFGS")$minimize(f, c(0, 0))
  expect_equal(out$parameter, c(1, 2), tolerance = 1e-6)
  expect_error(
    Optimizer$new("no_package"),
    "must be a function or a name of the form"
  )
  expect_error(
    optimizer_dictionary$add(
      "label" = "bad", "algorithm" = 1, "arg_objective" = "fn",
      "arg_initial" = "par", "arg_lower" = NA, "arg_upper" = NA,
      "arg_gradient" = NA, "arg_hessian" = NA, "gradient_as_attribute" = FALSE,
      "hessian_as_attribute" = FALSE, "out_value" = "value",
      "out_parameter" = "par", "direction" = "min"
    )
  )
  for (label in c("nelder_mead", "bfgs", "no_package")) {
    optimizer_dictionary$remove(label)
  }
  expect_false(
    any(c("nelder_mead", "bfgs", "no_package") %in% optimizer_dictionary$keys)
  )
})
