test_that("construct optimizer works", {
  expect_error(set_optimizer())
  expect_error(set_optimizer(opt_fun = stats::nlm))
  expect_error(set_optimizer(opt_fun = stats::nlm, f = "f"))
  expect_error(set_optimizer(opt_fun = stats::nlm, f = "f", p = "p"))
  expect_error(set_optimizer(opt_fun = stats::nlm, f = "f", p = "p", v = "v"))
  expect_error(set_optimizer(opt_fun = stats::nlm, f = "f", p = "p", z = "z"))
  optimizer <- new_optimizer(
    opt_fun = pracma::nelder_mead,
    opt_name = "nelder_mead",
    add = list("tol" = 1e-10),
    f = "fn",
    p = "x0",
    v = "fmin",
    z = "xmin",
    out_ign = "restarts"
  )
  expect_s3_class(optimizer, "optimizer")
  optimizer <- validate_optimizer(optimizer)
  expect_s3_class(optimizer, "optimizer")
})

test_that("optimizier validation works", {
  expect_error(set_optimizer(
    opt_fun = f_ackley,
    f = "fn",
    p = "x0",
    v = "fmin",
    z = "xmin"
  ))
  expect_error(set_optimizer(
    opt_fun = stats::nlm,
    f = "f",
    p = "p",
    v = "minimum",
    z = "estimate",
    test_par = list(
      validate = TRUE,
      f_test = f_ackley
    )
  ))
  expect_warning(set_optimizer(
    opt_fun = function(f, p) Sys.sleep(10),
    f = "f",
    p = "p",
    v = "minimum",
    z = "estimate",
    test_par = list(
      validate = TRUE,
      f_test = f_ackley,
      npar = 10,
      opt_checks = 1,
      opt_check_time = 1
    )
  ))
  expect_error(set_optimizer(
    opt_fun = function(f, p) return(p),
    f = "f",
    p = "p",
    v = "minimum",
    z = "estimate"
  ))
  expect_error(set_optimizer(
    opt_fun = stats::nlm,
    f = "f",
    p = "p",
    v = "bad_name",
    z = "estimate"
  ))
  expect_error(set_optimizer(
    opt_fun = function(f, p) list("minimum" = "not_a_numeric"),
    f = "f",
    p = "p",
    v = "minimum",
    z = "estimate"
  ))
  expect_error(set_optimizer(
    opt_fun = stats::nlm,
    f = "f",
    p = "p",
    v = "minimum",
    z = "bad_name"
  ))
  expect_error(set_optimizer(
    opt_fun = function(f, p) list("minimum" = 0, "estimate" = "not_a_numeric"),
    f = "f",
    p = "p",
    v = "minimum",
    z = "estimate"
  ))
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
  out <- apply_optimizer(optimizer_nlm(), f_ackley, c(-2, 2))
  expect_type(out, "list")
  expect_type(out$v, "double")
  expect_type(out$z, "double")
})

test_that("Ackley function call works", {
  expect_equal(f_ackley(c(0, 0)), 0)
})

