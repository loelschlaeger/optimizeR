test_that("construct optimizer works", {
  opt <- new_optimizer(
    opt = pracma::nelder_mead,
    opt_name = "nelder_mead",
    add = list("tol" = 1e-10),
    f = "fn",
    p = "x0",
    v = "fmin",
    z = "xmin",
    out_ign = "restarts"
  )
  expect_s3_class(opt, "optimizer")
  opt <- validate_optimizer(opt)
  expect_s3_class(opt, "optimizer")
})

test_that("optimizier validation works", {
  expect_error(set_optimizer(
    opt = f_ackley,
    f = "fn",
    p = "x0",
    v = "fmin",
    z = "xmin"
  ))
})

test_that("pre-specified nlm optimizer works", {
  opt <- set_optimizer_nlm()
  expect_s3_class(opt, "optimizer")
})

test_that("pre-specified optim optimizer works", {
  opt <- set_optimizer_optim()
  expect_s3_class(opt, "optimizer")
})

test_that("optimization works", {
  out <- optimizeR(set_optimizer_nlm(), f_ackley, c(-2,2))
  expect_type(out, "list")
  expect_type(out$v, "double")
  expect_type(out$z, "double")
})
