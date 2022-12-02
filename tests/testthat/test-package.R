test_that("exceptions work", {
  expect_error(optimizeR_stop("error"))
  expect_warning(optimizeR_warn("warnings"))
})
