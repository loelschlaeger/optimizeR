test_that("test optimization function works", {
  expect_equal(
    test_optimizer(p = 1),
    list("parameter" = 1, "value" = 1)
  )
  expect_warning(
    test_optimizer(p = 1, warning_prob = 1, warning_msg = "my warning"),
    "my warning"
  )
  expect_error(
    test_optimizer(p = 1, error_prob = 1, error_msg = "my error"),
    "my error"
  )
})

test_that("test objectiv function works", {
  expect_equal(
    test_objective(1),
    1
  )
  expect_warning(
    test_objective(1, warning_prob = 1, warning_msg = "my warning"),
    "my warning"
  )
  expect_error(
    test_objective(1, error_prob = 1, error_msg = "my error"),
    "my error"
  )
})
