test_that("Gaussian mixture model parameter spaces work", {
  ### define parameter spaces
  normal_mixture_spaces <- ParameterSpaces$
    new(
    parameter_names = c("mu", "sd", "lambda"),
    parameter_lengths_in_o_space = c(2, 2, 1)
  )$
    o2i(
    "mu" = function(x) x,
    "sd" = function(x) exp(x),
    "lambda" = function(x) c(plogis(x), 1 - plogis(x))
  )$
    i2o(
    "mu" = function(x) x,
    "sd" = function(x) log(x),
    "lambda" = function(x) qlogis(x[1])
  )
  expect_snapshot(
    print(normal_mixture_spaces, show_transformer = TRUE)
  )

  ### switch between parameter spaces
  par <- list( # parameters in interpretation space
    "mu" = c(2, 4),
    "sd" = c(0.5, 1),
    "lambda" = c(0.4, 0.6)
  )
  x <- normal_mixture_spaces$switch(par
  ) # switch to optimization space
  expect_equal(
    round(x, 2), c(2, 4, -0.69, 0, -0.41)
  )
  expect_equal(
    normal_mixture_spaces$switch(x
    ), # switch back
    par
  )
})

test_that("Probit parameter spaces work", {
  ### define parameter spaces
  parameter_names <- c("b", "Omega", "Sigma")
  parameter_lengths_in_o_space <- c(3, 6, 3)
  probit_parameter <- ParameterSpaces$new(
    parameter_names = parameter_names,
    parameter_lengths_in_o_space = parameter_lengths_in_o_space
  )
  probit_parameter$o2i(
    "Omega" = function(x) oeli::chol_to_cov(x),
    "Sigma" = function(x) oeli::undiff_cov(oeli::chol_to_cov(x))
  )
  probit_parameter$i2o(
    "Omega" = function(x) oeli::cov_to_chol(x),
    "Sigma" = function(x) oeli::cov_to_chol(oeli::diff_cov(x))
  )
  expect_snapshot(
    print(probit_parameter, TRUE)
  )

  ### switch between parameter spaces
  par <- list(
    b = c(1, 2, 3),
    Omega = oeli::sample_covariance_matrix(dim = 3),
    Sigma = oeli::sample_covariance_matrix(dim = 3)
  )
  x <- probit_parameter$switch(x = par
  )
  checkmate::expect_numeric(x, len = 12)
  par2 <- probit_parameter$switch(x = x
  )
  checkmate::expect_list(par2, len = 3)
  expect_identical(par$b, par2$b)
  expect_identical(round(par$Omega, 2), round(par2$Omega, 2))
  expect_identical(round(oeli::diff_cov(par$Sigma), 2), round(oeli::diff_cov(par2$Sigma), 2))

  ### catch errors
  expect_error(
    probit_parameter$switch(rnorm(13)
    ),
    "Input `x` is bad: Must have length 12, but has length 13"
  )
  expect_error(
    probit_parameter$switch(c(par, list("bad" = 1))
    ),
    "Input `x` is bad: Names must be a permutation of"
  )
  expect_error(
    probit_parameter$switch(par[c(1, 2)]
    ),
    "Input `x` is bad: Names must be a set equal to"
  )
  expect_error(
    probit_parameter$switch(list(
      b = 1,
      Omega = oeli::sample_covariance_matrix(dim = 3),
      Sigma = oeli::sample_covariance_matrix(dim = 3)
    )
    ),
    "Must have length 3, but has length 1"
  )
  expect_error(
    probit_parameter$switch("bad"
    ),
    "Input `x` must be"
  )
})

test_that("Parameter spaces with zero length parameter work", {
  ### define parameter spaces
  zero_parameter_spaces <- ParameterSpaces$
    new(
    parameter_names = c("x", "y", "z"),
    parameter_lengths_in_o_space = c(1, 0, 1)
  )$
    o2i(
    "y" = function(x) c("A", "B")
  )$
    i2o(
    "y" = function(x) c()
  )
  expect_snapshot(
    print(zero_parameter_spaces, show_transformer = TRUE)
  )

  ### switch between parameter spaces
  par <- list("x" = 1, "y" = c("A", "B"), "z" = 2)
  x <- zero_parameter_spaces$switch(par
  ) # switch to optimization space
  expect_equal(x, c(1, 2))
  expect_equal(
    zero_parameter_spaces$switch(x
    ), # switch back
    par
  )
})
