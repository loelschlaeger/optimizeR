devtools::load_all()


test_that("parameter spaces work", {
  parameter_names <- c("b", "Omega", "Sigma")
  parameter_lengths_in_o_space <- c(3, 6, 3)
  probit_parameter <- ParameterSpaces$new(
    parameter_names = parameter_names,
    parameter_lengths_in_o_space = parameter_lengths_in_o_space
  )

  self <- probit_parameter
  private <- self$.__enclos_env__$private

  probit_parameter$o2i(
    "Omega" = function(x) oeli::chol_to_cov(x),
    "Sigma" = function(x) oeli::undiff_cov(oeli::chol_to_cov(x))
  )

  probit_parameter$i2o(
    "Omega" = function(x) oeli::cov_to_chol(x),
    "Sigma" = function(x) oeli::cov_to_chol(oeli::diff_cov(x))
  )


  # TODO
  print(self, TRUE)




  probit_parameter$switch(
    x = list(
      b = 1:3,
      Omega = oeli::sample_covariance_matrix(dim = 3),
      Sigma = oeli::sample_covariance_matrix(dim = 3)
    )
  )


  # TODO
  probit_parameter$switch(rnorm(12)
  )
})

# TODO: vignette
