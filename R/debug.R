
#' @format NULL
#' @export

optimizer_debug <- Optimizer$
  new(algorithm = test_optimizer)

#' test optimizer
#' @description
#' A short description...

test_optimizer <- function(
    f, p, ..., parameter = p, value = f(parameter), seconds = 0,
    warning_prob = 0, error_prob = 0
) {

}

#' test function
#' @description
#' A short description...

test_objective <- function(x, warning_prob = 0, error_prob = 0) {

}
