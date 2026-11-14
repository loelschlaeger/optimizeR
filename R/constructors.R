#' Create optimizer and objective objects
#'
#' @description
#' The function `optimizer()` creates an \code{\link{Optimizer}} object and is
#' equivalent to `Optimizer$new()`.
#'
#' The function `objective()` creates an \code{\link{Objective}} object and is
#' equivalent to `Objective$new()`.
#'
#' @param which \[`character(1)`\]\cr
#' Either:
#'
#' - one of \code{optimizer_dictionary$keys} (the package of the optimizer
#' must be installed)
#' - or \code{"custom"}, in which case \code{$definition()} must be used to
#' define the optimizer details.
#'
#' @param ...
#' For `optimizer()`, named arguments to pass to the optimizer algorithm.
#' Additionally, `.verbose = FALSE` can be set to suppress the status message
#' for `which = "custom"`.
#'
#' For `objective()`, optionally additional function arguments that are fixed
#' during the optimization.
#'
#' @param f \[`function`\]\cr
#' A \code{function} to be optimized.
#'
#' @param target \[`character()`\]\cr
#' The argument name(s) to optimize.
#'
#' If \code{NULL} (default), the first function argument is selected.
#'
#' @param npar \[`integer()`\]\cr
#' The length of each target argument.
#'
#' @return
#' For `optimizer()`: An \code{\link{Optimizer}} object.
#'
#' For `objective()`: An \code{\link{Objective}} object.
#'
#' @name constructors
#' @export
#'
#' @examples
#' ### the Rosenbrock function
#' rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
#'
#' ### minimize with 'stats::nlm'
#' nlm <- optimizer("stats::nlm", iterlim = 200)
#' nlm$minimize(rosenbrock, initial = c(-1.2, 1))
#'
#' ### evaluate the objective
#' rosenbrock_objective <- objective(rosenbrock, npar = 2)
#' rosenbrock_objective$evaluate(c(1, 1))

optimizer <- function(which, ...) {
  Optimizer$new(which = which, ...)
}

#' @rdname constructors
#' @export

objective <- function(f, target = NULL, npar, ...) {
  out <- Objective$new(f = f, target = target, npar = npar, ...)
  ### set objective name (only known here)
  out$objective_name <- oeli::variable_name(f)
  out
}
