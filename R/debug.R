#' Test functions
#'
#' @name debug
#'
#' @description
#' These functions are useful for testing or debugging the behavior of objective
#' and optimization functions.
#'
#' They can throw warnings or errors on purpose.
#'
#' @param objective \[`function`\]\cr
#' The objective function.
#'
#' @param initial \[`numeric()`\]\cr
#' The initial parameter vector.
#'
#' @param ... \[any\]\cr
#' Optionally additional arguments to be passed to \code{objective}.
#'
#' @param parameter \[any\]\cr
#' Defines the output parameter.
#'
#' @param x \[any\]\cr
#' Defines the input.
#'
#' @param value \[any\]\cr
#' Defines the output function value.
#'
#' @param seconds \[`numeric(1)`\]\cr
#' A delay in number of seconds.
#'
#' @param warning_prob \[`numeric(1)`\]\cr
#' The probability for throwing a warning.
#'
#' @param error_prob \[`numeric(1)`\]\cr
#' The probability for throwing an error.
#'
#' @param warning_msg \[`character(1)`\]\cr
#' The warning message.
#'
#' @param error_msg \[`character(1)`\]\cr
#' The error message.
#'
#' @param call. \[`logical(1)`\]\cr
#' Passed to \code{\link[base]{warning}} or \code{\link[base]{stop}},
#' respectively.
#'
#' @return
#' A \code{list} with elements \code{parameter} and \code{value}.
NULL

#' @rdname debug

test_optimizer <- function(
    objective = test_objective,
    initial = 1,
    ...,
    parameter = 1,
    value = objective(parameter),
    seconds = 0,
    warning_prob = 0,
    error_prob = 0,
    warning_msg = "warning",
    error_msg = "error",
    call. = TRUE
  ) {

  ### input checks
  oeli::input_check_response(
    check = checkmate::check_function(objective),
    "objective"
  )
  oeli::input_check_response(
    check = checkmate::check_number(seconds, lower = 0),
    "seconds"
  )
  oeli::input_check_response(
    check = checkmate::check_number(warning_prob, lower = 0, upper = 1),
    "warning_prob"
  )
  oeli::input_check_response(
    check = checkmate::check_number(error_prob, lower = 0, upper = 1),
    "error_prob"
  )
  oeli::input_check_response(
    check = checkmate::check_string(warning_msg),
    "warning_msg"
  )
  oeli::input_check_response(
    check = checkmate::check_string(error_msg),
    "error_msg"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(call.),
    "call."
  )

  ### simulate optimizer function
  Sys.sleep(seconds)
  if (sample(c(TRUE, FALSE), 1, prob = c(warning_prob, 1 - warning_prob))) {
    warning(warning_msg, call. = call.)
  }
  if (sample(c(TRUE, FALSE), 1, prob = c(error_prob, 1 - error_prob))) {
    stop(error_msg, call. = call.)
  }
  list("parameter" = parameter, "value" = value)
}

#' @rdname debug

test_objective <- function(
    x,
    value = x,
    warning_prob = 0,
    error_prob = 0,
    warning_msg = "warning",
    error_msg = "error",
    call. = TRUE
  ) {

  ### input checks
  oeli::input_check_response(
    check = checkmate::check_number(warning_prob, lower = 0, upper = 1),
    "warning_prob"
  )
  oeli::input_check_response(
    check = checkmate::check_number(error_prob, lower = 0, upper = 1),
    "error_prob"
  )
  oeli::input_check_response(
    check = checkmate::check_string(warning_msg),
    "warning_msg"
  )
  oeli::input_check_response(
    check = checkmate::check_string(error_msg),
    "error_msg"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(call.),
    "call."
  )

  ### simulate objective function
  if (sample(c(TRUE, FALSE), 1, prob = c(warning_prob, 1 - warning_prob))) {
    warning(warning_msg, call. = call.)
  }
  if (sample(c(TRUE, FALSE), 1, prob = c(error_prob, 1 - error_prob))) {
    stop(error_msg, call. = call.)
  }
  value
}
