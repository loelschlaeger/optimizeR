#' Test optimization function
#'
#' @description
#' This function is useful for testing or debugging the behavior of optimization
#' functions. It can throw a warning and / or an error on purpose.
#'
#' @param objective
#' An objective \code{function}.
#' @param initial
#' The initial parameter vector.
#' @param ...
#' Optionally additional arguments to be passed to \code{objective}.
#' @param parameter
#' Defines the output \code{parameter}.
#' @param value
#' Defines the output \code{value}.
#' @param seconds
#' A delay in number of seconds.
#' @param warning_prob
#' The probability for throwing a warning.
#' @param error_prob
#' The probability for throwing an error.
#' @param warning_msg
#' The warning message.
#' @param error_msg
#' The error message.
#' @param call.
#' Passed to \code{\link[base]{warning}} or \code{\link[base]{stop}},
#' respectively.
#'
#' @return
#' A \code{list} with elements \code{parameter} and \code{value}.
#'
#' @export

test_optimizer <- function(
    objective = test_objective, initial = 1, ..., parameter = 1,
    value = objective(parameter), seconds = 0, warning_prob = 0, error_prob = 0,
    warning_msg = "warning", error_msg = "error", call. = TRUE) {
  checkmate::assert_function(objective)
  checkmate::assert_number(seconds, lower = 0)
  checkmate::assert_number(warning_prob, lower = 0, upper = 1)
  checkmate::assert_number(error_prob, lower = 0, upper = 1)
  checkmate::assert_string(warning_msg)
  checkmate::assert_string(error_msg)
  checkmate::assert_flag(call.)
  Sys.sleep(seconds)
  if (sample(c(TRUE, FALSE), 1, prob = c(warning_prob, 1 - warning_prob))) {
    warning(warning_msg, call. = call.)
  }
  if (sample(c(TRUE, FALSE), 1, prob = c(error_prob, 1 - error_prob))) {
    stop(error_msg, call. = call.)
  }
  list("parameter" = parameter, "value" = value)
}

#' Test objective function
#'
#' @description
#' This function is useful for testing or debugging the behavior of objective
#' functions. It can throw a warning and / or an error on purpose.
#'
#' @param x
#' Any input.
#' @param value
#' The return value, any object.
#' @param warning_prob
#' The probability for throwing a warning.
#' @param error_prob
#' The probability for throwing an error.
#' @param warning_msg
#' The warning message.
#' @param error_msg
#' The error message.
#' @param call.
#' Passed to \code{\link[base]{warning}} or \code{\link[base]{stop}},
#' respectively.
#'
#' @return
#' The argument \code{value}.
#'
#' @export

test_objective <- function(
    x, value = x, warning_prob = 0, error_prob = 0, warning_msg = "warning",
    error_msg = "error", call. = TRUE) {
  checkmate::assert_number(warning_prob, lower = 0, upper = 1)
  checkmate::assert_number(error_prob, lower = 0, upper = 1)
  checkmate::assert_string(warning_msg)
  checkmate::assert_string(error_msg)
  checkmate::assert_flag(call.)
  if (sample(c(TRUE, FALSE), 1, prob = c(warning_prob, 1 - warning_prob))) {
    warning(warning_msg, call. = call.)
  }
  if (sample(c(TRUE, FALSE), 1, prob = c(error_prob, 1 - error_prob))) {
    stop(error_msg, call. = call.)
  }
  value
}
