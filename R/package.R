#' optimizeR: Unified Framework for Numerical Optimizer
#'
#' @description
#' This package provides a unified framework for numerical optimizer,
#' particularly for inputs and outputs.
#'
#' @docType package
#' @name optimizeR
#' @keywords
#' internal
"_PACKAGE"

#' @noRd
#' @keywords
#' internal

optimizeR_stop <- function(event, debug = character(), call. = FALSE) {
  msg <- paste(event, debug, sep = "\n", collapse = "")
  stop(msg, call. = call.)
}

#' @noRd
#' @keywords
#' internal

optimizeR_warn <- function(event, debug = character(), call. = FALSE, immediate. = FALSE) {
  msg <- paste(event, debug, sep = "\n", collapse = "")
  warning(msg, call. = call., immediate. = immediate.)
}
