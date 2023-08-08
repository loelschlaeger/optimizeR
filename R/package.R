#' optimizeR: Unified Framework for Numerical Optimizer
#'
#' @description
#' This package provides a unified framework for numerical optimizer,
#' in particular for their inputs and outputs.
#'
#' @docType package
#'
#' @name optimizeR
#'
#' @keywords internal
"_PACKAGE"

# #' @noRd
# #' @importFrom utils packageVersion
# #' @importFrom glue glue
# #' @keywords internal
#
# .onAttach <- function(lib, pkg) {
#   msg <- glue::glue(
#     "Thanks for using {{optimizeR}} {utils::packageVersion('optimizeR')}."
#   )
#   packageStartupMessage(msg)
#   invisible()
# }

#' @noRd
#' @importFrom cli cli_abort
#' @keywords internal

optimizeR_stop <- function(msg, ...) {
  msg <- c(msg, ...)
  names(msg)[1] <- "x"
  names(msg)[-1] <- "*"
  cli::cli_abort(msg, call = NULL)
}

#' @noRd
#' @importFrom cli cli_warn
#' @keywords internal

optimizeR_warn <- function(msg, ...) {
  msg <- c(msg, ...)
  names(msg)[1] <- "!"
  names(msg)[-1] <- "*"
  cli::cli_warn(msg, call = NULL)
}
