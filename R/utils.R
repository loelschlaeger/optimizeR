#' Check for number
#'
#' @description
#' This function checks if the input \code{x} is a (vector of) number(s), i.e.
#' a (vector of) positive integer value(s).
#'
#' @param x
#' A (vector of) value(s).
#'
#' @return
#' A logical vector of the same length as \code{x}.
#'
#' @examples
#' \dontrun{
#' is_number(c(0, 1, 1.5))
#' }
#'
#' @keywords
#' internal utils

is_number <- function(x) {
  sapply(x, function(x) is.numeric(x) && x > 0 && x %% 1 == 0, USE.NAMES = F)
}

#' Try an expression silently
#'
#' @description
#' This function tries to execute \code{expr} and returns a string with the
#' error message if the execution failed.
#'
#' @details
#' This function is a wrapper for \code{\link[base]{try}}.
#'
#' @param expr
#' An R expression to try.
#'
#' @return
#' Either the value of \code{expr} or in case of a failure an object of class
#' \code{fail}, which contains the error message.
#'
#' @examples
#' \dontrun{
#' try_silent(1 + 1)
#' try_silent(1 + "1")
#' }
#'
#' @keywords
#' internal utils

try_silent <- function(expr) {
  out <- suppressWarnings(try(expr, silent = TRUE))
  if ("try-error" %in% class(out)) {
    out <- structure(as.character(out[1]), class = "fail")
  }
  return(out)
}

#' Interruption of long evaluations
#'
#' @description
#' This function evaluates \code{expr} and interrupts the evaluation after
#' \code{secs} seconds.
#'
#' @details
#' This function is a wrapper for \code{\link[R.utils]{withTimeout}}.
#'
#' @param expr
#' An R expression to be evaluated.
#' @param secs
#' The number of seconds.
#'
#' @return
#' Either the value of \code{expr} or \code{NULL} if the evaluation time
#' exceeded \code{secs} seconds.
#'
#' @examples
#' \dontrun{
#' foo <- function(x) { for(i in 1:10) Sys.sleep(x/10); return(x) }
#' timed(foo(0.5), 1)
#' timed(foo(1.5), 1)
#' }
#'
#' @keywords
#' internal utils

timed <- function(expr, secs) {
  if (!(length(secs) == 1 && is_number(secs))) {
    stop("'secs' must be a number.", call. = FALSE)
  }
  setTimeLimit(cpu = secs, elapsed = secs, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  tryCatch(
    {
      expr
    },
    error = function(e) {
      msg <- e$message
      tl <- grepl("reached elapsed time limit|reached CPU time limit", msg)
      if (tl) return(NULL) else stop(msg, call. = FALSE)
    }
  )
}

#' Measure computation time
#'
#' @description
#' This function measures the computation time of a \code{do.call} call.
#'
#' @details
#' This function is a wrapper for \code{\link[base]{do.call}}.
#'
#' @param what
#' Passed to \code{\link[base]{do.call}}.
#' @param args
#' Passed to \code{\link[base]{do.call}}.
#'
#' @return
#' A list of the two elements \code{"res"} (the results of the \code{do.call}
#' call) and \code{"time"} (the computation time).
#'
#' @examples
#' \dontrun{
#' what <- function(s) { Sys.sleep(s); return(s) }
#' args <- list(s = 1)
#' do.call_timed(what = what, args = args)
#' }
#'
#' @keywords
#' internal utils

do.call_timed <- function(what, args) {
  start <- Sys.time()
  res <- do.call(what = what, args = args)
  end <- Sys.time()
  total <- difftime(end, start)
  return(list("res" = res, "time" = total))
}
