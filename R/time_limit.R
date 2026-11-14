### deadline of the running computations
time_limit <- new.env(parent = emptyenv())
time_limit$deadline <- Inf

#' @keywords internal
#' @noRd

with_time_limit <- function(expr, seconds, on_time_out) {

  ### nothing to do if there is no time limit
  if (is.infinite(seconds) && is.infinite(time_limit$deadline)) {
    return(expr)
  }

  ### set deadline
  deadline_enclosing <- time_limit$deadline
  deadline <- as.numeric(Sys.time()) + seconds
  limited <- deadline < deadline_enclosing
  if (limited) {
    time_limit$deadline <- deadline
    on.exit({
      time_limit$deadline <- deadline_enclosing
      update_time_limit()
    })
    update_time_limit()
  }

  ### evaluate expression
  tryCatch(
    {
      value <- expr
      if (limited) {
        ### reset deadline before returning
        time_limit$deadline <- deadline_enclosing
        update_time_limit()
        limited <- FALSE
      }
      value
    },
    optimizeR_time_out = function(condition) {
      if (limited) on_time_out else stop(condition)
    },
    error = function(e) {
      if (!is_time_out(e)) {
        stop(e)
      }
      if (limited) on_time_out else signal_time_out()
    }
  )

}

#' @keywords internal
#' @noRd

check_time_limit <- function() {
  if (as.numeric(Sys.time()) >= time_limit$deadline - 1e-3) {
    signal_time_out()
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd

update_time_limit <- function() {
  seconds <- max(
    time_limit$deadline - as.numeric(Sys.time()), .Machine$double.eps
  )
  setTimeLimit(elapsed = seconds, transient = TRUE)
}

#' @keywords internal
#' @noRd

is_time_out <- function(e) {
  if (!is.finite(time_limit$deadline)) {
    return(FALSE)
  }
  time_limit_message <- "reached elapsed time limit"
  grepl(time_limit_message, conditionMessage(e), fixed = TRUE) ||
    grepl(
      gettext(time_limit_message, domain = "R"), conditionMessage(e),
      fixed = TRUE
    ) ||
    as.numeric(Sys.time()) >= time_limit$deadline - 1e-3
}

#' @keywords internal
#' @noRd

signal_time_out <- function() {
  stop(
    structure(
      list("message" = "time limit exceeded", "call" = NULL),
      class = c("optimizeR_time_out", "condition")
    )
  )
}
