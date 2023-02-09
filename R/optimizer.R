#' Specify numerical optimizer
#'
#' @description
#' This function specifies the framework for a numerical optimizer.
#'
#' Two wrappers for common optimizer:
#' 1. \code{optimizer_nlm} specifies the \code{\link[stats]{nlm}} optimizer.
#' 2. \code{optimizer_optim} specifies the \code{\link[stats]{optim}} optimizer.
#'
#' @inheritSection new_optimizer Format
#'
#' @param ...
#' Additional arguments to be passed to the optimizer. Without
#' specifications, the default values of the optimizer are used.
#' @inheritParams new_optimizer
#' @inheritParams validate_optimizer
#'
#' @return
#' An \code{optimizer} object.
#'
#' @seealso
#' Use [apply_optimizer()] to apply an \code{optimizer} object for numerical
#' optimization.
#'
#' @export
#'
#' @importFrom stats rnorm
#'
#' @examples
#' define_optimizer(
#'   optimizer = pracma::nelder_mead,
#'   objective = "fn",
#'   initial = "x0",
#'   value = "fmin",
#'   parameter = "xmin",
#'   output_ignore = c("fcount", "restarts", "errmess"), # ignore some outputs
#'   tol = 1e-6, # an additional argument for pracma::nelder_mead()
#'   validate = TRUE # validate the framework
#' )
#'
#' @keywords specification

define_optimizer <- function(
    optimizer, objective, initial, value, parameter, ...,
    output_ignore = character(0), validate = FALSE,
    validation_settings = list(
      "objective_test" = function(x) {
        # Ackley test function
        stopifnot(is.numeric(x), length(x) == 2)
        -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) -
          exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
      },
      "objective_add" = list(),
      "initial" = round(stats::rnorm(2), 2),
      "check_seconds" = 10
    )
) {
  if(missing(optimizer)){
    optimizeR_stop(
      "Please specify argument 'optimizer'.",
      "It should be a function that performs numerical optimization."
    )
  }
  if(missing(objective)){
    optimizeR_stop(
      "Please specify argument 'objective'.",
      "It should be the name of the function argument of 'optimizer'."
    )
  }
  if(missing(initial)){
    optimizeR_stop(
      "Please specify argument 'initial'.",
      "It should be the name of the initial value argument of 'optimizer'."
    )
  }
  if(missing(value)){
    optimizeR_stop(
      "Please specify argument 'value'.",
      "It should be the name of the optimal function value in the output list of 'optimizer'."
    )
  }
  if(missing(parameter)){
    optimizeR_stop(
      "Please specify argument 'parameter'.",
      "It should be the name of the optimal parameter vector in the output list of 'optimizer'."
    )
  }
  optimizer_name <- deparse(substitute(optimizer))
  if (!is.character(optimizer_name) || length(optimizer_name) != 1) {
    optimizer_name <- "unnamed_optimizer"
  }
  validate_optimizer(
    x = new_optimizer(
      optimizer = optimizer,
      optimizer_name = optimizer_name,
      optimizer_add = list(...),
      objective = objective,
      initial = initial,
      value = value,
      parameter = parameter,
      output_ignore = output_ignore
    ),
    validate = validate,
    validation_settings = validation_settings
  )
}

#' @rdname define_optimizer
#' @export
#' @importFrom stats nlm

optimizer_nlm <- function(
    ..., output_ignore = character(0), validate = FALSE,
    validation_settings = list()
  ) {
  define_optimizer(
    optimizer = stats::nlm,
    objective = "f",
    initial = "p",
    value = "minimum" ,
    parameter = "estimate",
    ...,
    output_ignore = output_ignore,
    validate = validate,
    validation_settings = validation_settings
  )
}

#' @rdname define_optimizer
#' @export
#' @importFrom stats optim

optimizer_optim <- function(
    ..., output_ignore = character(0), validate = FALSE,
    validation_settings = list()
  ) {
  define_optimizer(
    optimizer = stats::optim,
    objective = "fn",
    initial = "par",
    value = "value",
    parameter = "par",
    ...,
    output_ignore = output_ignore,
    validate = validate,
    validation_settings = validation_settings
  )
}

#' Construct `optimizer` object
#'
#' @description
#' This function constructs an S3 \code{optimizer} object.
#'
#' @details
#' # Format
#' An \code{optimizer} object is a \code{list} of five elements:
#' \describe{
#'   \item{optimizer}{A \code{function}, the optimization function
#'   \code{optimizer}.}
#'   \item{optimizer_name}{A \code{character}, the name of
#'   \code{optimizer}.}
#'   \item{optimizer_add}{A named \code{list}, where each element
#'   is an additional function argument for \code{optimizer}.}
#'   \item{argument_names}{A named \code{list} of four
#'   \code{character}:
#'   \describe{
#'     \item{objective}{the name of the function input of \code{optimizer}}
#'     \item{initial}{the name of the starting parameter values input of
#'     \code{optimizer}}
#'     \item{value}{the name of the optimal function value in the output list
#'     of \code{optimizer}}
#'     \item{parameter}{the name of the optimal parameter vector in the
#'     output list of \code{optimizer}.}
#'   }}
#'   \item{output_ignore}{A \code{character} vector of element
#'   names in the output \code{list} of \code{optimizer} that are ignored.
#'   The elements \code{value} and \code{parameter} are added automatically to
#'   \code{output_ignore}, because they are saved
#'   separately, see the output documentation of \code{\link{apply_optimizer}}.}
#' }
#'
#' @param x
#' A \code{list}.
#' @param optimizer
#' A \code{function}, a numerical optimizer. Four conditions must be met:
#' 1. It must have an input named \code{"objective"} for a \code{function}, the
#'    objective function which is optimized over its first argument.
#' 2. It must have an input named \code{"initial"} for a \code{numerical}
#'    vector, the initial parameter vector.
#' 3. It must have a \code{...} argument for additional parameters to
#'    the objective function.
#' 4. The output must be a named \code{list}, including the optimal function
#'    value and the optimal parameter vector.
#' @param optimizer_name
#' A \code{character}, the name of \code{optimizer}.
#' @param optimizer_add
#' A \code{list} of additional and named arguments to be passed to
#' \code{optimizer}.
#' @param objective
#' A \code{character}, the name of the function input of \code{optimizer}.
#' @param initial
#' A \code{character}, the name of the starting parameter values input of
#' \code{optimizer}.
#' @param value
#' A \code{character}, the name of the optimal function value in the output list
#' of \code{optimizer}.
#' @param parameter
#' A \code{character}, the name of the optimal parameter vector in the output
#' list of \code{optimizer}.
#' @param output_ignore
#' A \code{character} vector of element names in the output of \code{optimizer}
#' that are not saved. The elements \code{value} and \code{parameter} are
#' added automatically to \code{output_ignore}, because they are saved
#' separately, see the output documentation of \code{\link{apply_optimizer}}.
#'
#' @return
#' An S3 object of class \code{optimizer}.
#'
#' @keywords internal

new_optimizer <- function(
    x = list(), optimizer = function() {}, optimizer_name = character(),
    optimizer_add = list(), objective = character(), initial = character(),
    value = character(), parameter = character(), output_ignore = character()
) {
  stopifnot(is.list(x))
  stopifnot(is.function(optimizer))
  stopifnot(is.character(optimizer_name))
  stopifnot(is.list(optimizer_add))
  stopifnot(is.character(objective))
  stopifnot(is.character(initial))
  stopifnot(is.character(value))
  stopifnot(is.character(parameter))
  stopifnot(is.character(output_ignore))
  x[["optimizer"]] <- optimizer
  x[["optimizer_name"]] <- optimizer_name
  x[["optimizer_add"]] <- optimizer_add
  x[["argument_names"]] <- list(
    "objective" = objective,
    "initial" = initial,
    "value" = value,
    "parameter" = parameter
  )
  x[["output_ignore"]] <- union(output_ignore, c(value, parameter))
  structure(x, class = "optimizer")
}

#' Validate `optimizer` object
#'
#' @description
#' This function validates an \code{optimizer} object.
#'
#' @param x
#' An object of class \code{optimizer}.
#' @param validate
#' A \code{logical}, set to \code{TRUE} (\code{FALSE}) to (not) validate the
#' \code{optimizer} object.
#' By default, \code{validate = FALSE}.
#' @param validation_settings
#' Ignored if \code{valdiate = FALSE}.
#' Otherwise, a \code{list} of validation settings:

#' \describe{
#'   \item{objective_test}{A \code{function}, the test function to be optimized.
#'   By default, it is the
#'   \href{https://en.wikipedia.org/wiki/Ackley_function}{Ackley function}.}
#'   \item{objective_add}{A \code{list} of additional arguments to
#'   \code{objective_test} (if any).
#'   By default, \code{objective_add = list()}, because the default function
#'   for \code{objective_test} does not have additional arguments.}
#'   \item{initial}{A \code{numeric} vector, the initial values for the
#'   optimization of \code{objective_test}.
#'   By default, \code{initial = round(stats::rnorm(2), 2)}.}
#'   \item{check_seconds}{An \code{integer}, the maximum number of seconds
#'   before the test is aborted.
#'   The test call is considered to be successful if no error occurred
#'   within \code{check_seconds} seconds.
#'   By default, \code{check_seconds = 10}.}
#' }
#'
#' @return
#' The validated input \code{x}.
#'
#' @keywords internal
#'
#' @importFrom stats runif

validate_optimizer <- function(
    x = new_optimizer(), validate = FALSE, validation_settings = list()
) {
  stopifnot(inherits(x, "optimizer"))
  stopifnot(isTRUE(validate) || isFALSE(validate))
  stopifnot(is.list(validation_settings))
  if (validate) {
    if (!exists("objective_test", where = validation_settings)) {
      validation_settings[["objective_test"]] <- function(x) {
        stopifnot(is.numeric(x), length(x) == 2)
        -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) -
          exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
      }
    }
    if (!exists("objective_add", where = validation_settings)) {
      validation_settings[["objective_add"]] <- list()
    }
    if (!exists("initial", where = validation_settings)) {
      validation_settings[["initial"]] <- round(stats::rnorm(2), 2)
    }
    if (!exists("check_seconds", where = validation_settings)) {
      validation_settings[["check_seconds"]] <- 10
    }
    initial <- validation_settings[["initial"]]
    opt_out <- try_silent(
      expr = timed(
        do.call(
          what = x[["optimizer"]],
          args = c(
            structure(
              list(validation_settings[["objective_test"]], initial),
              names = x[["argument_names"]][c("objective", "initial")]
            ),
            x[["optimizer_add"]], validation_settings[["objective_add"]]
          )
        ),
        secs = validation_settings[["check_seconds"]]
      )
    )
    if (is.null(opt_out)) {
      optimizeR_warn(
        "Optimizer test run cannot be validated.",
        "The test run returned `NULL`.
        The optimization most likely reached the time limit.",
        "Try to increase 'check_seconds'."
      )
    } else if (inherits(opt_out, "fail")) {
      stop("Optimizer test run failed.\n", opt_out[1], call. = FALSE)
    } else {
      if (!is.list(opt_out)) {
        optimizeR_stop(
          "Optimizer output is not a `list`.",
          "The 'optimizer' function should return a named `list`."
        )
      }
      if (!x[["argument_names"]][["value"]] %in% names(opt_out)) {
        optimizeR_stop(
          "Element 'value' is not contained in the optimizer output.",
          "Please make sure that 'value' is contained in the output list."
        )
      } else {
        value <- opt_out[[x[["argument_names"]][["value"]]]]
        if (!(is.numeric(value) && length(value) == 1)) {
          optimizeR_stop(
            "The optimal function value is not a single numeric."
          )
        }
      }
      if (!x[["argument_names"]][["parameter"]] %in% names(opt_out)) {
        optimizeR_stop(
          "Element 'parameter' is not contained in the optimizer output.",
          "Please make sure that 'parameter' is contained in the output list."
        )
      } else {
        optimum <- opt_out[[x[["argument_names"]][["parameter"]]]]
        if (!is.numeric(optimum)) {
          optimizeR_stop(
            "The optimum is not a numeric value."
          )
        }
      }
    }
  }
  return(x)
}

#' @exportS3Method
#' @noRd
#' @keywords internal

print.optimizer <- function(x, ...) {
  stopifnot(inherits(x, "optimizer"))
  cat("<optimizer '", x[["optimizer_name"]], "'>", sep = "")
}

#' Apply `optimizer` object
#'
#' @description
#' This function performs numerical optimization using an \code{optimizer}
#' object.
#'
#' @param optimizer
#' An object of class \code{optimizer}.
#' @param objective
#' The \code{function} to be optimized, returning a single \code{numeric}.
#' Its first argument must be a \code{numeric} vector of the length of
#' \code{initial}, followed by any other arguments specified by the \code{...}
#' argument.
#' @param initial
#' A \code{numeric} vector with starting parameter values for the optimization.
#' @param ...
#' Additional arguments to be passed to \code{objective}.
#'
#' @return
#' A named \code{list}, containing at least three elements:
#' \describe{
#'   \item{value}{A \code{numeric}, the value of the estimated optimum of
#'         \code{objective}.}
#'   \item{parameter}{A \code{numeric} vector, the parameter vector where the
#'         optimum of \code{objective} is obtained.}
#'   \code{seconds}{A \code{numeric}, the total optimization time in seconds.}
#'   \code{initial}{A \code{numeric}, the initial parameter values.}
#' }
#' Additional output elements of the optimizer (if not excluded by the
#' \code{output_ignore} element via \code{\link{define_optimizer}}) are
#' appended.
#'
#' @seealso
#' [define_optimizer()] for specifying an \code{optimizer} object.
#'
#' @export
#'
#' @examples
#' apply_optimizer(optimizer_nlm(), function(x) x^4 + 3*x - 5, 2)

apply_optimizer <- function(
    optimizer = optimizer_nlm(), objective, initial, ...
  ) {
  start <- Sys.time()
  res <- do.call(
    what = optimizer[["optimizer"]],
    args = c(
      structure(list(objective), names = optimizer[["argument_names"]][["objective"]]),
      structure(list(initial), names = optimizer[["argument_names"]][["initial"]]),
      optimizer[["optimizer_add"]], list(...)
    )
  )
  end <- Sys.time()
  c(
    structure(
      list(res[[optimizer[["argument_names"]][["value"]]]],
           res[[optimizer[["argument_names"]][["parameter"]]]],
           as.numeric(difftime(end, start, units = "secs")),
           initial),
      names = c("value", "parameter", "seconds", "initial")
    ),
    res[!names(res) %in% optimizer[["output_ignore"]]]
  )
}

