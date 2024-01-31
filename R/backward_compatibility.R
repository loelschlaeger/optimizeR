#' Specify numerical optimizer
#'
#' @description
#' This function specifies the framework for a numerical optimizer.
#'
#' Two wrappers for well-known optimizers are already available:
#' 1. \code{optimizer_nlm()} for the \code{\link[stats]{nlm}} optimizer
#' 2. \code{optimizer_optim()} for the \code{\link[stats]{optim}} optimizer
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
#' @examples
#' define_optimizer(
#'   .optimizer = pracma::nelder_mead,           # optimization function
#'   .objective = "fn",                          # name of function input
#'   .initial = "x0",                            # name of initial input
#'   .value = "fmin",                            # name of value output
#'   .parameter = "xmin",                        # name of parameter output
#'   .direction = "min",                         # optimizer minimizes
#'   .output_ignore = c("restarts", "errmess"),  # ignore some outputs
#'   tol = 1e-6,                                 # additional optimizer argument
#'   .validate = TRUE                            # validate the object
#' )

define_optimizer <- function(
    .optimizer, .objective, .initial, .value, .parameter, .direction, ...,
    .output_ignore = character(0), .validate = FALSE,
    .validation_settings = list(
      "objective_test" = TestFunctions::TF_ackley,
      "objective_add" = list(),
      "initial" = round(stats::rnorm(2), 2),
      "check_seconds" = 10
    )
) {
  if (missing(.optimizer)) {
    cli::cli_abort(c(
      "x" = "Please specify argument {.var .optimizer}.",
      "*" = "It should be a {.cls function} that performs numerical
             optimization."
    ), call = NULL
    )
  }
  if (missing(.objective)) {
    cli::cli_abort(c(
      "x" = "Please specify argument {.var .objective}.",
      "*" = "It should be the name of the function argument of
             {.var .optimizer}."
    ), call = NULL
    )
  }
  if (missing(.initial)) {
    cli::cli_abort(c(
      "x" = "Please specify argument {.var .initial}.",
      "*" = "It should be the name of the initial value argument of
             {.var .optimizer}."
    ), call = NULL
    )
  }
  if (missing(.value)) {
    cli::cli_abort(c(
      "x" = "Please specify argument {.var .value}.",
      "*" = "It should be the name of the optimal function value in the output
             list of {.var .optimizer}."
    ), call = NULL
    )
  }
  if (missing(.parameter)) {
    cli::cli_abort(c(
      "x" = "Please specify argument {.var .parameter}.",
      "*" = "It should be the name of the optimal parameter vector in the output
             list of {.var .optimizer}."
    ), call = NULL
    )
  }
  if (missing(.direction)) {
    cli::cli_abort(c(
      "x" = "Please specify argument {.var .direction}.",
      "*" = "It indicates whether the optimizer minimizes ({.val min}) or
             maximizes ({.val max})."
    ), call = NULL
    )
  }
  optimizer_name <- deparse(substitute(.optimizer))
  if (!is.character(optimizer_name) || length(optimizer_name) != 1) {
    optimizer_name <- "unnamed_optimizer"
  }
  optimizer_s3_object <- validate_optimizer(
    x = new_optimizer(
      .optimizer = .optimizer,
      optimizer_name = optimizer_name,
      optimizer_arguments = list(...),
      .objective = .objective,
      .initial = .initial,
      .value = .value,
      .parameter = .parameter,
      .direction = .direction,
      .output_ignore = .output_ignore
    ),
    .validate = .validate,
    .validation_settings = .validation_settings
  )
  suppressMessages(
    optimizer_r6_object <- Optimizer$new(which = "custom")
  )
  optimizer_r6_object$definition(
    algorithm = optimizer_s3_object$optimizer,
    arg_objective = optimizer_s3_object$optimizer_labels$objective,
    arg_initial = optimizer_s3_object$optimizer_labels$initial,
    out_value = optimizer_s3_object$optimizer_labels$value,
    out_parameter = optimizer_s3_object$optimizer_labels$parameter,
    direction = optimizer_s3_object$optimizer_direction
  )
  do.call(optimizer_r6_object$set_arguments, optimizer_s3_object$optimizer_arguments)
  optimizer_r6_object$label <- optimizer_s3_object$optimizer_name
  optimizer_r6_object$output_ignore <- setdiff(
    optimizer_s3_object$output_ignore,
    unlist(optimizer_s3_object$optimizer_labels[c("value", "parameter")])
  )
  return(optimizer_r6_object)
}

#' @rdname define_optimizer
#' @export

optimizer_nlm <- function(
    ..., .output_ignore = character(0), .validate = FALSE,
    .validation_settings = list()
) {
  define_optimizer(
    .optimizer = stats::nlm,
    .objective = "f",
    .initial = "p",
    .value = "minimum" ,
    .parameter = "estimate",
    .direction = "min",
    ...,
    .output_ignore = .output_ignore,
    .validate = .validate,
    .validation_settings = .validation_settings
  )
}

#' @rdname define_optimizer
#' @export

optimizer_optim <- function(
    ..., .direction = "min", .output_ignore = character(0), .validate = FALSE,
    .validation_settings = list()
) {
  define_optimizer(
    .optimizer = stats::optim,
    .objective = "fn",
    .initial = "par",
    .value = "value",
    .parameter = "par",
    .direction = .direction,
    ...,
    .output_ignore = .output_ignore,
    .validate = .validate,
    .validation_settings = .validation_settings
  )
}

#' Construct \code{optimizer} object
#'
#' @description
#' This function constructs an S3 \code{optimizer} object.
#'
#' @details
#' # Format
#' An \code{optimizer} object is a \code{list} of six elements:
#' \describe{
#'   \item{optimizer}{A \code{function}, the optimization algorithm.}
#'   \item{optimizer_name}{A \code{character}, the name of
#'   \code{optimizer}.}
#'   \item{optimizer_arguments}{A named \code{list}, where each element
#'   is an additional function argument for \code{optimizer}.}
#'   \item{optimizer_direction}{Either \code{"min"} if the optimizer minimizes
#'   or \code{"max"} if the optimizer maximizes.}
#'   \item{optimizer_labels}{A named \code{list} of four
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
#' @param .optimizer
#' A \code{function}, a numerical optimizer. Four conditions must be met:
#' 1. It must have an input named \code{.objective} for a \code{function}, the
#'    objective function which is optimized over its first argument.
#' 2. It must have an input named \code{.initial} for a \code{numerical}
#'    vector, the initial parameter vector.
#' 3. It must have a \code{...} argument for additional parameters to
#'    the objective function.
#' 4. The output must be a named \code{list}, including the optimal function
#'    value and the optimal parameter vector.
#' @param optimizer_name
#' A \code{character}, the name of \code{optimizer}.
#' @param optimizer_arguments
#' A \code{list} of additional and named arguments to be passed to
#' \code{optimizer}.
#' @param .objective
#' A \code{character}, the name of the function input of \code{optimizer}.
#' @param .initial
#' A \code{character}, the name of the starting parameter values input of
#' \code{optimizer}.
#' @param .value
#' A \code{character}, the name of the optimal function value in the output list
#' of \code{optimizer}.
#' @param .parameter
#' A \code{character}, the name of the optimal parameter vector in the output
#' list of \code{optimizer}.
#' @param .direction
#' A \code{character}, indicates whether the optimizer minimizes (\code{"min"})
#' or maximizes (\code{"max"}).
#' @param .output_ignore
#' A \code{character} vector of element names in the output of \code{.optimizer}
#' that are not saved. The elements \code{.value} and \code{.parameter} are
#' added automatically to \code{.output_ignore}, because they are saved
#' separately, see the output documentation of \code{\link{apply_optimizer}}.
#'
#' @return
#' An S3 object of class \code{optimizer}.
#'
#' @keywords internal

new_optimizer <- function(
    x = list(), .optimizer = function() {}, optimizer_name = character(),
    optimizer_arguments = list(), .objective = character(),
    .initial = character(), .value = character(), .parameter = character(),
    .direction = character(), .output_ignore = character()
) {
  stopifnot(is.list(x))
  stopifnot(is.function(.optimizer))
  stopifnot(is.character(optimizer_name), length(optimizer_name) == 1)
  stopifnot(is.list(optimizer_arguments))
  stopifnot(is.character(.objective), length(.objective) == 1)
  stopifnot(is.character(.initial), length(.initial) == 1)
  stopifnot(is.character(.value), length(.value) == 1)
  stopifnot(is.character(.parameter), length(.parameter) == 1)
  stopifnot(identical(.direction, "min") || identical(.direction, "max"))
  stopifnot(is.character(.output_ignore))
  x[["optimizer"]] <- .optimizer
  x[["optimizer_name"]] <- optimizer_name
  x[["optimizer_arguments"]] <- optimizer_arguments
  x[["optimizer_direction"]] <- .direction
  x[["optimizer_labels"]] <- list(
    "objective" = .objective,
    "initial" = .initial,
    "value" = .value,
    "parameter" = .parameter
  )
  x[["output_ignore"]] <- union(.output_ignore, c(.value, .parameter))
  structure(x, class = "optimizer")
}

#' Validate `optimizer` object
#'
#' @description
#' This function validates an \code{optimizer} object.
#'
#' @param x
#' An object of class \code{optimizer}.
#' @param .validate
#' A \code{logical}, set to \code{TRUE} (\code{FALSE}) to (not) validate the
#' \code{optimizer} object.
#' By default, \code{.validate = FALSE}.
#' @param .validation_settings
#' Ignored if \code{.valdiate = FALSE}.
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

validate_optimizer <- function(
    x = new_optimizer(), .validate = FALSE, .validation_settings = list()
) {
  stopifnot(inherits(x, "optimizer"))
  stopifnot(isTRUE(.validate) || isFALSE(.validate))
  stopifnot(is.list(.validation_settings))
  if (.validate) {
    if (!exists("objective_test", where = .validation_settings)) {
      .validation_settings[["objective_test"]] <- TestFunctions::TF_ackley
    }
    if (!exists("objective_add", where = .validation_settings)) {
      .validation_settings[["objective_add"]] <- list()
    }
    if (!exists("initial", where = .validation_settings)) {
      .validation_settings[["initial"]] <- round(stats::rnorm(2), 2)
    }
    if (!exists("check_seconds", where = .validation_settings)) {
      .validation_settings[["check_seconds"]] <- 10
    }
    initial <- .validation_settings[["initial"]]
    opt_out <- oeli::try_silent(
      expr = oeli::timed(
        do.call(
          what = x[["optimizer"]],
          args = c(
            structure(
              list(.validation_settings[["objective_test"]], initial),
              names = x[["optimizer_labels"]][c("objective", "initial")]
            ),
            x[["optimizer_arguments"]], .validation_settings[["objective_add"]]
          )
        ),
        seconds = .validation_settings[["check_seconds"]]
      )
    )
    if (is.null(opt_out)) {
      cli::cli_warn(c(
        "!" = "Optimizer test run cannot be validated.",
        "*" = "The test run returned {.val NULL}, the optimization most likely
               reached the time limit.",
        "*" = "Try to increase {.var check_seconds}."
      ))
    } else if (inherits(opt_out, "fail")) {
      cli::cli_abort(c(
        "x" = "Optimizer test run failed.",
        "*" = "Message: {opt_out[1]}"
      ), call = NULL
      )
    } else {
      if (!is.list(opt_out)) {
        cli::cli_abort(
          c("x" = "Optimizer output is not a {.cls list}."),
          call = NULL
        )
      }
      if (!x[["optimizer_labels"]][["value"]] %in% names(opt_out)) {
        cli::cli_abort(
          c("x" = "Element {.var value} is not part of the optimizer output."),
          call = NULL
        )
      } else {
        value <- opt_out[[x[["optimizer_labels"]][["value"]]]]
        if (!(is.numeric(value) && length(value) == 1)) {
          cli::cli_abort(
            c("x" = "The optimal value is not a single {.cls numeric}."),
            call = NULL
          )
        }
      }
      if (!x[["optimizer_labels"]][["parameter"]] %in% names(opt_out)) {
        cli::cli_abort(
          c("x" = "Element {.var parameter} is not part of the optimizer
                   output."),
          call = NULL
        )
      } else {
        optimum <- opt_out[[x[["optimizer_labels"]][["parameter"]]]]
        if (!is.numeric(optimum)) {
          cli::cli_abort(
            c("x" = "The optimum is not a {.cls numeric}."),
            call = NULL
          )
        }
      }
    }
  }
  return(x)
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
#' A \code{function} to be optimized, returning a single \code{numeric}.
#' Its first argument must be a \code{numeric} vector of the same length as
#' \code{initial}, followed by any other arguments specified by the \code{...}
#' argument.
#' @param initial
#' A \code{numeric} vector with starting parameter values for the optimization.
#' @param ...
#' Additional arguments to be passed to \code{optimizer}.
#'
#' @return
#' A named \code{list}, containing at least these four elements:
#' \describe{
#'   \item{\code{value}}{A \code{numeric}, the value of the estimated optimum of
#'         \code{objective}.}
#'   \item{\code{parameter}}{A \code{numeric} vector, the parameter vector where
#'         the optimum of \code{objective} is obtained.}
#'   \item{\code{seconds}}{A \code{numeric}, the total optimization time in
#'         seconds.}
#'   \item{\code{initial}}{A \code{numeric}, the initial parameter values.}
#' }
#' Appended are additional output elements of the optimizer (if not excluded by
#' the \code{output_ignore} element via \code{\link{define_optimizer}}).
#'
#' @seealso
#' [define_optimizer()] for creating an \code{optimizer} object.
#'
#' @export
#'
#' @examples
#' apply_optimizer(optimizer_nlm(), function(x) x^4 + 3*x - 5, 2)

apply_optimizer <- function(
    optimizer = optimizer_nlm(), objective, initial, ...
) {
  optimizer$minimize(objective = objective, initial = initial, ...)
}

