#' Specify objective function
#'
#' @description
#' The \code{Objective} object specifies the framework for an objective function
#' for numerical optimization.
#'
#' @param f \[`function`\]\cr
#' A \code{function} to be optimized.
#'
#' It is expected that \code{f} has at least one \code{numeric} argument.
#'
#' Further, it is expected that the return value of \code{f} is of the
#' structure \code{numeric(1)}, i.e. a single \code{numeric} value (although
#' this can be altered via the \code{output_template} field).
#'
#' @param target \[`character()`\]\cr
#' The argument name(s) of \code{f} that get optimized.
#'
#' All target arguments must receive a \code{numeric} \code{vector}.
#'
#' Can be \code{NULL} (default), then it is the first argument of \code{f}.
#'
#' @param npar \[`integer()`\]\cr
#' The length of each target arguments, i.e., the length(s) of the
#' \code{numeric} \code{vector} argument(s) specified by \code{target}.
#'
#' @param ...
#' Optionally additional arguments to \code{f} that are fixed during
#' the optimization.
#'
#' @param overwrite \[`logical(1)`\]\cr
#' Allow overwriting?
#'
#' @param verbose \[`logical(1)`\]\cr
#' Print status messages?
#'
#' @param argument_name \[`character(1)`\]\cr
#' A name of an argument for \code{f}.
#'
#' @param .at \[`numeric()`\]\cr
#' The values for the target argument(s), written in a single vector (hence must
#' be of length \code{sum(self$npar)}).
#'
#' @param .negate \[`logical(1)`\]\cr
#' Negate the \code{numeric} return value of \code{f}?
#'
#' @return
#' An \code{Objective} object.
#'
#' @export
#'
#' @examples
#' ### define log-likelihood function of Gaussian mixture model
#' llk <- function(mu, sd, lambda, data) {
#'   sd <- exp(sd)
#'   lambda <- plogis(lambda)
#'   cluster_1 <- lambda * dnorm(data, mu[1], sd[1])
#'   cluster_2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
#'   sum(log(cluster_1 + cluster_2))
#' }
#'
#' ### the log-likelihood function is supposed to be optimized over the first
#' ### three arguments, the 'data' argument is constant
#' objective <- Objective$new(
#'   f = llk, target = c("mu", "sd", "lambda"), npar = c(2, 2, 1),
#'   data = faithful$eruptions
#' )
#'
#' ### evaluate the objective function at 1:5 (1:2 is passed to mu, 3:4 to sd,
#' ### and 5 to lambda)
#' objective$evaluate(1:5)

Objective <- R6::R6Class(

  classname = "Objective",

  public = list(

    #' @description
    #' Creates a new \code{Objective} object.

    initialize = function(f, target = NULL, npar, ...) {

      ### input checks
      checkmate::assert_function(f)
      if (is.null(target)) {
        target <- oeli::function_arguments(f, with_ellipsis = FALSE)[1]
      }
      checkmate::assert_character(target, any.missing = FALSE, min.len = 1)
      checkmate::assert_function(f, args = target)
      checkmate::assert_integerish(
        npar, lower = 1, any.missing = FALSE, len = length(target)
      )
      arguments <- list(...)
      arguments <- c(
        arguments,
        oeli::function_defaults(f, names(arguments))
      )

      ### define objective
      do.call(self$set_argument, c(arguments, list(verbose = FALSE)))
      self$objective_name <- oeli::variable_name(f)
      private$.f <- f
      private$.target <- target
      private$.npar <- npar
    },

    #' @description
    #' Set a fixed function argument.

    set_argument = function(..., overwrite = TRUE, verbose = self$verbose) {
      checkmate::assert_flag(overwrite)
      checkmate::assert_flag(verbose)
      arguments <- list(...)
      checkmate::check_names(arguments, type = "strict")
      argument_names <- names(arguments)
      for (i in seq_along(arguments)) {
        if (argument_names[i] %in% names(private$.arguments)) {
          if (!overwrite) {
            cli::cli_abort(
              "Argument {.var {argument_names[i]}} already exists, call
               {.var $set_argument(..., {.val overwrite = TRUE})} to overwrite.",
              call = NULL
            )
          } else {
            if (verbose) {
              cli::cli_alert("Overwriting argument {.val {argument_names[i]}}.")
            }
          }
        } else {
          if (verbose) {
            cli::cli_alert("Setting argument {.val {argument_names[i]}}.")
          }
        }
        private$.arguments[argument_names[i]] <- arguments[i]
      }
      invisible(self)
    },

    #' @description
    #' Get a fixed function argument.

    get_argument = function(argument_name, verbose = self$verbose) {
      private$.check_argument_specified(argument_name, verbose = verbose)
      checkmate::assert_flag(verbose)
      if (verbose) {
        cli::cli_alert("Returning argument {.val {argument_name}}.")
      }
      private$.arguments[[argument_name]]
    },

    #' @description
    #' Remove a fixed function argument.

    remove_argument = function(argument_name, verbose = self$verbose) {
      private$.check_argument_specified(argument_name, verbose = verbose)
      checkmate::assert_flag(verbose)
      if (verbose) {
        cli::cli_alert("Removing argument {.val {argument_name}}.")
      }
      private$.arguments[[argument_name]] <- NULL
      invisible(self)
    },

    #' @description
    #' Validate an \code{Objective} object.

    validate = function(.at) {
      if (missing(.at)) {
        cli::cli_abort(
          "Please specify the argument {.var .at}",
          call = NULL
        )
      }
      private$.check_target(.at, verbose = TRUE)
      private$.check_arguments_complete(verbose = TRUE)
      cli::cli_progress_step(
        "Evaluating the function.",
        msg_done = "The function output has the expected structure, great!",
        msg_failed = "Test evaluation failed."
      )
      output <- self$evaluate(.at = .at)
      if (!oeli::identical_structure(output, private$.output_template)) {
        stop(
          "The function output does not have the expected structure.",
          call. = FALSE
        )
      }
      cli::cli_progress_done()
      invisible(self)
    },

    #' @description
    #' Evaluate the objective function.

    evaluate = function(.at, .negate = FALSE, ...) {
      private$.check_target(.at, verbose = FALSE)
      checkmate::assert_flag(.negate)
      splits <- c(0, cumsum(private$.npar))
      .at <- structure(
        lapply(seq_along(splits)[-1], function(i) {
          .at[(splits[i - 1] + 1):(splits[i])]
        }),
        names = private$.target
      )
      setTimeLimit(cpu = self$seconds, elapsed = self$seconds, transient = TRUE)
      on.exit({
        setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
      })
      args <- c(.at, oeli::merge_lists(list(...), private$.arguments))
      tryCatch(
        {
          suppressWarnings(
            value <- do.call(what = private$.f, args = args),
            classes = if (self$hide_warnings) "warning" else ""
          )
          if (.negate) -value else value
        },
        error = function(e) {
          msg <- e$message
          if (grepl("reached elapsed time limit|reached CPU time limit", msg)) {
            return("time limit reached")
          } else {
            cli::cli_abort(
              paste("Function evaluation threw an error:", msg),
              call = NULL
            )
          }
        }
      )
    },

    #' @description
    #' Print details of the \code{Objective} object.

    print = function() {
      cli::cat_bullet(c(
        paste(
          "Function:", private$.objective_name
        ),
        paste(
          "Definition:", oeli::function_body(private$.f, nchar = 40)
        ),
        paste(
          "Targets (length):",
          paste(
            paste0(private$.target, " (", private$.npar, ")"), collapse = ", "
          )
        ),
        paste(
          "Fixed arguments specified:",
          paste(names(private$.arguments), collapse = ", ")
        )
      ))
      invisible(self)
    }

  ),

  active = list(

    #' @field objective_name \[`character(1)`\]\cr
    #' The label for the objective function.

    objective_name = function(value) {
      if (missing(value)) {
        return(private$.objective_name)
      } else {
        checkmate::assert_string(value)
        private$.objective_name <- value
      }
    },

    #' @field fixed_arguments \[`character()`\]\cr
    #' The name(s) of the fixed argument(s) (if any).
    fixed_arguments = function(value) {
      if (missing(value)) {
        names(private$.arguments)
      } else {
        cli::cli_abort(
          "Field {.var fixed_arguments} is read-only.",
          call = NULL
        )
      }
    },

    #' @field seconds \[`numeric(1)`\]\cr
    #' A time limit in seconds. Computations are interrupted
    #' prematurely if \code{seconds} is exceeded.
    #'
    #' No time limit if \code{seconds = Inf} (the default).
    #'
    #' Note the limitations documented in \code{\link[base]{setTimeLimit}}.

    seconds = function(value) {
      if (missing(value)) {
        return(private$.seconds)
      } else {
        checkmate::assert_number(value, lower = 0, finite = FALSE)
        private$.seconds <- value
      }
    },

    #' @field hide_warnings \[`logical(1)`\]\cr
    #' Hide warnings when evaluating the objective function?

    hide_warnings = function(value) {
      if (missing(value)) {
        return(private$.hide_warnings)
      } else {
        checkmate::assert_flag(value)
        private$.hide_warnings <- value
      }
    },

    #' @field verbose \[`logical(1)`\]\cr
    #' Print status messages?

    verbose = function(value) {
      if (missing(value)) {
        return(private$.verbose)
      } else {
        checkmate::assert_flag(value)
        private$.verbose <- value
      }
    },

    #' @field npar \[`integer()`\]\cr
    #' The length of each target argument.

    npar = function(value) {
      if (missing(value)) {
        structure(private$.npar, names = private$.target)
      } else {
        cli::cli_abort(
          "Field {.var npar} is read-only.",
          call = NULL
        )
      }
    },

    #' @field output_template \[`any`\]\cr
    #' A template of the expected output value, used for the \code{validate}
    #' method.

    output_template = function(value) {
      if (missing(value)) {
        private$.output_template
      } else {
        private$.output_template <- value
      }
    }

  ),

  private = list(

    .f = NULL,
    .objective_name = character(),
    .target = character(),
    .npar = integer(),
    .arguments = list(),
    .seconds = Inf,
    .hide_warnings = FALSE,
    .verbose = TRUE,
    .output_template = numeric(1),

    ### helper function that checks the target argument
    .check_target = function(.at, verbose = self$verbose) {
      if (!checkmate::test_numeric(
        .at, any.missing = FALSE, len = sum(private$.npar)
      )) {
        variable_name <- oeli::variable_name(.at, fallback = ".at")
        cli::cli_abort(
          "Input {.var {variable_name}} must be a {.cls numeric} of length
          {sum(private$.npar)}.",
          call = NULL
        )
      }
      if (verbose) {
        cli::cli_alert_success(
          "The value{?s} for the {length(private$.npar)} target argument{?s}
          {?is/are} correctly specified."
        )
      }
      invisible(TRUE)
    },

    ### helper function that checks if a function argument is specified
    .check_argument_specified = function(
      argument_name, verbose = self$verbose
    ) {
      checkmate::assert_string(argument_name)
      if (!argument_name %in% names(private$.arguments)) {
        cli::cli_abort(
          "Function argument {.var {argument_name}} is required but not
          specified, please call
          {.var $set_argument({.val {argument_name}} = ...)} first.",
          call = NULL
        )
      }
      if (verbose) {
        cli::cli_alert_success(
          "Required argument {.val {argument_name}} is specified."
        )
      }
    },

    ### helper function that checks if all required arguments are specified
    .check_arguments_complete = function(verbose = self$verbose) {
      arguments_required <- oeli::function_arguments(
        private$.f, with_default = FALSE, with_ellipsis = FALSE
      )
      for (argument_name in setdiff(arguments_required, private$.target)) {
        private$.check_argument_specified(argument_name, verbose = FALSE)
      }
      if (verbose) {
        cli::cli_alert_success(
          "All required fixed arguments are specified."
        )
      }
    }

  )

)
