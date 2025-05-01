#' Specify objective function object
#'
#' @description
#' The \code{Objective} object specifies the framework for an objective function
#' for numerical optimization.
#'
#' @param target \[`character()`\]\cr
#' The argument name(s) that get optimized.
#'
#' All target arguments must be \code{numeric}.
#'
#' Can be \code{NULL} (default), then the first function argument is selected.
#'
#' @param npar \[`integer()`\]\cr
#' The length of each target argument, i.e., the length(s) of the
#' \code{numeric} \code{vector} argument(s) specified by \code{target}.
#'
#' @param ...
#' Optionally additional function arguments that are fixed during the
#' optimization.
#'
#' @param .verbose \[`logical(1)`\]\cr
#' Print status messages?
#'
#' @param argument_name \[`character(1)`\]\cr
#' A function argument name.
#'
#' @param .at \[`numeric()`\]\cr
#' The values for the target argument(s), written in a single vector.
#'
#' Must be of length \code{sum(self$npar)}.
#'
#' @param .negate \[`logical(1)`\]\cr
#' Negate the function return value?
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
#' ### optimize over the first three arguments, the 'data' argument is constant
#' objective <- Objective$new(
#'   f = llk, target = c("mu", "sd", "lambda"), npar = c(2, 2, 1),
#'   data = faithful$eruptions
#' )
#'
#' ### evaluate at 1:5 (1:2 is passed to mu, 3:4 to sd, and 5 to lambda)
#' objective$evaluate(1:5)

Objective <- R6::R6Class(

  classname = "Objective",

  public = list(

    #' @description
    #' Creates a new \code{Objective} object.
    #'
    #' @param f \[`function`\]\cr
    #' A \code{function} to be optimized.
    #'
    #' It is expected that \code{f} has at least one \code{numeric} argument.
    #'
    #' Further, it is expected that the return value of \code{f} is of the
    #' structure \code{numeric(1)}, i.e. a single \code{numeric} value.

    initialize = function(f, target = NULL, npar, ...) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(f),
        var_name = "f"
      )
      oeli::input_check_response(
        check = checkmate::check_function(f),
        var_name = "f"
      )
      if (is.null(target)) {
        target <- oeli::function_arguments(f, with_ellipsis = FALSE)[1]
      }
      oeli::input_check_response(
        check = checkmate::check_character(
          target, any.missing = FALSE, min.len = 1
        ),
        var_name = "target"
      )
      oeli::input_check_response(
        check = checkmate::check_function(f, args = target),
        var_name = "f"
      )
      oeli::input_check_response(
        check = oeli::check_missing(npar),
        var_name = "npar"
      )
      oeli::input_check_response(
        check = checkmate::check_integerish(
          npar, lower = 1, any.missing = FALSE, len = length(target)
        ),
        var_name = "f"
      )
      arguments <- list(...)
      arguments <- c(
        arguments,
        oeli::function_defaults(f, names(arguments))
      )

      ### define objective
      do.call(self$set_argument, c(arguments, list(.verbose = FALSE)))
      self$objective_name <- oeli::variable_name(f)
      private$.f <- f
      private$.target <- target
      private$.npar <- npar

    },

    #' @description
    #' Set a function argument that remains fixed during optimization.
    #'
    #' @param .overwrite \[`logical(1)`\]\cr
    #' Overwrite existing values?

    set_argument = function(..., .overwrite = TRUE, .verbose = self$verbose) {

      ### input checks
      oeli::input_check_response(
        check = checkmate::check_flag(.overwrite),
        var_name = ".overwrite"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(.verbose),
        var_name = ".verbose"
      )
      arguments <- list(...)
      if (length(arguments) > 0) {
        oeli::input_check_response(
          check = checkmate::check_names(names(arguments), type = "strict"),
          var_name = "..."
        )
      }

      ### set arguments
      argument_names <- names(arguments)
      for (i in seq_along(arguments)) {
        if (argument_names[i] %in% names(private$.arguments)) {
          if (!.overwrite) {
            cli::cli_abort(
              "Argument {.var {argument_names[i]}} already exists, call
               {.var $set_argument(..., {.val .overwrite = TRUE})} to
              overwrite.",
              call = NULL
            )
          } else {
            if (.verbose) {
              cli::cli_alert("Overwriting argument {.val {argument_names[i]}}.")
            }
          }
        } else {
          if (.verbose) {
            cli::cli_alert("Setting argument {.val {argument_names[i]}}.")
          }
        }
        private$.arguments[argument_names[i]] <- arguments[i]
      }

      ### synchronize with gradient and Hessian (if available)
      private$.sync_arguments(.verbose = .verbose)

      invisible(self)

    },

    #' @description
    #' Get a fixed function argument.

    get_argument = function(argument_name, .verbose = self$verbose) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(argument_name),
        var_name = "argument_name"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(.verbose),
        var_name = ".verbose"
      )
      private$.check_argument_specified(argument_name, .verbose = .verbose)

      ### get argument
      if (.verbose) {
        cli::cli_alert("Returning argument {.val {argument_name}}.")
      }
      private$.arguments[[argument_name]]

    },

    #' @description
    #' Remove a fixed function argument.

    remove_argument = function(argument_name, .verbose = self$verbose) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(argument_name),
        var_name = "argument_name"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(.verbose),
        var_name = ".verbose"
      )
      private$.check_argument_specified(argument_name, .verbose = .verbose)

      ### remove argument
      if (.verbose) {
        cli::cli_alert("Removing argument {.val {argument_name}}.")
      }
      private$.arguments[[argument_name]] <- NULL

      ### synchronize with gradient and Hessian (if available)
      private$.sync_arguments(.verbose = .verbose)

      invisible(self)

    },

    #' @description
    #' Set a gradient function.
    #'
    #' @param gradient \[`function`\]\cr
    #' A \code{function} that computes the gradient of the objective function
    #' \code{f}.
    #'
    #' It is expected that \code{gradient} has the same call as \code{f}, and
    #' that \code{gradient} returns a \code{numeric} \code{vector} of length
    #' \code{self$npar}.

    set_gradient = function(
      gradient, target = self$target, npar = self$npar, ...,
      .verbose = self$verbose
    ) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(gradient),
        var_name = "gradient"
      )

      ### determine arguments
      arguments <- oeli::merge_lists(list(...), private$.arguments)

      ### setting gradient
      private$.gradient <- do.call(
        Objective$new,
        c(list(f = gradient, target = target, npar = npar), arguments)
      )
      private$.gradient$objective_name <- oeli::variable_name(
        gradient, fallback = "gradient"
      )
      if (.verbose) {
        cli::cli_alert("Setting gradient function.")
      }

      invisible(self)
    },

    #' @description
    #' Set a Hessian function.
    #'
    #' @param hessian \[`function`\]\cr
    #' A \code{function} that computes the Hessian of the objective function
    #' \code{f}.
    #'
    #' It is expected that \code{hessian} has the same call as \code{f}, and
    #' that \code{hessian} returns a \code{numeric} \code{matrix} of dimension
    #' \code{self$npar} times \code{self$npar}.

    set_hessian = function(
      hessian, target = self$target, npar = self$npar, ...,
      .verbose = self$verbose
    ) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(hessian),
        var_name = "hessian"
      )

      ### determine arguments
      arguments <- oeli::merge_lists(list(...), private$.arguments)

      ### setting Hessian
      private$.hessian <- do.call(
        Objective$new,
        c(list(f = hessian, target = target, npar = npar), arguments)
      )
      private$.hessian$objective_name <- oeli::variable_name(
        hessian, fallback = "hessian"
      )
      if (.verbose) {
        cli::cli_alert("Setting Hessian function.")
      }

      invisible(self)
    },

    #' @description
    #' Evaluate the objective function.
    #'
    #' @param .gradient_as_attribute \[`logical(1)\]\cr
    #' Add the value of the gradient function as an attribute to the output?
    #'
    #' The attribute name is defined via the `.gradient_attribute_name`
    #' argument.
    #'
    #' Ignored if `$gradient_specified` is `FALSE`.
    #'
    #' @param .gradient_attribute_name \[`character(1)\]\cr
    #' Only relevant if `.gradient_as_attribute = TRUE`.
    #'
    #' In that case, the attribute name for the gradient (if available).
    #'
    #' @param .hessian_as_attribute \[`logical(1)\]\cr
    #' Add the value of the Hessian function as an attribute to the output?
    #'
    #' The attribute name is defined via the `.hessian_attribute_name`
    #' argument.
    #'
    #' Ignored if `$hessian_specified` is `FALSE`.
    #'
    #' @param .hessian_attribute_name \[`character(1)\]\cr
    #' Only relevant if `.hessian_as_attribute = TRUE`.
    #'
    #' In that case, the attribute name for the Hessian (if available).

    evaluate = function(
      .at,
      .negate = FALSE,
      .gradient_as_attribute = FALSE,
      .gradient_attribute_name = "gradient",
      .hessian_as_attribute = FALSE,
      .hessian_attribute_name = "hessian",
      ...
    ) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(.at),
        var_name = ".at"
      )
      private$.check_target(.at)
      oeli::input_check_response(
        check = checkmate::check_flag(.negate),
        var_name = ".negate"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(.gradient_as_attribute),
        var_name = ".gradient_as_attribute"
      )
      oeli::input_check_response(
        check = checkmate::check_string(
          .gradient_attribute_name, na.ok = !.gradient_as_attribute
        ),
        var_name = ".gradient_attribute_name"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(.hessian_as_attribute),
        var_name = ".hessian_as_attribute"
      )
      oeli::input_check_response(
        check = checkmate::check_string(
          .hessian_attribute_name, na.ok = !.hessian_as_attribute
        ),
        var_name = ".hessian_attribute_name"
      )

      ### evaluation
      splits <- c(0, cumsum(private$.npar))
      .at_split <- structure(
        lapply(seq_along(splits)[-1], function(i) {
          .at[(splits[i - 1] + 1):(splits[i])]
        }),
        names = private$.target
      )
      setTimeLimit(cpu = self$seconds, elapsed = self$seconds, transient = TRUE)
      on.exit({
        setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
      })
      args <- c(.at_split, oeli::merge_lists(list(...), private$.arguments))
      out <- tryCatch(
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

      ### add gradient and Hessian
      if (isTRUE(.gradient_as_attribute) && self$gradient_specified) {
        attr(out, .gradient_attribute_name) <- self$evaluate_gradient(
          .at = .at, .negate = .negate, ...
        )
      }
      if (isTRUE(.hessian_as_attribute) && self$hessian_specified) {
        attr(out, .hessian_attribute_name) <- self$evaluate_hessian(
          .at = .at, .negate = .negate, ...
        )
      }

      ### return
      return(out)

    },

    #' @description
    #' Evaluate the gradient function.

    evaluate_gradient = function(.at, .negate = FALSE, ...) {
      if (self$gradient_specified) {
        oeli::input_check_response(
          check = oeli::check_missing(.at),
          var_name = ".at"
        )
        private$.gradient$evaluate(.at = .at, .negate = .negate, ...)
      } else {
        cli::cli_abort(paste(
            "Gradient function is required but not specified, please call",
            "{.var $set_gradient()} first."
          ),
          call = NULL
        )
      }
    },

    #' @description
    #' Evaluate the Hessian function.

    evaluate_hessian = function(.at, .negate = FALSE, ...) {
      if (self$hessian_specified) {
        oeli::input_check_response(
          check = oeli::check_missing(.at),
          var_name = ".at"
        )
        private$.hessian$evaluate(.at = .at, .negate = .negate, ...)
      } else {
        cli::cli_abort(paste(
            "Hessian function is required but not specified, please call",
            "{.var $set_hessian()} first."
          ),
          call = NULL
        )
      }
    },

    #' @description
    #' Print details of the \code{Objective} object.

    print = function() {
      cli::cat_bullet(c(
        paste(
          "Function:", private$.objective_name
        ),
        paste(
          "Targets (length):",
          paste(
            paste0(private$.target, " (", private$.npar, ")"), collapse = ", "
          )
        ),
        paste(
          "Fixed arguments:",
          if(length(private$.arguments) == 0) {
            "none"
          } else {
            paste(names(private$.arguments), collapse = ", ")
          },
          collapse = ", "
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
        oeli::input_check_response(
          check = checkmate::check_string(value),
        )
        private$.objective_name <- value
      }
    },

    #' @field fixed_arguments \[`character()`, read-only\]\cr
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
        oeli::input_check_response(
          check = checkmate::check_number(value, lower = 0, finite = FALSE)
        )
        private$.seconds <- value
      }
    },

    #' @field hide_warnings \[`logical(1)`\]\cr
    #' Hide warnings when evaluating the objective function?

    hide_warnings = function(value) {
      if (missing(value)) {
        return(private$.hide_warnings)
      } else {
        oeli::input_check_response(
          check = checkmate::check_flag(value)
        )
        private$.hide_warnings <- value
      }
    },

    #' @field verbose \[`logical(1)`\]\cr
    #' Print status messages?

    verbose = function(value) {
      if (missing(value)) {
        return(private$.verbose)
      } else {
        oeli::input_check_response(
          check = checkmate::check_flag(value),
          var_name = "verbose"
        )
        private$.verbose <- value
      }
    },

    #' @field npar \[`integer()`, read-only\]\cr
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

    #' @field target \[`character()`, read-only\]\cr
    #' The argument name(s) that get optimized.

    target = function(value) {
      if (missing(value)) {
        private$.target
      } else {
        cli::cli_abort(
          "Field {.var target} is read-only.",
          call = NULL
        )
      }
    },

    #' @field gradient_specified \[`logical(1)`, read-only\]\cr
    #' Whether a gradient function has been specified via `$set_gradient()`.

    gradient_specified = function(value) {
      if (missing(value)) {
        !is.null(private$.gradient)
      } else {
        cli::cli_abort(
          "Field {.var gradient_specified} is read-only.",
          call = NULL
        )
      }
    },

    #' @field hessian_specified \[`logical(1)`, read-only\]\cr
    #' Whether a Hessian function has been specified via `$set_hessian()`.

    hessian_specified = function(value) {
      if (missing(value)) {
        !is.null(private$.hessian)
      } else {
        cli::cli_abort(
          "Field {.var hessian_specified} is read-only.",
          call = NULL
        )
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
    .verbose = getOption("verbose", default = FALSE),
    .gradient = NULL,
    .hessian = NULL,

    ### helper function that checks the target argument
    .check_target = function(.at, .verbose = self$verbose) {

      oeli::input_check_response(
        check = checkmate::check_numeric(
          .at, any.missing = FALSE, len = sum(private$.npar)
        ),
        var_name = oeli::variable_name(.at, fallback = ".at")
      )
      oeli::input_check_response(
        check = checkmate::check_flag(.verbose),
        var_name = ".verbose"
      )
      if (.verbose) {
        cli::cli_alert_success(paste(
          "The value{?s} for the {length(private$.npar)} target argument{?s}",
          "{?is/are} correctly specified."
        ))
      }
      invisible(TRUE)

    },

    ### helper function that checks if a function argument is specified
    .check_argument_specified = function(
      argument_name, .verbose = self$verbose
    ) {

      oeli::input_check_response(
        check = checkmate::check_string(argument_name),
        var_name = "argument_name"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(.verbose),
        var_name = ".verbose"
      )

      if (!argument_name %in% names(private$.arguments)) {
        cli::cli_abort(paste(
            "Function argument {.var {argument_name}} is required but not",
            "specified, please call",
            "{.var $set_argument({.val {argument_name}} = ...)} first."
          ),
          call = NULL
        )
      }
      if (.verbose) {
        cli::cli_alert_success(
          "Required argument {.val {argument_name}} is specified."
        )
      }
      invisible(TRUE)

    },

    ### helper function that checks if all required arguments are specified
    .check_arguments_complete = function(.verbose = self$verbose) {

      oeli::input_check_response(
        check = checkmate::check_flag(.verbose),
        var_name = ".verbose"
      )
      arguments_required <- oeli::function_arguments(
        private$.f, with_default = FALSE, with_ellipsis = FALSE
      )
      for (argument_name in setdiff(arguments_required, private$.target)) {
        private$.check_argument_specified(argument_name, .verbose = FALSE)
      }
      if (.verbose) {
        cli::cli_alert_success(
          "All required fixed arguments are specified."
        )
      }

    },

    ### helper function to synchronize arguments with gradient and Hessian
    ### function (if specified)
    .sync_arguments = function(.verbose = self$verbose) {

        if (!is.null(private$.gradient)) {
          private$.gradient$.__enclos_env__$private$.arguments <-
            private$.arguments
          if (.verbose) {
            cli::cli_alert("Synchronized arguments with gradient function.")
          }
        }

        if (!is.null(private$.gradient)) {
          private$.hessian$.__enclos_env__$private$.arguments <-
            private$.arguments
          if (.verbose) {
            cli::cli_alert("Synchronized arguments with Hessian function.")
          }
        }

    }

  )

)
