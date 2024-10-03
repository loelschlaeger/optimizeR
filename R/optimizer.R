#' Specify numerical optimizer object
#'
#' @description
#' The \code{Optimizer} object defines a numerical optimizer based on an
#' optimization algorithm implemented in R.
#'
#' The main advantage of working with an \code{Optimizer} object instead of
#' using the optimization function directly lies in the standardized inputs and
#' outputs.
#'
#' Any R function that fulfills the following four constraints can be defined as
#' an \code{Optimizer} object:
#' 1. It must have an input for a \code{function}, the objective function to be
#'    optimized.
#' 2. It must have an input for a \code{numeric} vector, the initial values from
#'    where the optimizer starts.
#' 3. It must have a \code{...} argument for additional parameters passed on to
#'    the objective function.
#' 4. The output must be a named \code{list}, including the optimal function
#'    value and the optimal parameter vector.
#'
#' @param objective \[`function` | `Objective`\]\cr
#' A \code{function} to be optimized that
#'
#' 1. has at least one argument that receives a \code{numeric} \code{vector}
#' 2. and returns a single \code{numeric} value.
#'
#' Alternatively, it can also be a \code{\link{Objective}} object for more
#' flexibility.
#'
#' @param initial \[`numeric()`\]\cr
#' Starting parameter values for the optimization.
#'
#' @param ... \[`any`\]\cr
#' Optionally additional named arguments to be passed to the optimizer
#' algorithm. Without specifications, default values are used.
#'
#' @param direction \[`character(1)`\]\cr
#' Either \code{"min"} for minimization or \code{"max"} for maximization.
#'
#' @param .verbose \[`logical(1)`\]\cr
#' Print status messages?
#'
#' @examples
#' ### Task: compare minimization with 'stats::nlm' and 'pracma::nelder_mead'
#'
#' # 1. define objective function and initial values
#' objective <- TestFunctions::TF_ackley
#' initial <- c(3, 3)
#'
#' # 2. get overview of optimizers in dictionary
#' optimizer_dictionary$keys
#'
#' # 3. define 'nlm' optimizer
#' nlm <- Optimizer$new(which = "stats::nlm")
#'
#' # 4. define the 'pracma::nelder_mead' optimizer (not contained in the dictionary)
#' nelder_mead <- Optimizer$new(which = "custom")
#' nelder_mead$definition(
#'   algorithm = pracma::nelder_mead, # the optimization function
#'   arg_objective = "fn",            # the argument name for the objective function
#'   arg_initial = "x0",              # the argument name for the initial values
#'   out_value = "fmin",              # the element for the optimal function value in the output
#'   out_parameter = "xmin",          # the element for the optimal parameters in the output
#'   direction = "min"                # the optimizer minimizes
#' )
#'
#' # 5. compare the minimization results
#' nlm$minimize(objective, initial)
#' nelder_mead$minimize(objective, initial)
#'
#' @export

Optimizer <- R6::R6Class(

  classname = "Optimizer",

  public = list(

    #' @description
    #' Initializes a new \code{Optimizer} object.
    #'
    #' @param which
    #' A \code{character}, either one of \code{optimizer_dictionary$keys} or
    #' \code{"custom"} (in which case \code{$definition()} must be used to
    #' define the optimizer details).
    #'
    #' @return
    #' A new \code{Optimizer} object.

    initialize = function(which, ..., .verbose = TRUE) {

      ### input checks
      oeli::input_check_response(
        check = checkmate::check_string(which),
        var_name = "which"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(.verbose),
        var_name = ".verbose"
      )

      ### initialize optimizer
      if (identical(which, "custom")) {
        if (.verbose) {
          cli::cli_inform(
            "Use method {.fun $definition} next to define a custom optimizer."
          )
        }
      } else {
        checkmate::assert_choice(which, choices = optimizer_dictionary$keys)
        if (which %in% optimizer_dictionary$keys) {
          self$definition(
            algorithm = optimizer_dictionary$get(key = which, value = "algorithm"),
            arg_objective = optimizer_dictionary$get(key = which, value = "arg_objective"),
            arg_initial = optimizer_dictionary$get(key = which, value = "arg_initial"),
            out_value = optimizer_dictionary$get(key = which, value = "out_value"),
            out_parameter = optimizer_dictionary$get(key = which, value = "out_parameter"),
            direction = optimizer_dictionary$get(key = which, value = "direction")
          )
          self$label <- which
        }
        self$set_arguments(...)
      }
    },

    #' @description
    #' Defines an optimizer.
    #'
    #' @param algorithm \[`function`\]\cr
    #' The optimization algorithm.
    #'
    #' @param arg_objective \[`character(1)`\]\cr
    #' The argument name for the objective function in \code{algorithm}.
    #'
    #' @param arg_initial \[`character(1)`\]\cr
    #' The argument name for the initial values in \code{algorithm}.
    #'
    #' @param arg_lower \[`character(1)` | `NA`\]\cr
    #' Optionally the argument name for the lower parameter bound in
    #' \code{algorithm}.
    #'
    #' Can be `NA` if not available.
    #'
    #' @param arg_upper \[`character(1)` | `NA`\]\cr
    #' Optionally the argument name for the upper parameter bound in
    #' \code{algorithm}.
    #'
    #' Can be `NA` if not available.
    #'
    #' @param arg_gradient \[`character(1)` | `NA`\]\cr
    #' Optionally the argument name for the gradient function in
    #' \code{algorithm}.
    #'
    #' Can be `NA` if not available.
    #'
    #' @param arg_hessian \[`character(1)` | `NA`\]\cr
    #' Optionally the argument name for the Hessian function in
    #' \code{algorithm}.
    #'
    #' Can be `NA` if not available.
    #'
    #' @param gradient_as_attribute \[`logical(1)`\]\cr
    #' Only relevant if `arg_gradient` is not `NA`.
    #'
    #' In that case, does `algorithm` expect that the gradient is an attribute
    #' of the objective function output (as for example in
    #' \code{\link[stats]{nlm}})? In that case, `arg_gradient` defines the
    #' attribute name.
    #'
    #' @param out_value \[`character(1)`\]\cr
    #' The element name for the optimal function value in the output \code{list}
    #' of \code{algorithm}.
    #'
    #' @param out_parameter \[`character(1)`\]\cr
    #' The element name for the optimal parameters in the output \code{list} of
    #' \code{algorithm}.
    #'
    #' @param direction \[`character(1)`\]\cr
    #' Either \code{"min"} (if the optimizer minimizes) or \code{"max"}
    #' (if the optimizer maximizes).

    definition = function(
      algorithm,
      arg_objective,
      arg_initial,
      arg_lower = NA_character_,
      arg_upper = NA_character_,
      arg_gradient = NA_character_,
      arg_hessian = NA_character_,
      gradient_as_attribute = FALSE,
      hessian_as_attribute = TRUE,
      out_value,
      out_parameter,
      direction
    ) {
      if (missing(algorithm)) {
        cli::cli_abort(c(
          "x" = "Please specify argument {.var algorithm}.",
          "*" = "It should be a {.cls function} that can perform numerical
                 optimization."
        ), call = NULL)
      }
      self$algorithm <- algorithm
      self$label <- oeli::variable_name(algorithm)
      if (missing(arg_objective)) {
        cli::cli_abort(c(
          "x" = "Please specify argument {.var arg_objective}.",
          "*" = "It should be the name of the function argument of
                 {.var algorithm}."
        ), call = NULL)
      }
      self$arg_objective <- arg_objective
      if (missing(arg_initial)) {
        cli::cli_abort(c(
          "x" = "Please specify argument {.var arg_initial}.",
          "*" = "It should be the name of the initial value argument of
                 {.var algorithm}."
        ), call = NULL)
      }
      self$arg_initial <- arg_initial
      if (missing(out_value)) {
        cli::cli_abort(c(
          "x" = "Please specify argument {.var out_value}.",
          "*" = "It should be the name of the optimal function value in the
                 output list of {.var algorithm}."
        ), call = NULL)
      }
      self$out_value <- out_value
      if (missing(out_parameter)) {
        cli::cli_abort(c(
          "x" = "Please specify argument {.var out_parameter}.",
          "*" = "It should be the name of the optimal parameter vector in the
                 output list of {.var algorithm}."
        ), call = NULL)
      }
      self$out_parameter <- out_parameter
      if (missing(direction)) {
        cli::cli_abort(c(
          "x" = "Please specify argument {.var direction}.",
          "*" = "It indicates whether the optimizer minimizes ({.val min}) or
                 maximizes ({.val max})."
        ), call = NULL)
      }
      self$direction <- direction
      args_available <- oeli::function_arguments(
        self$algorithm, with_default = TRUE, with_ellipsis = TRUE
      )
      if (!self$arg_objective %in% args_available) {
        cli::cli_warn(
          "The optimizer needs to have the argument {.val {self$arg_objective}}."
        )
      }
      if (!self$arg_initial %in% args_available) {
        cli::cli_warn(
          "The optimizer needs to have the argument {.val {self$arg_initial}}."
        )
      }
      if (!"..." %in% args_available) {
        cli::cli_warn(
          "The optimizer needs to have an ellipsis argument."
        )
      }
      invisible(self)
    },

    #' @description
    #' Sets optimizer arguments.
    #' @return
    #' The \code{Optimizer} object.

    set_arguments = function(...) {
      arguments <- list(...)
      private$.arguments[names(arguments)] <- arguments
    },

    #' @description
    #' Validates the \code{Optimizer} object. A time limit in seconds for
    #' the optimization can be set via the \code{$seconds} field.
    #' @return
    #' The \code{Optimizer} object.

    validate = function(
      objective = optimizeR::test_objective, initial = round(stats::rnorm(2)),
      ..., direction = "min"
    ) {

      ### test objective
      objective <- private$.build_objective(objective, initial)
      cli::cli_progress_step(
        "Checking the objective.",
        msg_done = "The objective is fine."
      )
      objective$validate(.at = initial)

      ### test optimization
      cli::cli_progress_step(
        paste0("Trying to ", direction, "imize.")
      )
      out <- private$.optimize(
        objective = objective,
        initial = initial,
        additional_arguments = list(...),
        direction = direction
      )

      ### output checks
      if (isTRUE(out[["time_out"]])) {
        cli::cli_alert_warning(
          "Time limit reached, consider increasing {.val $seconds}."
        )
      } else if (!is.null(out[["error_message"]])) {
        cli::cli_abort(
          "Test optimization failed: {out[['error_message']]}"
        )
      }
      cli::cli_progress_step(
        "Checking that the output is a list.",
        msg_done = "The output is the required list."
      )
      if (!is.list(out)) {
        cli::cli_abort(
          "Test optimization did not return a {.cls list}."
        )
      }
      values_required <- c("value", "parameter", "seconds", "initial")
      cli::cli_progress_step(
        "Checking for output elements {values_required}.",
        msg_done = "All required elements are in the output."
      )
      for (value in values_required) {
        if (!value %in% names(out)) {
          cli::cli_abort("Output does not contain the element {.var {value}}.")
        }
      }

      ### finish
      cli::cli_progress_done()
      invisible(self)
    },

    #' @description
    #' Performing minimization.
    #' @return
    #' A named \code{list}, containing at least these five elements:
    #' \describe{
    #'   \item{\code{value}}{A \code{numeric}, the minimum function value.}
    #'   \item{\code{parameter}}{A \code{numeric} vector, the parameter vector
    #'   where the minimum is obtained.}
    #'   \item{\code{seconds}}{A \code{numeric}, the optimization time in seconds.}
    #'   \item{\code{initial}}{A \code{numeric}, the initial parameter values.}
    #'   \item{\code{error}}{Either \code{TRUE} if an error occurred, or \code{FALSE}, else.}
    #' }
    #' Appended are additional output elements of the optimizer.
    #'
    #' If an error occurred, then the error message is also appended as element
    #' \code{error_message}.
    #'
    #' If the time limit was exceeded, this also counts as an error. In addition,
    #' the flag \code{time_out = TRUE} is appended.
    #' @examples
    #' Optimizer$new("stats::nlm")$
    #'   minimize(objective = function(x) x^4 + 3*x - 5, initial = 2)

    minimize = function(objective, initial, ...) {
      private$.optimize(
        objective = objective, initial = initial,
        additional_arguments = list(...), direction = "min"
      )
    },

    #' @description
    #' Performing maximization.
    #'
    #' @return
    #' A named \code{list}, containing at least these five elements:
    #' \describe{
    #'   \item{\code{value}}{A \code{numeric}, the maximum function value.}
    #'   \item{\code{parameter}}{A \code{numeric} vector, the parameter vector
    #'   where the maximum is obtained.}
    #'   \item{\code{seconds}}{A \code{numeric}, the optimization time in seconds.}
    #'   \item{\code{initial}}{A \code{numeric}, the initial parameter values.}
    #'   \item{\code{error}}{Either \code{TRUE} if an error occurred, or \code{FALSE}, else.}
    #' }
    #' Appended are additional output elements of the optimizer.
    #'
    #' If an error occurred, then the error message is also appended as element
    #' \code{error_message}.
    #'
    #' If the time limit was exceeded, this also counts as an error. In addition,
    #' the flag \code{time_out = TRUE} is appended.
    #'
    #' @examples
    #' Optimizer$new("stats::nlm")$
    #'   maximize(objective = function(x) -x^4 + 3*x - 5, initial = 2)

    maximize = function(objective, initial, ...) {
      private$.optimize(
        objective = objective, initial = initial,
        additional_arguments = list(...), direction = "max"
      )
    },

    #' @description
    #' Performing minimization or maximization.
    #'
    #' @return
    #' A named \code{list}, containing at least these five elements:
    #' \describe{
    #'   \item{\code{value}}{A \code{numeric}, the maximum function value.}
    #'   \item{\code{parameter}}{A \code{numeric} vector, the parameter vector
    #'   where the maximum is obtained.}
    #'   \item{\code{seconds}}{A \code{numeric}, the optimization time in seconds.}
    #'   \item{\code{initial}}{A \code{numeric}, the initial parameter values.}
    #'   \item{\code{error}}{Either \code{TRUE} if an error occurred, or \code{FALSE}, else.}
    #' }
    #' Appended are additional output elements of the optimizer.
    #'
    #' If an error occurred, then the error message is also appended as element
    #' \code{error_message}.
    #'
    #' If the time limit was exceeded, this also counts as an error. In addition,
    #' the flag \code{time_out = TRUE} is appended.
    #'
    #' @examples
    #' objective <- function(x) -x^4 + 3*x - 5
    #' optimizer <- Optimizer$new("stats::nlm")
    #' optimizer$optimize(objective = objective, initial = 2, direction = "min")
    #' optimizer$optimize(objective = objective, initial = 2, direction = "max")

    optimize = function(objective, initial, direction = "min", ...) {
      private$.optimize(
        objective = objective, initial = initial,
        additional_arguments = list(...), direction = direction
      )
    },

    #' @description
    #' Prints the optimizer label.
    #' @return
    #' Invisibly the \code{Optimizer} object.

    print = function(...) {
      cat("<optimizer '", self$label, "'>", sep = "")
      invisible(self)
    }

  ),

  private = list(

    .label = "unlabeled",
    .algorithm = NULL,
    .arg_objective = NULL,
    .arg_initial = NULL,
    .out_value = NULL,
    .out_parameter = NULL,
    .direction = NULL,
    .arguments = list(),
    .seconds = Inf,
    .hide_warnings = FALSE,
    .output_ignore = character(),

    ### helper function that prepares the optimization results
    .prepare_result = function(result, initial, invert_objective) {
      out <- list()
      if (private$.out_value %in% names(result$result)) {
        if (invert_objective) {
          out[["value"]] <- -result$result[[private$.out_value]]
        } else {
          out[["value"]] <- result$result[[private$.out_value]]
        }
      } else {
        out[["value"]] <- NA_real_
      }
      if (private$.out_parameter %in% names(result$result)) {
        out[["parameter"]] <- result$result[[private$.out_parameter]]
      } else {
        out[["parameter"]] <- rep(NA_real_, length(initial))
      }
      if ("time" %in% names(result)) {
        out[["seconds"]] <- as.numeric(result$time)
      } else {
        out[["seconds"]] <- NA_real_
      }
      out[["initial"]] <- initial
      if (!"error" %in% names(result)) {
        out[["error"]] <- FALSE
      } else {
        out[["error"]] <- TRUE
      }
      not_add <- c(private$.output_ignore, private$.out_value, private$.out_parameter)
      oeli::merge_lists(
        out,
        result$result[!names(result$result) %in% not_add]
      )
    },

    ### helper function that build an 'Objective' object from a function
    .build_objective = function(objective, initial) {
      if (!checkmate::test_r6(objective, "Objective")) {
        objective <- Objective$new(
          f = objective,
          target = names(formals(objective))[1],
          npar = length(initial)
        )
      }
      return(objective)
    },

    ### helper function that performs optimization
    .optimize = function(objective, initial, additional_arguments, direction) {
      checkmate::assert_choice(direction, c("min", "max"))
      checkmate::assert_list(additional_arguments)
      checkmate::assert_atomic_vector(initial, any.missing = FALSE)
      checkmate::assert_numeric(initial)
      objective <- private$.build_objective(objective, initial)
      checkmate::assert_numeric(initial, len = sum(objective$npar))
      invert_objective <- !identical(private$.direction, direction)
      args <- c(
        ### defines objective and initial argument for optimizer
        structure(
          list(objective$evaluate, initial),
          names = c(private$.arg_objective, private$.arg_initial)
        ),
        ### ensures that optimizer minimizes
        list(".negate" = invert_objective),
        ### arguments via ... have priority over previously specified arguments
        oeli::merge_lists(additional_arguments, private$.arguments)
      )
      checkmate::assert_list(args, names = "unique")
      result <- tryCatch(
        {
          suppressWarnings(
            oeli::timed(
              oeli::do.call_timed(
                what = private$.algorithm,
                args = args,
                units = "secs"
              ),
              seconds = private$.seconds,
              on_time_out = "error"
            ),
            classes = if (private$.hide_warnings) "warning" else ""
          )
        },
        error = function(e) {
          error_message <- e$message
          time_out <- grepl("time limit exceeded", error_message)
          if (time_out) {
            list(
              "result" = list("error_message" = error_message, "time_out" = TRUE),
              "error" = TRUE,
              "time" = NA_real_
            )
          } else {
            list(
              "result" = list("error_message" = error_message),
              "error" = TRUE,
              "time" = NA_real_
            )
          }
        }
      )
      private$.prepare_result(result, initial, invert_objective)
    }

  ),

  active = list(

    #' @field label \[`character(1)`\]\cr
    #' The label for the optimizer.

    label = function(value) {
      if (missing(value)) {
        private$.label
      } else {
        oeli::input_check_response(
          check = checkmate::check_string(value)
        )
        private$.label <- value
      }
    },

    #' @field algorithm \[`function`\]\cr
    #' The optimization algorithm.

    algorithm = function(value) {
      if (missing(value)) {
        private$.algorithm
      } else {
        oeli::input_check_response(
          check = checkmate::check_function(value)
        )
        private$.algorithm <- value
      }
    },

    #' @field arg_objective \[`character(1)`\]\cr
    #' The argument name for the objective function in \code{algorithm}.

    arg_objective = function(value) {
      if (missing(value)) {
        private$.arg_objective
      } else {
        oeli::input_check_response(
          check = checkmate::check_string(value)
        )
        private$.arg_objective <- value
      }
    },

    #' @field arg_initial \[`character(1)`\]\cr
    #' The argument name for the initial values in \code{algorithm}.

    arg_initial = function(value) {
      if (missing(value)) {
        private$.arg_initial
      } else {
        oeli::input_check_response(
          check = checkmate::check_string(value)
        )
        private$.arg_initial <- value
      }
    },

    #' @field out_value \[`character(1)`\]\cr
    #' The element name for the optimal function value in the output \code{list}
    #' of \code{algorithm}.

    out_value = function(value) {
      if (missing(value)) {
        private$.out_value
      } else {
        oeli::input_check_response(
          check = checkmate::check_string(value)
        )
        private$.out_value <- value
      }
    },

    #' @field out_parameter \[`character(1)`\]\cr
    #' The element name for the optimal parameters in the output \code{list} of
    #' \code{algorithm}.

    out_parameter = function(value) {
      if (missing(value)) {
        private$.out_parameter
      } else {
        oeli::input_check_response(
          check = checkmate::check_string(value)
        )
        private$.out_parameter <- value
      }
    },

    #' @field direction \[`character(1)`\]\cr
    #' Either \code{"min"} (if the optimizer minimizes) or \code{"max"}
    #' (if the optimizer maximizes).

    direction = function(value) {
      if (missing(value)) {
        private$.direction
      } else {
        oeli::input_check_response(
          check = checkmate::check_choice(value, c("min", "max"))
        )
        private$.direction <- value
      }
    },

    #' @field arguments \[`list()`\]\cr
    #' Custom arguments for \code{algorithm}.
    #'
    #' Defaults are used for arguments that are not specified.

    arguments = function(value) {
      if (missing(value)) {
        private$.arguments
      } else {
        oeli::input_check_response(
          check = checkmate::check_list(value, names = "unique")
        )
        private$.arguments <- value
      }
    },

    #' @field seconds \[`numeric(1)`\]\cr
    #' A time limit in seconds.
    #'
    #' Optimization is interrupted prematurely if \code{seconds} is exceeded.
    #'
    #' No time limit if \code{seconds = Inf} (the default).
    #'
    #' Note the limitations documented in \code{\link[base]{setTimeLimit}}.

    seconds = function(value) {
      if (missing(value)) {
        private$.seconds
      } else {
        oeli::input_check_response(
          check = checkmate::check_number(value, lower = 0, finite = FALSE)
        )
        private$.seconds <- value
      }
    },

    #' @field hide_warnings \[`logical(1)`\]\cr
    #' Hide warnings during optimization?

    hide_warnings = function(value) {
      if (missing(value)) {
        private$.hide_warnings
      } else {
        oeli::input_check_response(
          check = checkmate::check_flag(value)
        )
        private$.hide_warnings <- value
      }
    },

    #' @field output_ignore \[`character()`\]\cr
    #' Elements to ignore (not include) in the optimization output.

    output_ignore = function(value) {
      if (missing(value)) {
        private$.output_ignore
      } else {
        oeli::input_check_response(
          check = checkmate::check_names(value)
        )
        private$.output_ignore <- value
      }
    }
  )

)
