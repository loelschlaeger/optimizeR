#' Specify numerical optimizer as R6 object
#'
#' @description
#' A \code{Optimizer} R6 object defines a numerical optimizer based on an
#' optimization function implemented in R.
#' The main advantage of working with an \code{Optimizer} object instead of
#' using the optimization function directly lies in the standardized inputs and
#' outputs.
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
#' @param objective
#' A \code{function} to be optimized, returning a single \code{numeric}.
#' Its first argument must be a \code{numeric} vector of the same length as
#' \code{initial}, followed by any other arguments specified by the \code{...}
#' argument.
#' @param initial
#' A \code{numeric} vector with starting parameter values for the optimization.
#' @param ...
#' Optionally additional arguments to be passed to the optimizer algorithm.
#' Without specifications, default values are used.
#' @param direction
#' Either \code{"min"} for minimization or \code{"max"} for maximization.
#' @param seconds
#' A \code{numeric}, the number of seconds after which the optimization
#' is interrupted. Can also be \code{NULL} for no time limit.
#'
#' @examples
#' ### Apply 'stats::nlm' and 'pracma::nelder_mead' and compare their results:
#'
#' # 1. define objective function and initial values
#' objective <- TestFunctions::TF_ackley
#' initial <- 1:2
#'
#' # 2. get overview of optimizers in dictionary
#' optimizer_dictionary$keys
#'
#' # 3. define 'nlm' optimizer
#' optimizer_nlm <- Optimizer$new(which = "stats::nlm")
#'
#' # get a summary of the optimizer
#' summary(optimizer_nlm)
#'
#' # 4. define a custom optimizer (not contained in the dictionary)
#' optimizer_nelder_mead <- Optimizer$new(which = "custom")
#' optimizer_nelder_mead$definition(
#'   algorithm = pracma::nelder_mead,  # the optimization function
#'   arg_objective = "fn",             # the argument name for the objective function
#'   arg_initial = "x0",               # the argument name for the initial values
#'   out_value = "fmin",               # the element for the optimal function value in the output
#'   out_parameter = "xmin",           # the element for the optimal parameters in the output
#'   direction = "min"                 # the direction of the optimizer (here minimization)
#' )
#'
#' # 5. compare the optimization results
#' optimizer_nlm$apply(objective, initial)
#' optimizer_nelder_mead$apply(objective, initial)
#'
#' @export

Optimizer <- R6::R6Class(

  classname = "Optimizer",

  public = list(

    #' @description
    #' Initializes a new \code{Optimizer} object.
    #' @param which
    #' A \code{character}, either one of \code{optimizer_dictionary$keys} or
    #' \code{"custom"} (in which case \code{$definition()} must be used to
    #' define the optimizer details).
    #' @return
    #' A new \code{Optimizer} object.

    initialize = function(which, ...) {
      checkmate::assert_choice(
        which, choices = c(optimizer_dictionary$keys, "custom")
      )
      if (which %in% optimizer_dictionary$keys) {
        for (value in c(
            "label", "algorithm", "arg_objective", "arg_initial", "out_value",
            "out_parameter", "direction", "arguments", "output_elements"
          )
        ) {
          self[value] <- optimizer_dictionary$get(key = which, value = value)
        }
      }
      self$set_arguments(...)
    },

    #' @description
    #' Defines an optimizer.
    #' @param algorithm
    #' A \code{function}, the optimization algorithm.
    #' @param arg_objective
    #' A \code{character}, the argument name for the objective function in
    #' \code{algorithm}.
    #' @param arg_initial
    #' A \code{character}, the argument name for the initial values in
    #' \code{algorithm}.
    #' @param out_value
    #' A \code{character}, the element name for the optimal function value in
    #' the output \code{list} of \code{algorithm}.
    #' @param out_parameter
    #' A \code{character}, the element name for the optimal parameters in the
    #' output \code{list} of \code{algorithm}.
    #' @param direction
    #' Either \code{"min"} (if the optimizer minimizes) or \code{"max"}
    #' (if the optimizer maximizes).
    #' @param arguments
    #' A named \code{list} of arguments for \code{algorithm}. By default, the
    #' default arguments are used.
    #' @param output_elements
    #' A \code{character} of element names to be included in the output.
    #' Can also be \code{NULL}, in which case all elements are included.
    #' @return
    #' The \code{Optimizer} object.

    definition = function(
      algorithm, arg_objective, arg_initial, out_value, out_parameter,
      direction, arguments = formals(algorithm), output_elements = NULL
    ) {
      if (missing(.optimizer)) {
        cli::cli_abort(c(
          "x" = "Please specify argument {.var algorithm}.",
          "*" = "It should be a {.cls function} that performs numerical
                 optimization."
        ), call = NULL)
      }
      self$algorithm <- algorithm
      self$label <- paste0(
        deparse(
          eval.parent(
            substitute(
              substitute(algorithm)
            )
          ), width.cutoff = 500L
        ),
      collapse = "\n")
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
      self$arguments <- arguments
      self$output_elements <- output_elements
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
    #' Validates the \code{Optimizer} object.
    #' @return
    #' The \code{Optimizer} object.

    validate = function(
      objective = optimizeR::test_objective, initial = round(stats::rnorm(2)),
      ..., direction = "min", seconds = 3
    ) {
      self$apply(
        objective = objective, initial = initial, ..., direction = direction,
        seconds = seconds
      )
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
      opt_out <- try_silent(
        expr = timed(
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
          secs = .validation_settings[["check_seconds"]]
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
    },

    #' @description
    #' Applies an optimizer.
    #' @return
    #' A named \code{list}, containing at least these four elements:
    #' \describe{
    #'   \item{\code{value}}{A \code{numeric}, the value of the estimated
    #'   optimum of \code{objective}.}
    #'   \item{\code{parameter}}{A \code{numeric} vector, the parameter vector
    #'   where the optimum of \code{objective} is obtained.}
    #'   \item{\code{seconds}}{A \code{numeric}, the total optimization time in
    #'   seconds.}
    #'   \item{\code{initial}}{A \code{numeric}, the initial parameter values.}
    #' }
    #' Appended are additional output elements of the optimizer (if not excluded
    #' by the \code{$output_elements} field).
    #' @examples
    #' Optimizer$new("stats::nlm")$apply(function(x) x^4 + 3*x - 5, 2)

    apply = function(
      objective, initial, ..., direction = "min", seconds = self$seconds
    ) {
      start <- Sys.time()
      res <- do.call(
        what = optimizer[["optimizer"]],
        args = c(
          structure(
            list(objective),
            names = optimizer[["optimizer_labels"]][["objective"]]
          ),
          structure(
            list(initial), names = optimizer[["optimizer_labels"]][["initial"]]
          ),
          optimizer[["optimizer_arguments"]], list(...)
        ),
        quote = TRUE
      )
      end <- Sys.time()
      c(
        structure(
          list(res[[optimizer[["optimizer_labels"]][["value"]]]],
               res[[optimizer[["optimizer_labels"]][["parameter"]]]],
               as.numeric(difftime(end, start, units = "secs")),
               initial),
          names = c("value", "parameter", "seconds", "initial")
        ),
        res[!names(res) %in% optimizer[["output_ignore"]]]
      )
    },

    #' @description
    #' Prints the optimizer label.
    #' @return
    #' Invisibly the \code{Optimizer} object.

    print = function(...) {
      cat("<optimizer '", self$label, "'>", sep = "")
      invisible(self)
    },

    #' @description
    #' Returns a summary of the optimizer
    #' @return
    #' Invisibly the \code{Optimizer} object.

    summary = function(...) {
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
    .arguments = NULL,
    .output_elements = NULL,
    .seconds = NULL,

    .check_arguments_complete = function() {

    }

  ),

  active = list(

    #' @field label
    #' A \code{character}, the label for the optimizer.
    label = function(value) {
      if (missing(value)) {
        private$.label
      } else {
        checkmate::assert_string(value)
        private$.label <- value
      }
    },

    #' @field algorithm
    #' A \code{function}, the optimization algorithm.
    algorithm = function(value) {
      if (missing(value)) {
        private$.algorithm
      } else {
        checkmate::assert_function(value)
        private$.algorithm <- value
      }
    },

    #' @field arg_objective
    #' A \code{character}, the argument name for the objective function in
    #' \code{algorithm}.
    arg_objective = function(value) {
      if (missing(value)) {
        private$.arg_objective
      } else {
        checkmate::assert_string(value)
        private$.arg_objective <- value
      }
    },

    #' @field arg_initial
    #' A \code{character}, the argument name for the initial values in
    #' \code{algorithm}.
    arg_initial = function(value) {
      if (missing(value)) {
        private$.arg_initial
      } else {
        checkmate::assert_string(value)
        private$.arg_initial <- value
      }
    },

    #' @field out_value
    #' A \code{character}, the element name for the optimal function value in
    #' the output \code{list} of \code{algorithm}.
    out_value = function(value) {
      if (missing(value)) {
        private$.out_value
      } else {
        checkmate::assert_string(value)
        private$.out_value <- value
      }
    },

    #' @field out_parameter
    #' A \code{character}, the element name for the optimal parameters in the
    #' output \code{list} of \code{algorithm}.
    out_parameter = function(value) {
      if (missing(value)) {
        private$.out_parameter
      } else {
        checkmate::assert_string(value)
        private$.out_parameter <- value
      }
    },

    #' @field direction
    #' Either \code{"min"} (if the optimizer minimizes) or \code{"max"}
    #' (if the optimizer maximizes).
    direction = function(value) {
      if (missing(value)) {
        private$.direction
      } else {
        checkmate::assert_choice(value, c("min", "max"))
        private$.direction <- value
      }
    },

    #' @field arguments
    #' A named \code{list} of arguments for \code{algorithm}. By default, the
    #' default arguments are used.
    arguments = function(value) {
      if (missing(value)) {
        private$.arguments
      } else {
        checkmate::assert_list(value, names = "strict")
        private$.arguments <- value
      }
    },

    #' @field output_elements
    #' A \code{character} of element names to be included in the output.
    #' Can also be \code{NULL}, in which case all elements are included.
    output_elements = function(value) {
      if (missing(value)) {
        private$.output_elements
      } else {
        checkmate::assert_character(value, null.ok = TRUE)
        private$.output_elements <- value
      }
    },

    #' @field seconds
    #' A \code{numeric}, the number of seconds after which the optimization
    #' is interrupted. Can also be \code{NULL} for no time limit.
    seconds = function(value) {
      if (missing(value)) {
        private$.seconds
      } else {
        checkmate::assert_number(value, lower = 0)
        private$.seconds <- value
      }
    }

  )

)
