#' Specify numerical optimizer object
#'
#' @description
#' The \code{Optimizer} object defines a numerical optimizer based on any
#' optimization algorithm implemented in R. The main advantage of working with
#' an \code{Optimizer} object instead of using the optimization function
#' directly lies in the standardized inputs and outputs.
#'
#' Any R function that fulfills the following two constraints can be defined
#' as an \code{Optimizer} object:
#'
#' 1. It must have an input for a \code{function}, the objective function to be
#'    optimized.
#' 2. Its output must include the optimal function value and the optimal
#'    parameter vector.
#'
#' @param objective \[`function` | `Objective`\]\cr
#' A \code{function} to be optimized that
#'
#' 1. receives a \code{numeric} \code{vector} as its first argument (the
#'    so-called target argument)
#' 2. and returns a single \code{numeric} value.
#'
#' Alternatively, it can also be an \code{\link{Objective}} object for more
#' flexibility (e.g., for another or more than one target argument, or for a
#' gradient and Hessian function).
#'
#' @param initial \[`numeric()`\]\cr
#' Starting parameter values for the optimization (without missing values).
#'
#' If the optimizer does not take initial values, they only define the number
#' of parameters.
#'
#' @param lower \[`NA` | `numeric()` | `numeric(1)`\]\cr
#' Lower bounds on the parameters.
#'
#' If a single number, this will be applied to all parameters.
#'
#' Can be `NA` to not define any bounds.
#'
#' Ignored (with a warning) for optimizers that do not support lower bounds.
#'
#' @param upper \[`NA` | `numeric()` | `numeric(1)`\]\cr
#' Upper bounds on the parameters.
#'
#' If a single number, this will be applied to all parameters.
#'
#' Can be `NA` to not define any bounds.
#'
#' Ignored (with a warning) for optimizers that do not support upper bounds.
#'
#' @param ... \[`any`\]\cr
#' In \code{Optimizer$new()} and \code{$set_arguments()}, named arguments to
#' pass to the optimizer algorithm. In \code{$minimize()},
#' \code{$maximize()}, and \code{$optimize()}, named arguments to pass to the
#' objective function.
#'
#' @param direction \[`character(1)`\]\cr
#' Either `"min"` for minimization or `"max"` for maximization.
#'
#' @param .verbose \[`logical(1)`\]\cr
#' Print status messages?
#'
#' @examplesIf requireNamespace("pracma", quietly = TRUE)
#' ### Task: compare minimization with 'stats::nlm' and 'pracma::nelder_mead'
#'
#' # 1. define objective function (the Ackley function) and initial values
#' objective <- function(x) {
#'   -20 * exp(-0.2 * sqrt(mean(x^2))) - exp(mean(cos(2 * pi * x))) +
#'     20 + exp(1)
#' }
#' initial <- c(3, 3)
#'
#' # 2. get overview of optimizers available in dictionary
#' optimizer_dictionary$keys
#'
#' # 3. define 'nlm' optimizer
#' nlm <- Optimizer$new(which = "stats::nlm")
#'
#' # 4. define the 'pracma::nelder_mead' optimizer
#' #    (not contained in the dictionary)
#' nelder_mead <- Optimizer$new(which = "custom")
#' nelder_mead$definition(
#'   algorithm = pracma::nelder_mead, # optimization function
#'   arg_objective = "fn",            # objective function argument name
#'   arg_initial = "x0",              # argument name for the initial values
#'   out_value = "fmin",              # element for the optimal function value
#'   out_parameter = "xmin",          # element for the optimal parameters
#'   direction = "min"                # optimizer minimizes
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
    #' @param which \[`character(1)`\]\cr
    #' Either:
    #'
    #' - one of \code{optimizer_dictionary$keys}
    #' - or \code{"custom"}, in which case \code{$definition()} must be used to
    #' define the optimizer details.
    #'
    #' @return
    #' A new \code{Optimizer} object.

    initialize = function(which, ..., .verbose = TRUE) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(which),
        var_name = "which"
      )
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
        oeli::input_check_response(
          check = checkmate::check_choice(
            which, choices = optimizer_dictionary$keys
          ),
          var_name = "which"
        )

        ### get algorithm, from its package if it is specified by name
        algorithm <- optimizer_dictionary$get(key = which, value = "algorithm")
        if (is.character(algorithm)) {
          algorithm <- strsplit(algorithm, split = "::", fixed = TRUE)[[1]]
          if (length(algorithm) != 2) {
            cli::cli_abort(
              paste(
                "The algorithm of {.val {which}} must be a function or a name",
                "of the form {.val package::function}."
              ),
              call = NULL
            )
          }
          algorithm_package <- algorithm[1]
          algorithm_name <- algorithm[2]
          package <- algorithm_package
          if (package == "optimizeR") {
            package <- adapter_packages[[algorithm_name]]
          }
          if (!requireNamespace(package, quietly = TRUE)) {
            cli::cli_abort(
              c(
                paste(
                  "The optimizer {.val {which}} needs the package",
                  "{.pkg {package}}."
                ),
                "i" = "Please install it first."
              ),
              call = NULL
            )
          }
          algorithm <- get(
            algorithm_name, envir = asNamespace(algorithm_package)
          )
        }

        ### define optimizer
        self$definition(
          algorithm = algorithm,
          arg_objective = optimizer_dictionary$get(
            key = which, value = "arg_objective"
          ),
          arg_initial = optimizer_dictionary$get(
            key = which, value = "arg_initial"
          ),
          arg_lower = optimizer_dictionary$get(
            key = which, value = "arg_lower"
          ),
          arg_upper = optimizer_dictionary$get(
            key = which, value = "arg_upper"
          ),
          arg_gradient = optimizer_dictionary$get(
            key = which, value = "arg_gradient"
          ),
          arg_hessian = optimizer_dictionary$get(
            key = which, value = "arg_hessian"
          ),
          gradient_as_attribute = optimizer_dictionary$get(
            key = which, value = "gradient_as_attribute"
          ),
          hessian_as_attribute = optimizer_dictionary$get(
            key = which, value = "hessian_as_attribute"
          ),
          out_value = optimizer_dictionary$get(
            key = which, value = "out_value"
          ),
          out_parameter = optimizer_dictionary$get(
            key = which, value = "out_parameter"
          ),
          direction = optimizer_dictionary$get(
            key = which, value = "direction"
          )
        )
        self$label <- which
      }
      self$set_arguments(...)
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
    #' @param arg_initial \[`character(1)` | `NA`\]\cr
    #' The argument name for the initial values in \code{algorithm}.
    #'
    #' Can be `NA` if \code{algorithm} does not take initial values.
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
    #' @param hessian_as_attribute \[`logical(1)`\]\cr
    #' Only relevant if `arg_hessian` is not `NA`.
    #'
    #' In that case, does `algorithm` expect that the Hessian is an attribute
    #' of the objective function output (as for example in
    #' \code{\link[stats]{nlm}})? In that case, `arg_hessian` defines the
    #' attribute name.
    #'
    #' @param out_value \[`character()` | `function`\]\cr
    #' The element name for the optimal function value in the output of
    #' \code{algorithm}.
    #'
    #' Can be a vector of names if the output is nested (e.g.,
    #' `c("optim", "bestval")` for the element `bestval` of the element
    #' `optim`). If the output is an S4 object, the names refer to its slots.
    #'
    #' Alternatively, it can also be a \code{function} that receives the output
    #' of \code{algorithm} and returns the optimal function value.
    #'
    #' @param out_parameter \[`character()` | `function`\]\cr
    #' The element name for the optimal parameters in the output of
    #' \code{algorithm}.
    #'
    #' Can be a vector of names if the output is nested (e.g.,
    #' `c("optim", "bestmem")` for the element `bestmem` of the element
    #' `optim`). If the output is an S4 object, the names refer to its slots.
    #'
    #' Alternatively, it can also be a \code{function} that receives the output
    #' of \code{algorithm} and returns the optimal parameters.
    #'
    #' @param direction \[`character(1)`\]\cr
    #' Either \code{"min"} (if the optimizer minimizes) or \code{"max"}
    #' (if the optimizer maximizes).
    #'
    #' @return
    #' Invisibly the \code{Optimizer} object.
    #'
    #' @examples
    #' ### define 'stats::optimize', which does not take initial values
    #' optimize <- Optimizer$new(which = "custom", .verbose = FALSE)
    #' optimize$definition(
    #'   algorithm = stats::optimize,
    #'   arg_objective = "f",
    #'   arg_initial = NA,
    #'   arg_lower = "lower",
    #'   arg_upper = "upper",
    #'   out_value = "objective",
    #'   out_parameter = "minimum",
    #'   direction = "min"
    #' )
    #' optimize$minimize(
    #'   objective = function(x) x^4 + 3*x - 5, initial = 2, lower = -5,
    #'   upper = 5
    #' )

    definition = function(
      algorithm,
      arg_objective,
      arg_initial,
      arg_lower = NA,
      arg_upper = NA,
      arg_gradient = NA,
      arg_hessian = NA,
      gradient_as_attribute = FALSE,
      hessian_as_attribute = FALSE,
      out_value,
      out_parameter,
      direction
    ) {

      oeli::input_check_response(
        check = oeli::check_missing(algorithm),
        var_name = "algorithm"
      )
      self$algorithm <- algorithm
      self$label <- oeli::variable_name(algorithm)

      oeli::input_check_response(
        check = oeli::check_missing(arg_objective),
        var_name = "arg_objective"
      )
      self$arg_objective <- arg_objective

      oeli::input_check_response(
        check = oeli::check_missing(arg_initial),
        var_name = "arg_initial"
      )
      self$arg_initial <- arg_initial

      self$arg_lower <- arg_lower
      self$arg_upper <- arg_upper
      self$arg_gradient <- arg_gradient
      self$arg_hessian <- arg_hessian
      self$gradient_as_attribute <- gradient_as_attribute
      self$hessian_as_attribute <- hessian_as_attribute
      oeli::input_check_response(
        check = if (gradient_as_attribute && is.na(arg_gradient)) {
          "Must be the attribute name if `gradient_as_attribute` is `TRUE`"
        } else {
          TRUE
        },
        var_name = "arg_gradient"
      )
      oeli::input_check_response(
        check = if (hessian_as_attribute && is.na(arg_hessian)) {
          "Must be the attribute name if `hessian_as_attribute` is `TRUE`"
        } else {
          TRUE
        },
        var_name = "arg_hessian"
      )

      oeli::input_check_response(
        check = oeli::check_missing(out_value),
        var_name = "out_value"
      )
      self$out_value <- out_value

      oeli::input_check_response(
        check = oeli::check_missing(out_parameter),
        var_name = "out_parameter"
      )
      self$out_parameter <- out_parameter

      oeli::input_check_response(
        check = oeli::check_missing(direction),
        var_name = "direction"
      )
      self$direction <- direction

      oeli::input_check_response(
        check = checkmate::check_function(
          algorithm,
          args = c(
            arg_objective, if (!is.na(arg_initial)) arg_initial,
            if (!is.na(arg_lower)) arg_lower, if (!is.na(arg_upper)) arg_upper,
            if (
              !is.na(arg_gradient) && isFALSE(gradient_as_attribute)
            ) arg_gradient,
            if (
              !is.na(arg_hessian) && isFALSE(hessian_as_attribute)
            ) arg_hessian
          )
        ),
        var_name = "algorithm"
      )

      invisible(self)
    },

    #' @description
    #' Sets optimizer arguments.
    #'
    #' The arguments for the objective function and the initial values cannot
    #' be set here, since they are specified when calling \code{$minimize()},
    #' \code{$maximize()}, or \code{$optimize()}.
    #'
    #' @return
    #' Invisibly the \code{Optimizer} object.

    set_arguments = function(...) {
      arguments <- list(...)
      oeli::input_check_response(
        check = checkmate::check_list(arguments, names = "unique"),
        var_name = "..."
      )
      private$.arguments[names(arguments)] <- arguments
      invisible(self)
    },

    #' @description
    #' Perform minimization.
    #'
    #' @return
    #' A named \code{list}, containing at least these five elements:
    #' \describe{
    #'   \item{\code{value}}{A \code{numeric}, the minimum function value.}
    #'   \item{\code{parameter}}{A \code{numeric} vector, the parameter vector
    #'   where the minimum is obtained.}
    #'   \item{\code{seconds}}{A \code{numeric}, the optimization time in
    #'   seconds.}
    #'   \item{\code{initial}}{A \code{numeric}, the initial parameter values.}
    #'   \item{\code{error}}{A \code{logical} flag: \code{TRUE} if an error
    #'   occurred, otherwise \code{FALSE}.}
    #' }
    #' Additional output elements from the optimizer are appended. If the
    #' optimizer output is not a named \code{list}, it is appended as element
    #' \code{output}.
    #'
    #' If an error occurred, the error message is also appended as element
    #' \code{error_message}.
    #'
    #' If the time limit was exceeded, this counts as an error. In addition,
    #' the flag \code{time_out = TRUE} is appended.
    #'
    #' @examples
    #' Optimizer$new("stats::nlm")$
    #'   minimize(objective = function(x) x^4 + 3*x - 5, initial = 2)

    minimize = function(
      objective,
      initial,
      lower = NA,
      upper = NA,
      ...
    ) {
      oeli::input_check_response(
        check = oeli::check_missing(objective),
        var_name = "objective"
      )
      oeli::input_check_response(
        check = oeli::check_missing(initial),
        var_name = "initial"
      )
      private$.optimize(
        objective = objective,
        initial = initial,
        lower = lower,
        upper = upper,
        additional_arguments = list(...),
        direction = "min"
      )
    },

    #' @description
    #' Perform maximization.
    #'
    #' @return
    #' A named \code{list}, containing at least these five elements:
    #' \describe{
    #'   \item{\code{value}}{A \code{numeric}, the maximum function value.}
    #'   \item{\code{parameter}}{A \code{numeric} vector, the parameter vector
    #'   where the maximum is obtained.}
    #'   \item{\code{seconds}}{A \code{numeric}, the optimization time in
    #'   seconds.}
    #'   \item{\code{initial}}{A \code{numeric}, the initial parameter values.}
    #'   \item{\code{error}}{A \code{logical} flag: \code{TRUE} if an error
    #'   occurred, otherwise \code{FALSE}.}
    #' }
    #' Additional output elements from the optimizer are appended. If the
    #' optimizer output is not a named \code{list}, it is appended as element
    #' \code{output}.
    #'
    #' If an error occurred, the error message is also appended as element
    #' \code{error_message}.
    #'
    #' If the time limit was exceeded, this also counts as an error. In
    #' addition, the flag \code{time_out = TRUE} is appended.
    #'
    #' @examples
    #' Optimizer$new("stats::nlm")$
    #'   maximize(objective = function(x) -x^4 + 3*x - 5, initial = 2)

    maximize = function(
      objective,
      initial,
      lower = NA,
      upper = NA,
      ...
    ) {
      oeli::input_check_response(
        check = oeli::check_missing(objective),
        var_name = "objective"
      )
      oeli::input_check_response(
        check = oeli::check_missing(initial),
        var_name = "initial"
      )
      private$.optimize(
        objective = objective,
        initial = initial,
        lower = lower,
        upper = upper,
        additional_arguments = list(...),
        direction = "max"
      )
    },

    #' @description
    #' Perform minimization or maximization.
    #'
    #' @return
    #' A named \code{list}, containing at least these five elements:
    #' \describe{
    #'   \item{\code{value}}{A \code{numeric}, the optimized function value.}
    #'   \item{\code{parameter}}{A \code{numeric} vector, the parameter vector
    #'   where the optimum is obtained.}
    #'   \item{\code{seconds}}{A \code{numeric}, the optimization time in
    #'   seconds.}
    #'   \item{\code{initial}}{A \code{numeric}, the initial parameter values.}
    #'   \item{\code{error}}{A \code{logical} flag: \code{TRUE} if an error
    #'   occurred, otherwise \code{FALSE}.}
    #' }
    #' Additional output elements from the optimizer are appended. If the
    #' optimizer output is not a named \code{list}, it is appended as element
    #' \code{output}.
    #'
    #' If an error occurred, the error message is also appended as element
    #' \code{error_message}.
    #'
    #' If the time limit was exceeded, this also counts as an error. In
    #' addition, the flag \code{time_out = TRUE} is appended.
    #'
    #' @examples
    #' objective <- function(x) sin(x)
    #' nlm <- Optimizer$new("stats::nlm")
    #' nlm$optimize(objective = objective, initial = 2, direction = "min")
    #' nlm$optimize(objective = objective, initial = 2, direction = "max")

    optimize = function(
      objective,
      initial,
      lower = NA,
      upper = NA,
      direction = "min",
      ...
    ) {
      oeli::input_check_response(
        check = oeli::check_missing(objective),
        var_name = "objective"
      )
      oeli::input_check_response(
        check = oeli::check_missing(initial),
        var_name = "initial"
      )
      private$.optimize(
        objective = objective,
        initial = initial,
        lower = lower,
        upper = upper,
        additional_arguments = list(...),
        direction = direction
      )
    },

    #' @description
    #' Prints the optimizer label.
    #' @return
    #' Invisibly the \code{Optimizer} object.

    print = function(...) {
      cat("<optimizer '", self$label, "'>\n", sep = "")
      invisible(self)
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
          check = checkmate::check_string(value),
          var_name = "label"
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
          check = checkmate::check_function(value),
          var_name = "algorithm"
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
          check = checkmate::check_string(value),
          var_name = "arg_objective"
        )
        private$.arg_objective <- value
      }
    },

    #' @field arg_initial \[`character(1)` | `NA`\]\cr
    #' The argument name for the initial values in \code{algorithm}.
    #'
    #' Can be `NA` if \code{algorithm} does not take initial values.

    arg_initial = function(value) {
      if (missing(value)) {
        private$.arg_initial
      } else {
        oeli::input_check_response(
          check = checkmate::check_string(value, na.ok = TRUE),
          var_name = "arg_initial"
        )
        private$.arg_initial <- value
      }
    },

    #' @field arg_lower \[`character(1)` | `NA`\]\cr
    #' Optionally the argument name for the lower parameter bound in
    #' \code{algorithm}.
    #'
    #' Can be `NA` if not available.

    arg_lower = function(value) {
      if (missing(value)) {
        private$.arg_lower
      } else {
        oeli::input_check_response(
          check = checkmate::check_string(value, na.ok = TRUE),
          var_name = "arg_lower"
        )
        private$.arg_lower <- value
      }
    },

    #' @field arg_upper \[`character(1)` | `NA`\]\cr
    #' Optionally the argument name for the upper parameter bound in
    #' \code{algorithm}.
    #'
    #' Can be `NA` if not available.

    arg_upper = function(value) {
      if (missing(value)) {
        private$.arg_upper
      } else {
        oeli::input_check_response(
          check = checkmate::check_string(value, na.ok = TRUE),
          var_name = "arg_upper"
        )
        private$.arg_upper <- value
      }
    },

    #' @field arg_gradient \[`character(1)` | `NA`\]\cr
    #' Optionally the argument name for the gradient function in
    #' \code{algorithm}.
    #'
    #' Can be `NA` if not available.

    arg_gradient = function(value) {
      if (missing(value)) {
        private$.arg_gradient
      } else {
        oeli::input_check_response(
          check = checkmate::check_string(value, na.ok = TRUE),
          var_name = "arg_gradient"
        )
        private$.arg_gradient <- value
      }
    },

    #' @field arg_hessian \[`character(1)` | `NA`\]\cr
    #' Optionally the argument name for the Hessian function in
    #' \code{algorithm}.
    #'
    #' Can be `NA` if not available.

    arg_hessian = function(value) {
      if (missing(value)) {
        private$.arg_hessian
      } else {
        oeli::input_check_response(
          check = checkmate::check_string(value, na.ok = TRUE),
          var_name = "arg_hessian"
        )
        private$.arg_hessian <- value
      }
    },

    #' @field gradient_as_attribute \[`logical(1)`\]\cr
    #' Only relevant if `arg_gradient` is not `NA`.
    #'
    #' In that case, does `algorithm` expect that the gradient is an attribute
    #' of the objective function output (as for example in
    #' \code{\link[stats]{nlm}})? In that case, `arg_gradient` defines the
    #' attribute name.

    gradient_as_attribute = function(value) {
      if (missing(value)) {
        private$.gradient_as_attribute
      } else {
        oeli::input_check_response(
          check = checkmate::check_flag(value),
          var_name = "gradient_as_attribute"
        )
        private$.gradient_as_attribute <- value
      }
    },

    #' @field hessian_as_attribute \[`logical(1)`\]\cr
    #' Only relevant if `arg_hessian` is not `NA`.
    #'
    #' In that case, does `algorithm` expect that the Hessian is an attribute
    #' of the objective function output (as for example in
    #' \code{\link[stats]{nlm}})? In that case, `arg_hessian` defines the
    #' attribute name.

    hessian_as_attribute = function(value) {
      if (missing(value)) {
        private$.hessian_as_attribute
      } else {
        oeli::input_check_response(
          check = checkmate::check_flag(value),
          var_name = "hessian_as_attribute"
        )
        private$.hessian_as_attribute <- value
      }
    },

    #' @field out_value \[`character()` | `function`\]\cr
    #' The element name for the optimal function value in the output of
    #' \code{algorithm}.
    #'
    #' Can be a vector of names if the output is nested.
    #'
    #' Alternatively, it can also be a \code{function} that receives the output
    #' of \code{algorithm} and returns the optimal function value.

    out_value = function(value) {
      if (missing(value)) {
        private$.out_value
      } else {
        oeli::input_check_response(
          check = list(
            checkmate::check_character(value, any.missing = FALSE, min.len = 1),
            checkmate::check_function(value)
          ),
          var_name = "out_value"
        )
        private$.out_value <- value
      }
    },

    #' @field out_parameter \[`character()` | `function`\]\cr
    #' The element name for the optimal parameters in the output of
    #' \code{algorithm}.
    #'
    #' Can be a vector of names if the output is nested.
    #'
    #' Alternatively, it can also be a \code{function} that receives the output
    #' of \code{algorithm} and returns the optimal parameters.

    out_parameter = function(value) {
      if (missing(value)) {
        private$.out_parameter
      } else {
        oeli::input_check_response(
          check = list(
            checkmate::check_character(value, any.missing = FALSE, min.len = 1),
            checkmate::check_function(value)
          ),
          var_name = "out_parameter"
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
          check = checkmate::check_choice(value, c("min", "max")),
          var_name = "direction"
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
          check = checkmate::check_list(value, names = "unique"),
          var_name = "arguments"
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
    #'
    #' The time limit refers to elapsed time and is also checked each time the
    #' optimizer evaluates the objective function.

    seconds = function(value) {
      if (missing(value)) {
        private$.seconds
      } else {
        oeli::input_check_response(
          check = checkmate::check_number(value, lower = 0, finite = FALSE),
          var_name = "seconds"
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
          check = checkmate::check_flag(value),
          var_name = "hide_warnings"
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
          check = checkmate::check_names(value),
          var_name = "output_ignore"
        )
        private$.output_ignore <- value
      }
    }
  ),

  private = list(

    .label = "unlabeled",
    .algorithm = NULL,
    .arg_objective = NULL,
    .arg_initial = NULL,
    .arg_lower = NA,
    .arg_upper = NA,
    .arg_gradient = NA,
    .arg_hessian = NA,
    .gradient_as_attribute = FALSE,
    .hessian_as_attribute = FALSE,
    .out_value = NULL,
    .out_parameter = NULL,
    .direction = NULL,
    .arguments = list(),
    .seconds = Inf,
    .hide_warnings = FALSE,
    .output_ignore = character(),

    ### helper function that extracts an element from the optimizer output
    .extract_output = function(output, element, var_name) {
      if (is.function(element)) {
        out <- tryCatch(
          element(output),
          error = function(e) {
            msg <- e$message
            cli::cli_abort(
              paste(
                "The optimizer output is bad: {.var {var_name}} threw an",
                "error: {msg}"
              ),
              call = NULL
            )
          }
        )
        if (is.null(out)) {
          cli::cli_abort(
            paste(
              "The optimizer output is bad: {.var {var_name}} returned",
              "{.code NULL}."
            ),
            call = NULL
          )
        }
        return(out)
      }
      for (name in element) {
        oeli::input_check_response(
          check = checkmate::check_names(
            if (isS4(output)) methods::slotNames(output) else names(output),
            must.include = name
          ),
          prefix = "The optimizer output is bad:"
        )
        output <- if (isS4(output)) {
          methods::slot(output, name)
        } else {
          output[[name]]
        }
      }
      return(output)
    },

    ### helper function that prepares the optimization results
    .prepare_result = function(result, initial, invert_objective) {
      output <- result$result
      if ("error" %in% names(result)) {
        value <- NA_real_
        parameter <- rep(NA_real_, length(initial))
      } else {
        value <- private$.extract_output(
          output, private$.out_value, "out_value"
        )
        parameter <- private$.extract_output(
          output, private$.out_parameter, "out_parameter"
        )
        if (!checkmate::test_list(output, names = "unique")) {
          output <- list("output" = output)
        }
      }
      out <- list(
        "value" = if (invert_objective) -value else value,
        "parameter" = parameter,
        "seconds" = as.numeric(result$time),
        "initial" = initial,
        "error" = "error" %in% names(result)
      )
      not_add <- c(
        private$.output_ignore,
        if (checkmate::test_string(private$.out_value)) private$.out_value,
        if (checkmate::test_string(private$.out_parameter)) {
          private$.out_parameter
        }
      )
      oeli::merge_lists(out, output[!names(output) %in% not_add])
    },

    ### helper function that build an 'Objective' object from a function
    .build_objective_object = function(objective, initial) {
      oeli::input_check_response(
        check = oeli::check_numeric_vector(
          initial, any.missing = FALSE, min.len = 1
        ),
        var_name = "initial"
      )
      if (!checkmate::test_r6(objective, "Objective")) {
        oeli::input_check_response(
          check = if (is.function(objective)) {
            TRUE
          } else {
            "Must be a function or an object of class 'Objective'"
          },
          var_name = "objective"
        )
        objective <- Objective$new(
          f = objective,
          target = names(formals(objective))[1],
          npar = length(initial)
        )
      } else {
        oeli::input_check_response(
          check = oeli::check_numeric_vector(
            initial, len = sum(objective$npar)
          ),
          var_name = "initial"
        )
      }
      return(objective)
    },

    ### helper function that performs optimization
    .optimize = function(
      objective, initial, lower, upper, additional_arguments, direction
    ) {

      ### input checks
      if (
        is.null(private$.algorithm) ||
        is.null(private$.arg_objective) ||
        is.null(private$.arg_initial) ||
        is.null(private$.out_value) ||
        is.null(private$.out_parameter) ||
        is.null(private$.direction)
      ) {
        cli::cli_abort(
          c(
            "The optimizer is not defined yet.",
            "i" = "Use method {.fun $definition} to define it."
          ),
          call = NULL
        )
      }
      oeli::input_check_response(
        check = checkmate::check_choice(direction, c("min", "max")),
        var_name = "direction"
      )
      oeli::input_check_response(
        check = checkmate::check_list(additional_arguments, names = "unique"),
        var_name = "..."
      )

      ### build objective function
      objective_object <- private$.build_objective_object(objective, initial)

      ### check optimizer arguments
      arguments_reserved <- intersect(
        names(private$.arguments),
        c(private$.arg_objective, private$.arg_initial)
      )
      if (length(arguments_reserved) > 0) {
        cli::cli_abort(
          paste(
            "The optimizer argument{?s} {.var {arguments_reserved}} cannot be",
            "set. The objective function and the initial values are specified",
            "when the optimization is called."
          ),
          call = NULL
        )
      }

      ### check parameter bounds
      parameter_bounds_arguments <- list()
      if (!checkmate::test_scalar_na(lower)) {
        if (!is.na(self$arg_lower)) {
          if (checkmate::test_number(lower)) {
            lower <- rep(lower, length(initial))
          }
          oeli::input_check_response(
            check = oeli::check_numeric_vector(
              lower, len = length(initial), any.missing = FALSE
            ),
            var_name = "lower"
          )
          parameter_bounds_arguments <- c(
            parameter_bounds_arguments,
            structure(
              list(lower), names = self$arg_lower
            )
          )
        } else {
          if (!self$hide_warnings) {
            cli::cli_warn(
              "The optimizer does not support lower parameter bounds."
            )
          }
          lower <- NA
        }
      }
      if (!checkmate::test_scalar_na(upper)) {
        if (!is.na(self$arg_upper)) {
          if (checkmate::test_number(upper)) {
            upper <- rep(upper, length(initial))
          }
          oeli::input_check_response(
            check = oeli::check_numeric_vector(
              upper, len = length(initial), any.missing = FALSE
            ),
            "upper"
          )
          parameter_bounds_arguments <- c(
            parameter_bounds_arguments,
            structure(
              list(upper), names = self$arg_upper
            )
          )
        } else {
          if (!self$hide_warnings) {
            cli::cli_warn(
              "The optimizer does not support upper parameter bounds."
            )
          }
          upper <- NA
        }
      }

      ### build objective, gradient, and Hessian wrappers
      invert_objective <- !identical(private$.direction, direction)
      additional_arguments <- keep_unevaluated(additional_arguments)
      evaluated <- FALSE
      evaluate_arguments <- function(.at) {
        c(
          list(
            ".at" = .at,
            ".negate" = invert_objective
          ),
          additional_arguments
        )
      }
      objective_function <- function(.at, ...) {
        check_time_limit()
        ### the optimizer may name the parameter argument
        if (missing(.at)) .at <- ..1
        evaluated <<- TRUE
        do.call(
          what = objective_object$evaluate,
          args = c(
            evaluate_arguments(.at),
            list(
              ".gradient_as_attribute" = self$gradient_as_attribute,
              ".gradient_attribute_name" = self$arg_gradient,
              ".hessian_as_attribute" = self$hessian_as_attribute,
              ".hessian_attribute_name" = self$arg_hessian
            )
          )
        )
      }
      gradient_function <- function(.at, ...) {
        check_time_limit()
        if (missing(.at)) .at <- ..1
        evaluated <<- TRUE
        do.call(
          what = objective_object$evaluate_gradient,
          args = evaluate_arguments(.at)
        )
      }
      hessian_function <- function(.at, ...) {
        check_time_limit()
        if (missing(.at)) .at <- ..1
        evaluated <<- TRUE
        as.matrix(
          do.call(
            what = objective_object$evaluate_hessian,
            args = evaluate_arguments(.at)
          )
        )
      }

      ### build gradient and hessian arguments
      gradient_hessian_arguments <- list()
      if (objective_object$gradient_specified && !self$gradient_as_attribute) {
        if (!is.na(self$arg_gradient)) {
          gradient_hessian_arguments <- c(
            gradient_hessian_arguments,
            structure(
              list(gradient_function),
              names = self$arg_gradient
            )
          )
        } else {
          if (!self$hide_warnings) {
            cli::cli_warn(
              "The optimizer does not support custom gradient function."
            )
          }
        }
      }
      if (objective_object$hessian_specified && !self$hessian_as_attribute) {
        if (!is.na(self$arg_hessian)) {
          gradient_hessian_arguments <- c(
            gradient_hessian_arguments,
            structure(
              list(hessian_function),
              names = self$arg_hessian
            )
          )
        } else {
          if (!self$hide_warnings) {
            cli::cli_warn(
              "The optimizer does not support custom Hessian function."
            )
          }
        }
      }

      ### build optimizer arguments
      args <- c(
        ### defines objective and initial argument for optimizer
        structure(
          list(objective_function),
          names = private$.arg_objective
        ),
        if (!is.na(private$.arg_initial)) {
          structure(
            list(initial),
            names = private$.arg_initial
          )
        },

        ### internal helper arguments have priority over optimizer arguments
        oeli::merge_lists(
          parameter_bounds_arguments,
          gradient_hessian_arguments,
          private$.arguments
        )
      )
      checkmate::assert_list(args, names = "unique") # to make sure

      ### first without '...' in the wrappers if optimizer does not pass
      ### arguments on, as some optimizers check the arguments of the objective
      arguments_passed_on <- setdiff(
        names(private$.arguments),
        oeli::function_arguments(private$.algorithm)
      )
      args_attempts <- list(args)
      if (length(arguments_passed_on) == 0) {
        args_without_dots <- args
        wrappers <- c(private$.arg_objective, names(gradient_hessian_arguments))
        for (wrapper in wrappers) {
          formals(args_without_dots[[wrapper]]) <- alist(.at = )
        }
        args_attempts <- list(args_without_dots, args)
      }

      ### optimize
      for (attempt in seq_along(args_attempts)) {
        result <- tryCatch(
          {
            suppressWarnings(
              with_time_limit(
                oeli::do.call_timed(
                  what = private$.algorithm,
                  args = args_attempts[[attempt]],
                  units = "secs"
                ),
                seconds = private$.seconds,
                on_time_out = list(
                  "result" = list(
                    "error_message" = "time limit exceeded",
                    "time_out" = TRUE
                  ),
                  "error" = TRUE,
                  "time" = NA_real_
                )
              ),
              classes = if (private$.hide_warnings) "warning" else ""
            )
          },
          error = function(e) {
            error_message <- trimws(unname(cli::ansi_strip(e$message)))
            list(
              "result" = list("error_message" = error_message),
              "error" = TRUE,
              "time" = NA_real_
            )
          }
        )
        if (attempt == 1) {
          first_result <- result
        }
        ### again with '...' only if the optimizer failed before it evaluated
        if (
          !isTRUE(result$error) || evaluated || isTRUE(result$result$time_out)
        ) {
          break
        }
      }
      if (isTRUE(result$error) && !evaluated) {
        result <- first_result
      }

      ### prepare result
      private$.prepare_result(result, initial, invert_objective)
    }

  )

)
