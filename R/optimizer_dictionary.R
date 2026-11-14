#' Dictionary of optimizer functions
#'
#' @description
#' A dictionary of currently included numerical optimizer functions in the
#' `{optimizeR}` package, which can be selected via their label in
#' \code{\link{Optimizer}$new()} and \code{\link{optimizer}()}.
#'
#' Any other optimizer function can be defined via the method
#' \code{$definition()} of an \code{\link{Optimizer}} object.
#'
#' @details
#' # Adding optimizers
#' Further optimizers can be added to the dictionary via its method `$add()`.
#' It requires the same elements as the entries of the dictionary (see the
#' example below), i.e., a `label`, the `algorithm` (either the optimization
#' function itself or its name in the form `"package::function"`, in which
#' case the package is only required when an \code{\link{Optimizer}} object
#' is created), and the arguments of the method `$definition()` of an
#' \code{\link{Optimizer}} object. Note that the dictionary is built when
#' `{optimizeR}` is loaded, such that added optimizers are available until the
#' end of the R session.
#'
#' @format
#' An \code{R6} object of class \code{\link[oeli]{Dictionary}}.
#'
#' @export
#'
#' @examples
#' print(optimizer_dictionary)
#'
#' ### the definition of an optimizer
#' str(optimizer_dictionary$get("stats::nlm"))
#'
#' ### add an optimizer to the dictionary
#' if (requireNamespace("pracma", quietly = TRUE)) {
#'   optimizer_dictionary$add(
#'     "label" = "pracma::nelder_mead",
#'     "algorithm" = "pracma::nelder_mead",
#'     "arg_objective" = "fn",
#'     "arg_initial" = "x0",
#'     "arg_lower" = NA,
#'     "arg_upper" = NA,
#'     "arg_gradient" = NA,
#'     "arg_hessian" = NA,
#'     "gradient_as_attribute" = FALSE,
#'     "hessian_as_attribute" = FALSE,
#'     "out_value" = "fmin",
#'     "out_parameter" = "xmin",
#'     "direction" = "min"
#'   )
#'   nelder_mead <- Optimizer$new("pracma::nelder_mead")
#'   nelder_mead$minimize(function(x) sum((x - 1)^2), initial = c(0, 0))
#' }

optimizer_dictionary <- NULL

#' @keywords internal
#' @noRd

build_optimizer_dictionary <- function() {

  ### build dictionary
  optimizer_dictionary <- oeli::Dictionary$new(
    key_name = "label",
    value_names = c(
      "algorithm",
      "arg_objective",
      "arg_initial",
      "arg_lower",
      "arg_upper",
      "arg_gradient",
      "arg_hessian",
      "gradient_as_attribute",
      "hessian_as_attribute",
      "out_value",
      "out_parameter",
      "direction"
    ),
    value_assert = alist(
      "algorithm" = checkmate::assert_multi_class(
        classes = c("character", "function")
      ),
      "arg_objective" = checkmate::assert_string(),
      "arg_initial" = checkmate::assert_string(na.ok = TRUE),
      "arg_lower" = checkmate::assert_string(na.ok = TRUE),
      "arg_upper" = checkmate::assert_string(na.ok = TRUE),
      "arg_gradient" = checkmate::assert_string(na.ok = TRUE),
      "arg_hessian" = checkmate::assert_string(na.ok = TRUE),
      "gradient_as_attribute" = checkmate::assert_flag(),
      "hessian_as_attribute" = checkmate::assert_flag(),
      "out_value" = checkmate::assert_multi_class(
        classes = c("character", "function")
      ),
      "out_parameter" = checkmate::assert_multi_class(
        classes = c("character", "function")
      ),
      "direction" = checkmate::assert_choice(choices = c("min", "max"))
    ),
    allow_overwrite = FALSE,
    keys_reserved = c("custom"),
    dictionary_name = c("optimization algorithms")
  )

  ### add optimizers
  optimizer_dictionary$add(
    "label" = "lbfgsb3c::lbfgsb3c",
    "algorithm" = "lbfgsb3c::lbfgsb3c",
    "arg_objective" = "fn",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = "gr",
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "arg_initial" = "par",
    "out_value" = "value",
    "out_parameter" = "par",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "lbfgsb3c::lbfgsb3",
    "algorithm" = "lbfgsb3c::lbfgsb3",
    "arg_objective" = "fn",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = "gr",
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "arg_initial" = "par",
    "out_value" = "value",
    "out_parameter" = "par",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "lbfgsb3c::lbfgsb3f",
    "algorithm" = "lbfgsb3c::lbfgsb3f",
    "arg_objective" = "fn",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = "gr",
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "arg_initial" = "par",
    "out_value" = "value",
    "out_parameter" = "par",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "lbfgsb3c::lbfgsb3x",
    "algorithm" = "lbfgsb3c::lbfgsb3x",
    "arg_objective" = "fn",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = "gr",
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "arg_initial" = "par",
    "out_value" = "value",
    "out_parameter" = "par",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "stats::nlm",
    "algorithm" = "stats::nlm",
    "arg_objective" = "f",
    "arg_initial" = "p",
    "arg_lower" = NA,
    "arg_upper" = NA,
    "arg_gradient" = "gradient",
    "arg_hessian" = "hessian",
    "gradient_as_attribute" = TRUE,
    "hessian_as_attribute" = TRUE,
    "out_value" = "minimum",
    "out_parameter" = "estimate",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "stats::nlminb",
    "algorithm" = "stats::nlminb",
    "arg_objective" = "objective",
    "arg_initial" = "start",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = "gradient",
    "arg_hessian" = "hessian",
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "out_value" = "objective",
    "out_parameter" = "par",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "stats::optim",
    "algorithm" = "stats::optim",
    "arg_objective" = "fn",
    "arg_initial" = "par",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = "gr",
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "out_value" = "value",
    "out_parameter" = "par",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "ucminf::ucminf",
    "algorithm" = "ucminf::ucminf",
    "arg_objective" = "fn",
    "arg_initial" = "par",
    "arg_lower" = NA,
    "arg_upper" = NA,
    "arg_gradient" = "gr",
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "out_value" = "value",
    "out_parameter" = "par",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "nloptr::nloptr",
    "algorithm" = "optimizeR::adapter_nloptr",
    "arg_objective" = "eval_f",
    "arg_initial" = "x0",
    "arg_lower" = "lb",
    "arg_upper" = "ub",
    "arg_gradient" = "eval_grad_f",
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "out_value" = "objective",
    "out_parameter" = "solution",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "vntrs::vntrs",
    "algorithm" = "optimizeR::adapter_vntrs",
    "arg_objective" = "f",
    "arg_initial" = "initial",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = NA,
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "out_value" = "value",
    "out_parameter" = "parameter",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "ao::ao",
    "algorithm" = "ao::ao",
    "arg_objective" = "f",
    "arg_initial" = "initial",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = "gradient",
    "arg_hessian" = "hessian",
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "out_value" = "value",
    "out_parameter" = "estimate",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "optimx::optimr",
    "algorithm" = "optimizeR::adapter_optimr",
    "arg_objective" = "fn",
    "arg_initial" = "par",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = "gr",
    "arg_hessian" = "hess",
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "out_value" = "value",
    "out_parameter" = "par",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "DEoptim::DEoptim",
    "algorithm" = "optimizeR::adapter_deoptim",
    "arg_objective" = "fn",
    "arg_initial" = "initial",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = NA,
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "out_value" = "value",
    "out_parameter" = "parameter",
    "direction" = "min"
  )

  optimizer_dictionary$add(
    "label" = "GenSA::GenSA",
    "algorithm" = "optimizeR::adapter_gensa",
    "arg_objective" = "fn",
    "arg_initial" = "par",
    "arg_lower" = "lower",
    "arg_upper" = "upper",
    "arg_gradient" = NA,
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "out_value" = "value",
    "out_parameter" = "parameter",
    "direction" = "min"
  )

  ### return
  optimizer_dictionary
}
