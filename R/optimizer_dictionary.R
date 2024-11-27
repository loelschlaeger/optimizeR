#' Dictionary of optimizer functions
#'
#' @description
#' A dictionary of currently included numerical optimizer functions in the
#' `{optimizeR}` package.
#'
#' @format
#' An \code{R6} object of class \code{\link[oeli]{Dictionary}}.
#'
#' @export
#'
#' @examples
#' print(optimizer_dictionary)

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
    "algorithm" = checkmate::assert_function(),
    "arg_objective" = checkmate::assert_string(),
    "arg_initial" = checkmate::assert_string(),
    "arg_lower" = checkmate::assert_string(na.ok = TRUE),
    "arg_upper" = checkmate::assert_string(na.ok = TRUE),
    "arg_gradient" = checkmate::assert_string(na.ok = TRUE),
    "arg_hessian" = checkmate::assert_string(na.ok = TRUE),
    "gradient_as_attribute" = checkmate::assert_flag(),
    "hessian_as_attribute" = checkmate::assert_flag(),
    "out_value" = checkmate::assert_string(),
    "out_parameter" = checkmate::assert_string(),
    "direction" = checkmate::assert_choice(choices = c("min", "max"))
  ),
  allow_overwrite = FALSE,
  keys_reserved = c("custom"),
  dictionary_name = c("optimization algorithms")
)

optimizer_dictionary$add(
  "label" = "lbfgsb3c::lbfgsb3c",
  "algorithm" = lbfgsb3c::lbfgsb3c,
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
  "algorithm" = lbfgsb3c::lbfgsb3,
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
  "algorithm" = lbfgsb3c::lbfgsb3f,
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
  "algorithm" = lbfgsb3c::lbfgsb3x,
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
  "algorithm" = stats::nlm,
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
  "algorithm" = stats::nlminb,
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
  "algorithm" = stats::optim,
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
  "algorithm" = ucminf::ucminf,
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
