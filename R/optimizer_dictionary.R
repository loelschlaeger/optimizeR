#' Dictionary of optimizer functions
#'
#' @description
#' The `optimizer_dictionary` object is a dictionary of currently implemented
#' numerical optimizer functions.
#'
#' @format
#' An \code{R6} object of class \code{\link[oeli]{Dictionary}}.
#'
#' @export

optimizer_dictionary <- oeli::Dictionary$new(
  key_name = "label",
  alias_name = "method",
  value_names = c(
    "algorithm",
    "arg_objective",
    "arg_initial",
    "out_value",
    "out_parameter",
    "direction"
  ),
  value_assert = alist(
    "algorithm" = checkmate::assert_function(),
    "arg_objective" = checkmate::assert_string(),
    "arg_initial" = checkmate::assert_string(),
    "out_value" = checkmate::assert_string(),
    "out_parameter" = checkmate::assert_string(),
    "direction" = checkmate::assert_choice(choices = c("min", "max"))
  ),
  allow_overwrite = FALSE,
  keys_reserved = c("custom"),
  dictionary_name = c("optimizer algorithms"),
  alias_choices = c("constrained", "unconstrained")
)

if (require("lbfgsb3c", quietly = TRUE)) {
  optimizer_dictionary$add(
    "label" = "lbfgsb3c::lbfgsb3c",
    "algorithm" = lbfgsb3c::lbfgsb3c,
    "arg_objective" = "fn",
    "arg_initial" = "par",
    "out_value" = "value",
    "out_parameter" = "par",
    "direction" = "min"
  )
}

if (require("stats", quietly = TRUE)) {
  optimizer_dictionary$add(
    "label" = "stats::nlm",
    "method" = c("unconstrained"),
    "algorithm" = stats::nlm,
    "arg_objective" = "f",
    "arg_initial" = "p",
    "out_value" = "minimum",
    "out_parameter" = "estimate",
    "direction" = "min"
  )$add(
    "label" = "stats::nlminb",
    "algorithm" = stats::nlminb,
    "arg_objective" = "objective",
    "arg_initial" = "start",
    "out_value" = "objective",
    "out_parameter" = "par",
    "direction" = "min"
  )$add(
    "label" = "stats::optim",
    "algorithm" = stats::optim,
    "arg_objective" = "fn",
    "arg_initial" = "par",
    "out_value" = "value",
    "out_parameter" = "par",
    "direction" = "min"
  )
}

if (require("ucminf", quietly = TRUE)) {
  optimizer_dictionary$add(
    "label" = "ucminf::ucminf",
    "algorithm" = ucminf::ucminf,
    "arg_objective" = "fn",
    "arg_initial" = "par",
    "out_value" = "value",
    "out_parameter" = "par",
    "direction" = "min"
  )
}
