#' Installing optimizer packages
#'
#' @description
#' This function installs packages that implement optimization algorithms.
#'
#' @return
#' No return value.
#'
#' @export

install_optimizer_packages <- function() {
  pkgs_required <- c("lbfgsb3c", "stats", "ucminf")
  pkgs_not_installed <- character()
  for (pkg in pkgs_required) {
    if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
      pkgs_not_installed <- c(pkgs_not_installed, pkg)
    }
  }
  if (length(pkgs_not_installed) > 0) {
    message(
      "You are about to install the following packages: ",
      paste(pkgs_not_installed, collapse = ", "), "."
    )
    install_pkgs <- oeli::user_confirm("Are you sure?", default = TRUE)
  } else {
    install_pkgs <- FALSE
  }
  if (install_pkgs) {
    for (pkg in pkgs_not_installed) {
      tryCatch(
        utils::install.packages(pkg),
        error = function(e) {
          message(
            "Please install the package '", pkg, "' yourself.\n",
            "I failed because of: ", e
          )
        }
      )
    }
    if (length(pkgs_not_installed) > 0) {
      message(
        "You need to reload {optimizeR} to update the optimizer dictionary."
      )
      reload_optimizeR <- oeli::user_confirm(
        "May I do this for you?",
        default = TRUE
      )
      if (reload_optimizeR) {
        library("optimizeR")
      } else {
        message("Okay, please reload {optimizeR} yourself.")
      }
    }
  }
}

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
    "out_value" = "minimum",
    "out_parameter" = "min",
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
