#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom cli cli_abort
#' @importFrom cli cli_warn
#' @importFrom lbfgsb3c lbfgsb3c
#' @importFrom R6 R6Class
#' @importFrom stats nlm
#' @importFrom stats nlminb
#' @importFrom stats optim
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom TestFunctions TF_ackley
#' @importFrom ucminf ucminf
#' @importFrom utils install.packages
## usethis namespace: end
NULL

#' @noRd
.onLoad <- function(lib, pkg) {
  optimizer_dictionary$add(
    "label" = "optimizeR::test_optimizer",
    "algorithm" = test_optimizer,
    "arg_objective" = "objective",
    "arg_initial" = "initial",
    "arg_lower" = NA,
    "arg_upper" = NA,
    "arg_gradient" = NA,
    "arg_hessian" = NA,
    "gradient_as_attribute" = FALSE,
    "hessian_as_attribute" = FALSE,
    "out_value" = "value",
    "out_parameter" = "parameter",
    "direction" = "min"
  )
  invisible()
}
