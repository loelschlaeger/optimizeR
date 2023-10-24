#' Specify numerical optimizer
#'
#' @description
#' The \code{Optimizer} object specifies the framework for a numerical
#' optimizer.
#'
#' @return
#' An \code{Optimizer} object.
#'
#' @export
#'
#' @examples
#' ### define optimizer
#' optimizer_nelder_mead <- Optimizer$
#'   new(
#'     algorithm = pracma::nelder_mead  # optimization function
#'     tol = 1e-6                       # additional optimizer argument
#'   )$
#'   labels(
#'     objective = "fn",                # name of function input
#'     initial = "x0",                  # name of initial input
#'     value = "fmin",                  # name of value output
#'     parameter = "xmin"               # name of parameter output
#'   )
#'
#' ### define objective function
#' objective <- TestFunctions::TF_ackley
#'
#' ### optimize
#' optimizer_nelder_mead$apply(objective, initial = 1:2)
#'
#' ### compare to another optimizer
#' optimizer_nlm$apply(objective, initial = 1:2)

Optimizer <- R6::R6Class(

  classname = "Optimizer",

  public = list(

    #' @param algorithm
    #'
    #' @param name
    #'
    #' @param direction
    #'
    #' @param ...
    #' Additional arguments to be passed to the algorithm. Without
    #' specifications, the default values are used.
    initialize = function(algorithm, name = NULL, direction = "min", ...) {

      if (missing(algorithm)) {
        cli::cli_abort(c(
          "x" = "Please specify argument {.var algorithm}.",
          "*" = "It should be a {.cls function} that performs numerical
                optimization."
          ), call = NULL
        )
      }

    },

    labels = function(objective, initial, value, parameter) {

    },

    validate = function(
      objective = test_objective, initial = 1:2, ..., seconds = 10
    ) {

    },

    apply = function(objective, initial, ...) {

    },

    print = function(...) {
      cat("<optimizer '", self$name, "'>", sep = "")
    },

    summary = function(...) {

    }
  ),

  private = list(

    .algorithm = NULL,
    .name = NULL,
    .direction = "min"

  ),

  active = list(

    name = function(value) {

    },

    time = function(value) {

    },

    arguments = function(value) {

    },

    direction = function(value) {

    },

    output_ignore = function(value) {

    }

  )

)
