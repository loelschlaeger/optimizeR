#' Specify numerical optimizer
#'
#' @description
#' Use this function to specify the consistent framework for a numerical
#' optimizer.
#'
#' @format
#' The format of an \code{optimizer} object is documented in
#' \code{\link{new_optimizer}}.
#'
#' @param ...
#' Additional arguments to be passed to the optimizer. Without
#' specifications, the default values of the optimizer are used.
#' @inheritParams new_optimizer
#' @inheritParams validate_optimizer
#'
#' @return
#' An object of class \code{optimizer}.
#'
#' @seealso
#' [optimizer_nlm()] and [optimizer_optim()], two wrappers for the
#' \code{\link[stats]{nlm}} and \code{\link[stats]{optim}} optimizer.
#'
#' @export
#'
#' @examples
#' set_optimizer(
#'   opt_fun = pracma::nelder_mead,
#'   f = "fn",
#'   p = "x0",
#'   v = "fmin",
#'   z = "xmin",
#'   tol = 1e-6
#' )
#'
#' @keywords
#' specification

set_optimizer <- function(
    opt_fun, f, p, v, z, ..., out_ign = character(),
    test_par = list(
      validate = TRUE,
      f_test = f_ackley,
      npar = 2,
      add = list(),
      init_rest = list("lower" = -1, "upper" = 1),
      init_digits = 2,
      opt_checks = 10,
      opt_checks_time = 1
    )
) {
  if(missing(opt_fun)){
    optimizeR_stop(
      "Please specify argument 'opt_fun'.",
      "It should be a function that performs numerical optimization."
    )
  }
  if(missing(f)){
    optimizeR_stop(
      "Please specify argument 'f'.",
      "It should be the name of the function argument of 'opt_fun'."
    )
  }
  if(missing(p)){
    optimizeR_stop(
      "Please specify argument 'p'.",
      "It should be the name of the initial value argument of 'opt_fun'."
    )
  }
  if(missing(v)){
    optimizeR_stop(
      "Please specify argument 'v'.",
      "It should be the name of the optimal function value in the output list of 'opt_fun'."
    )
  }
  if(missing(z)){
    optimizeR_stop(
      "Pleas specify argument 'z'.",
      "It should be the name of the optimal parameter vector in the output list of 'opt_fun'."
    )
  }
  validate_optimizer(
    x = new_optimizer(
      opt_fun = opt_fun, opt_name = deparse(substitute(opt_fun)), add = list(...),
      f = f, p = p, v = v, z = z, out_ign = out_ign
    ),
    test_par = test_par
  )
}

#' Constructor
#'
#' @description
#' This function constructs an \code{optimizer} object.
#'
#' @details
#' The \code{optimizer} object defines a numerical optimizer.
#'
#' @format
#' An \code{optimizer} object is a \code{list} of five elements:
#' * The \code{opt_fun} element is the optimization function \code{opt_fun}.
#' * The \code{opt_name} element is the name of \code{opt_fun}.
#' * The \code{add} element is a named \code{list}, where each element is an
#'   additional function argument for \code{opt_fun}.
#' * The \code{arg_names} element is a named \code{list} of \code{characters}:
#'   * \code{f} (the name of the function input of \code{opt_fun}),
#'   * \code{p} (the name of the starting parameter values input of \code{opt_fun}),
#'   * \code{v} (the name of the optimal function value in the output list of
#'     \code{opt_fun}),
#'   * and \code{z} (the name of the optimal parameter vector in the output list
#'     of \code{opt_fun}).
#' * The \code{out_ign} element is a \code{character} vector of element names in the
#'   output of \code{opt_fun} that are not saved. The elements \code{v} and \code{z}
#'   are added automatically to \code{opt_ign}, because they are saved
#'   separately, see the output documentation of \code{\link{apply_optimizer}}.
#'
#' @param x
#' A \code{list}.
#' @param opt_fun
#' An object of class \code{function}, a numerical optimizer.
#' * It must have an input \code{f} for a \code{function}, which is optimized
#'   over its first argument.
#' * It must have an input \code{p} for a \code{numerical} vector, the initial
#'   parameter values.
#' * It must have a \code{...} argument for additional parameters to \code{f}.
#' * The output must be a named \code{list}, including the optimal function value
#'   (named as \code{v}) and parameter vector (named as \code{z}).
#' @param opt_name
#' A \code{character}, the name of \code{opt_fun}.
#' @param add
#' A \code{list} of additional and named arguments to be passed to \code{opt_fun}.
#' @param f
#' A \code{character}, the name of the function input of \code{opt_fun}.
#' @param p
#' A \code{character}, the name of the starting parameter values input of \code{opt_fun}.
#' @param v
#' A \code{character}, the name of the optimal function value in the output list of \code{opt_fun}.
#' @param z
#' A \code{character}, the name of the optimal parameter vector in the output list of \code{opt_fun}.
#' @param out_ign
#' A \code{character} vector of element names in the output of \code{opt_fun} that are not
#' saved. The elements \code{v} and \code{z} are added automatically to
#' \code{opt_ign}, because they are saved separately, see the output
#' documentation of \code{\link{apply_optimizer}}.
#'
#' @return
#' An object of class \code{optimizer}.
#'
#' @keywords
#' internal

new_optimizer <- function(
    x = list(), opt_fun = function() {}, opt_name = character(), add = list(),
    f = character(), p = character(), v = character(), z = character(),
    out_ign = character()
) {
  stopifnot(is.list(x))
  stopifnot(is.function(opt_fun))
  stopifnot(is.character(opt_name))
  stopifnot(is.list(add))
  stopifnot(is.character(f))
  stopifnot(is.character(p))
  stopifnot(is.character(v))
  stopifnot(is.character(z))
  stopifnot(is.character(out_ign))
  x[["opt_fun"]] <- opt_fun
  x[["opt_name"]] <- opt_name
  x[["add"]] <- add
  x[["arg_names"]] <- list(f = f, p = p, v = v, z = z)
  x[["out_ign"]] <- union(out_ign, c(v,z))
  structure(x, class = "optimizer")
}

#' Validator
#'
#' @description
#' This function validates an \code{optimizer} object.
#'
#' @param x
#' An object of class \code{optimizer}.
#' @param test_par
#' A \code{list} of test parameters for an \code{optimizer} object:
#' * \code{validate}, a \code{logical}, set to \code{TRUE} (\code{FALSE}) to (not)
#'   validate the \code{optimizer} object.
#'   By default, \code{validate = TRUE}.
#' * \code{f_test}, a test \code{function} to be optimized.
#'   By default, \code{f_test = \link{f_ackley}}.
#' * \code{npar}, the length of the first argument of \code{f_test}, i.e. the
#'   argument over which \code{f_test} is optimized.
#' * \code{add}, a \code{list} of additional arguments to \code{f_test}.
#' * \code{init_rest}, a \code{list} of two elements, \code{lower} and \code{upper},
#'   with lower and upper limits, respectively, for test initial values for the
#'   optimization of \code{f_test} with \code{opt_fun}.
#'   Can be single values (for joint limits) or \code{numeric} vectors of length
#'   \code{npar} (for individual limits).
#'   By default, \code{lower = -1} and \code{upper = 1}.
#' * \code{init_digits}, the number of decimal places for the test initial
#'   values.
#'   By default, \code{init_digits = 2}.
#' * \code{opt_checks}, the number of checks for \code{opt_fun} with random initial
#'   values (that fulfill the \code{init_rest} restrictions).
#'   By default, \code{opt_checks = 10}.
#' * \code{opt_check_time}, the maximum number of seconds for a single check for
#'   \code{opt_fun}.
#'   A check is considered to be successful, if no error occurred
#'   within \code{opt_check_time} seconds.
#'
#' @return
#' The validated input \code{x}.
#'
#' @keywords
#' internal
#'
#' @importFrom stats runif

validate_optimizer <- function(x = new_optimizer(), test_par = list()) {
  stopifnot(inherits(x, "optimizer"))
  stopifnot(is.list(test_par))
  if (!exists("validate", where = test_par)) {
    test_par[["validate"]] <- TRUE
  }
  if (test_par[["validate"]]) {
    if (!exists("f_test", where = test_par)) {
      test_par[["f_test"]] <- f_ackley
      test_par[["npar"]] <- 2
    }
    if (!exists("npar", where = test_par)) {
      optimizeR_stop(
        "Argument 'npar' in 'test_par' is not specified."
      )
    }
    if (!exists("add", where = test_par)) {
      test_par[["add"]] <- list()
    }
    if (!exists("init_rest", where = test_par)) {
      test_par[["init_rest"]] <- list("lower" = -1, "upper" = 1)
    }
    test_par[["init_rest"]] <- lapply(
      test_par[["init_rest"]], rep_len, length.out = test_par[["npar"]]
    )
    if (!exists("init_digits", where = test_par)) {
      test_par[["init_digits"]] <- 2
    }
    if (!exists("opt_checks", where = test_par)) {
      test_par[["opt_checks"]] <- 10
    }
    if (!exists("opt_checks_time", where = test_par)) {
      test_par[["opt_checks_time"]] <- 1
    }
    for (run in seq_len(test_par[["opt_checks"]])) {
      init <- sapply(
        X = seq_len(test_par[["npar"]]),
        FUN = function(s) {
          round(
            x = stats::runif(
              n = 1, min = test_par[["init_rest"]][["lower"]],
              max = test_par[["init_rest"]][["upper"]]
            ),
            digits = test_par[["init_digits"]]
          )
        }
      )
      opt_out <- try_silent(
        expr = timed(
          do.call(
            what = x[["opt_fun"]],
            args = c(
              structure(
                list(test_par[["f_test"]], init),
                names = x[["arg_names"]][c("f","p")]
              ),
              x[["add"]], test_par[["add"]]
            )
          ),
          secs = test_par[["opt_checks_time"]]
        )
      )
      if (is.null(opt_out)) {
        optimizeR_warn(
          paste(
            "Optimizer test run", run, "cannot be validated."
          ),
          paste(
            "Initial values:", paste(init, collapse = " "), "\n",
            "The test run returned NULL. The optimization most likely reached",
            "the time limit. Try to increase 'opt_checks_time'."
          ),
          immediate. = TRUE
        )
      } else if (inherits(opt_out, "fail")) {
        optimizeR_stop(
          paste(
            "Optimizer test run", run, "failed."
          ),
          paste(
            opt_out, "\nInitial values:", paste(init, collapse = " ")
          )
        )
      } else {
        if (!is.list(opt_out)) {
          optimizeR_stop(
            "Otimizer output is not a list.",
            "The 'opt_fun' function must return a named list."
          )
        }
        if (!x[["arg_names"]][["v"]] %in% names(opt_out)) {
          optimizeR_stop(
            "Element 'v' not contained in the optimizer output.",
            "Make sure that 'v' is contained in the output list."
          )
        } else {
          value <- opt_out[[x[["arg_names"]][["v"]]]]
          if (!(is.numeric(value) && length(value) == 1)) {
            optimizeR_stop(
              "The optimal function value is not a single numeric.",
              paste(
                "Initial values:", paste(init, collapse = " "), "\n",
                "Optimal function value:", value, "\n"
              )
            )
          }
        }
        if (!x[["arg_names"]][["z"]] %in% names(opt_out)) {
          optimizeR_stop(
            "Element 'z' not contained in the optimizer output.",
            "Make sure that 'z' is contained in the output list."
          )
        } else {
          optimum <- opt_out[[x[["arg_names"]][["z"]]]]
          if (!(is.numeric(optimum) && length(optimum) == test_par[["npar"]])) {
            optimizeR_stop(
              "The optimum is not a numeric value of length 'npar'.",
              paste(
                "Initial values:", paste(init, collapse = " "), "\n",
                "Optimum:", paste(optimum, collapse = " "), "\n"
              )
            )
          }
        }
      }
    }
  }
  return(x)
}

#' @exportS3Method
#' @noRd
#' @keywords
#' internal

print.optimizer <- function(x, ...) {
  stopifnot(inherits(x, "optimizer"))
  cat("<optimizer '", x[["opt_name"]], "'>", sep = "")
}

#' Specify nlm optimizer
#'
#' @description
#' This function is a wrapper for \code{\link{set_optimizer}} with the
#' \code{\link[stats]{nlm}} optimizer.
#'
#' @inheritParams set_optimizer
#'
#' @return
#' An object of class \code{optimizer}.
#'
#' @seealso
#' [set_optimizer()] for specifying a different optimizer.
#'
#' @export
#'
#' @importFrom stats nlm

optimizer_nlm <- function(..., out_ign = character(), test_par = list()) {
  set_optimizer(
    opt_fun = stats::nlm,
    f = "f",
    p = "p",
    v = "minimum" ,
    z = "estimate",
    ...,
    out_ign = out_ign,
    test_par = test_par
  )
}

#' Specify optim optimizer
#'
#' @description
#' This function is a wrapper for \code{\link{set_optimizer}} with the
#' \code{\link[stats]{optim}} optimizer.
#'
#' @inheritParams set_optimizer
#'
#' @return
#' An object of class \code{optimizer}.
#'
#' @seealso
#' [set_optimizer()] for specifying a different optimizer.
#'
#' @export
#'
#' @importFrom stats optim

optimizer_optim <- function(..., out_ign = character(), test_par = list()) {
  set_optimizer(
    opt_fun = stats::optim,
    f = "fn",
    p = "par",
    v = "value",
    z = "par",
    ...,
    out_ign = out_ign,
    test_par = test_par
  )
}

#' Optimization
#'
#' @description
#' This function performs numerical optimization using an \code{optimizer}
#' object.
#'
#' @param optimizer
#' An object of class \code{optimizer}.
#' @param f
#' The \code{function} to be optimized, returning a single \code{numeric}.
#' Its first argument must be a \code{numeric} vector of the length of \code{p},
#' followed by any other arguments specified by the \code{...} argument.
#' @param p
#' A \code{numeric} vector with starting parameter values for the optimization.
#' @param ...
#' Additional arguments to be passed to \code{f}.
#'
#' @return
#' A named \code{list}, containing the elements
#' * \code{v}, the value of the estimated optimum of \code{f},
#' * \code{z}, the parameter vector where the optimum of \code{f} is obtained,
#' * \code{time}, the total optimization time (as a \code{difftime} object),
#' * and additional output elements of the optimizer (if not excluded by the
#' \code{out_ign} element via \code{\link{set_optimizer}}).
#'
#' @seealso
#' [set_optimizer()] for specifying an \code{optimizer} object.
#'
#' @export
#'
#' @examples
#' apply_optimizer(optimizer_nlm(), function(x) -(x + 1)^2 + 1, 2)

apply_optimizer <- function(
    optimizer = optimizer_nlm(), f, p, ...
) {
  start <- Sys.time()
  res <- do.call(
    what = optimizer[["opt_fun"]],
    args = c(
      structure(list(f), names = optimizer[["arg_names"]][["f"]]),
      structure(list(p), names = optimizer[["arg_names"]][["p"]]),
      optimizer[["add"]], list(...)
    )
  )
  end <- Sys.time()
  c(
    structure(
      list(res[[optimizer[["arg_names"]][["v"]]]],
           res[[optimizer[["arg_names"]][["z"]]]],
           difftime(end, start)),
      names = c("v", "z", "time")
    ),
    res[!names(res) %in% optimizer[["out_ign"]]]
  )
}

#' Ackley function
#'
#' @references
#' https://en.wikipedia.org/wiki/Ackley_function
#'
#' @details
#' The function has multiple local minima and one global minimum in the origin.
#'
#' @param x
#' A \code{numeric} vector of length 2.
#'
#' @examples
#' f_ackley(c(0, 0))
#'
#' @export
#'
#' @return
#' The function value at \code{x}, a single \code{numeric} value.
#'
#' @keywords
#' internal function

f_ackley <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) -
    exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
}
