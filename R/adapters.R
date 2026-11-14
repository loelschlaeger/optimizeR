### packages that are required by the adapters
adapter_packages <- c(
  "adapter_nloptr" = "nloptr",
  "adapter_vntrs" = "vntrs",
  "adapter_optimr" = "optimx",
  "adapter_deoptim" = "DEoptim",
  "adapter_gensa" = "GenSA"
)

#' @keywords internal
#' @noRd

adapter_nloptr <- function(
    x0, eval_f, eval_grad_f = NULL, lb = NULL, ub = NULL, opts = list(), ...
  ) {

  ### set default algorithm and tolerance
  if (is.null(opts$algorithm)) {
    opts$algorithm <- ifelse(
      is.null(eval_grad_f), "NLOPT_LN_BOBYQA", "NLOPT_LD_LBFGS"
    )
  }
  if (is.null(opts$xtol_rel)) {
    opts$xtol_rel <- 1e-8
  }

  ### optimize (the objective function must not have further arguments)
  out <- nloptr::nloptr(
    x0 = x0,
    eval_f = function(x) eval_f(x),
    eval_grad_f = if (!is.null(eval_grad_f)) function(x) eval_grad_f(x),
    lb = lb, ub = ub, opts = opts, ...
  )

  ### return
  out[c("objective", "solution", "status", "message", "iterations")]

}

#' @keywords internal
#' @noRd

adapter_vntrs <- function(f, initial, lower = NULL, upper = NULL, ...) {
  npar <- length(initial)
  optima <- vntrs::vntrs(
    f = function(x) f(x), npar = npar, lower = lower, upper = upper, ...
  )
  if (is.null(optima) || nrow(optima) == 0) {
    cli::cli_abort("No optimum could be determined.", call = NULL)
  }
  list(
    "value" = optima$value[1],
    "parameter" = as.numeric(optima[1, seq_len(npar)]),
    "optima" = optima
  )
}

#' @keywords internal
#' @noRd

adapter_optimr <- function(
    par, fn, gr = NULL, hess = NULL, method = NULL, lower = -Inf, upper = Inf,
    ...
  ) {

  ### set default method
  if (is.null(method)) {
    bounded <- any(is.finite(lower)) || any(is.finite(upper))
    method <- ifelse(bounded, "L-BFGS-B", "Nelder-Mead")
  }

  ### optimize
  out <- optimx::optimr(
    par = par, fn = fn, gr = gr, hess = hess, method = method, lower = lower,
    upper = upper, ...
  )

  ### check if method could be applied
  if (anyNA(out$par)) {
    reason <- if (is.null(out$message)) "" else paste(":", trimws(out$message))
    cli::cli_abort(
      paste(
        "The method '{method}' failed with convergence code",
        "{out$convergence}{reason}"
      ),
      call = NULL
    )
  }

  ### return
  out$value <- as.numeric(out$value)
  out$par <- stats::setNames(as.numeric(out$par), names(par))
  out

}

#' @keywords internal
#' @noRd

adapter_deoptim <- function(
    fn, initial, lower = NULL, upper = NULL, control = list(), ...
  ) {
  require_bounds(
    lower = lower, upper = upper, npar = length(initial),
    label = "DEoptim::DEoptim"
  )
  control <- oeli::merge_lists(as.list(control), list("trace" = FALSE))
  out <- DEoptim::DEoptim(
    fn = function(x) fn(x), lower = lower, upper = upper,
    control = do.call(DEoptim::DEoptim.control, control), ...
  )
  list(
    "value" = out$optim$bestval,
    "parameter" = unname(out$optim$bestmem),
    "evaluations" = out$optim$nfeval,
    "iterations" = out$optim$iter
  )
}

#' @keywords internal
#' @noRd

adapter_gensa <- function(
    par, fn, lower = NULL, upper = NULL, control = list(), ...
  ) {
  require_bounds(
    lower = lower, upper = upper, npar = length(par), label = "GenSA::GenSA"
  )
  out <- GenSA::GenSA(
    par = par, fn = function(x) fn(x), lower = lower, upper = upper,
    control = control, ...
  )
  list("value" = out$value, "parameter" = out$par, "evaluations" = out$counts)
}

#' @keywords internal
#' @noRd

require_bounds <- function(lower, upper, npar, label) {
  lower_given <- checkmate::test_numeric(
    lower, finite = TRUE, any.missing = FALSE, len = npar
  )
  upper_given <- checkmate::test_numeric(
    upper, finite = TRUE, any.missing = FALSE, len = npar
  )
  if (!lower_given || !upper_given) {
    cli::cli_abort(
      paste(
        "The optimizer {.val {label}} searches between bounds and needs",
        "finite {.var lower} and {.var upper} bounds."
      ),
      call = NULL
    )
  }
  invisible(TRUE)
}
