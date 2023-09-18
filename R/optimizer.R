#' optim optimizer
#' @description
#' A short description...
#' @format NULL
#' @export

optimizer_nlm <- Optimizer$
  new(algorithm = stats::nlm, name = "nlm")$
  labels(
    objective = "f",
    initial = "p",
    value = "minimum",
    parameter = "min"
  )

#' @rdname optimizer_nlm
#' @description
#' Bla bla
#' @format NULL
#' @export

optimizer_optim <- Optimizer$
  new(algorithm = stats::nlm, name = "nlm")$
  labels(
    objective = "fn",
    initial = "par",
    value = "minimum",
    parameter = "min"
  )
