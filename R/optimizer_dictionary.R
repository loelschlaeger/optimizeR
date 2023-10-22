
# optimizer_dictionary <- oeli::Dictionary$new(
#   key = "label",
#   alias = "method",
#   values = c("algorithm", "arg_objective", "arg_initial", "out_value", "out_parameter", "direction", "arguments", "output_elements"),
#   value_types = c("function", "character", "character", "character", "character", "character", "list", "character"),
#   allow_overwrite = FALSE,
#   alias_allowed = c("constrained", "unconstrained")
# )$add(
#   "label" = "stats::nlm",
#   "method" = c("unconstrained"),
#   "algorithm" = stats::nlm,
#   "objective" = "f",
#   "initial" = "p",
#   "value" = "minimum",
#   "parameter" = "min",
#   "direction" = "min",
#   "arguments" = formals(stats::nlm),
#   "outputs" = c("minimum", "estimate", "gradient", "hessian", "code", "iterations")
# )$add(
#   "label" = "stats::optim",
#   "method" = c(""),
#   "algorithm" = stats::optim,
#   "objective" = "fn",
#   "initial" = "par",
#   "value" = "minimum",
#   "parameter" = "min",
#   "direction" = "min",
#   "arguments" = formals(stats::optim),
#   "outputs" = c("par", "value", "counts", "convergence", "message", "hessian")
# )



