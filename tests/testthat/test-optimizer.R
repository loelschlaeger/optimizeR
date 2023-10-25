# TODO: add tests

objective <- function(x) x^4 + 3*x - 5

target = "x"
npar = 1
initial = 2
direction = "min"
seconds = Inf

optimizer_nlm <- Optimizer$new("stats::nlm")

self <- optimizer_nlm
private <- self$.__enclos_env__$private

optimizer_nlm$apply(objective, target = target, npar = npar, initial = inital)
