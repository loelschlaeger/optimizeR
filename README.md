
<!-- README.md is generated from README.Rmd. Please edit that file -->

# optimizeR <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/optimizeR)](https://CRAN.R-project.org/package=optimizeR)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/last-month/optimizeR)](https://cran.r-project.org/package=optimizeR)
[![R-CMD-check](https://github.com/loelschlaeger/optimizeR/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/optimizeR/actions)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/optimizeR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/optimizeR?branch=master)
<!-- badges: end -->

If you’re looking for a way to standardize the inputs and outputs of
numerical optimizers in R, you might find the {optimizeR} package
useful. This package provides a unified framework for representing the
inputs and outputs of different optimizers. It does not actually
implement any optimizer functions itself.

## What is the problem?

When working with popular R optimizers such as
[`stats::nlm()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
and
[`stats::optim()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html),
it can be challenging to compare their results due to inconsistencies in
function arguments and output labels. For instance, `stats::nlm()` uses
`f` as its function argument and `p` as the argument for initial values,
while `stats::optim()` uses `fn` for its function argument and `par` for
initial values. Additionally, the optimal parameters and function values
are labeled differently, with `estimate` and `minimum` used in
`stats::nlm()` and `par` and `value` in `stats::optim()`. And all is
different again with, for example,
[`pracma::nelder_mead()`](https://CRAN.R-project.org/package=pracma).
This inconsistency is frustrating, especially if one wants to apply and
compare different optimizers.

## Our solution

To standardize the inputs and outputs of different R optimizers and make
them easier to apply and compare, you can define them using the
`define_optimizer()` function and then apply them using
`apply_optimizer()`. This way, the inputs and outputs will always be in
a consistent format across different optimizers.

For demonstration, say we want to minimize the [Ackley
function](https://en.wikipedia.org/wiki/Ackley_function) …

``` r
f_ackley <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) -
    exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
}
```

… and compare the performance of three optimizers: `stats::nlm()`,
`stats::optim()`, and `pracma::nelder_mead()`. The first two are already
pre-specified …

``` r
optimizer_nlm()
#> <optimizer 'stats::nlm'>
optimizer_optim()
#> <optimizer 'stats::optim'>
```

… and for the latter (as for any other optimizer you like) we can use
the general constructor:

``` r
optimizer_nelder_mead <- define_optimizer(
  optimizer = pracma::nelder_mead,
  objective = "fn",
  initial = "x0",
  value = "fmin",
  parameter = "xmin"
)
```

Now we optimize (with initial parameter vector `initial = c(-1, 1)`):

``` r
results <- lapply(
  list(optimizer_nlm(), optimizer_optim(), optimizer_nelder_mead),
  apply_optimizer, 
  objective = f_ackley, 
  initial = c(-1, 1)
)
names(results) <- c("stats::nlm", "stats::optim", "pracma::nelder_mead")
```

In the optimization output, `value` and `parameter` consistently denote
the optimal function values and the optimal parameters, while additional
optimizer-specific outputs are preserved. The optimization time in
seconds, `seconds`, and the initial parameter vector, `initial`, are
added:

``` r
str(results)
#> List of 3
#>  $ stats::nlm         :List of 7
#>   ..$ value     : num 1.66e-06
#>   ..$ parameter : num [1:2] -2.91e-07 5.08e-07
#>   ..$ seconds   : num 0.0316
#>   ..$ initial   : num [1:2] -1 1
#>   ..$ gradient  : num [1:2] -0.00824 0.0144
#>   ..$ code      : int 2
#>   ..$ iterations: int 33
#>  $ stats::optim       :List of 7
#>   ..$ value      : num 3.57
#>   ..$ parameter  : num [1:2] -0.969 0.969
#>   ..$ seconds    : num 0.000361
#>   ..$ initial    : num [1:2] -1 1
#>   ..$ counts     : Named int [1:2] 45 NA
#>   .. ..- attr(*, "names")= chr [1:2] "function" "gradient"
#>   ..$ convergence: int 0
#>   ..$ message    : NULL
#>  $ pracma::nelder_mead:List of 7
#>   ..$ value      : num 0
#>   ..$ parameter  : num [1:2] 0 0
#>   ..$ seconds    : num 0.0014
#>   ..$ initial    : num [1:2] -1 1
#>   ..$ count      : num 111
#>   ..$ convergence: num 0
#>   ..$ info       :List of 2
#>   .. ..$ solver  : chr "Nelder-Mead"
#>   .. ..$ restarts: num 0
```

By the way, are you surprised to see that `value` for `stats::optim()`
is different from the other optimizers? It seems that this optimizer has
become trapped in a local minimum. If you are interested in exploring
the initialization problem in numerical optimization, you may find the
[{ino} R package](https://github.com/loelschlaeger/ino) to be useful.

By the way, are you surprised that the result from `stats::optim()`
differs from the other optimizers? It appears that this optimizer got
stuck in a local minimum. If you are interested in the initialization
problem of numerical optimization, you might find the {ino} package
helpful.

## Installation

You can install the released version of {optimizeR} from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("optimizeR")
```

… and the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("loelschlaeger/optimizeR")
```

## Contact

Have a question, found a bug, request a feature, want to contribute?
[Please file an issue on
GitHub](https://github.com/loelschlaeger/optimizeR/issues/new/choose).
