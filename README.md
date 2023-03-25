
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

The {optimizeR} package provides a unified framework for numerical
optimizers in R, in particular for their inputs and outputs.

## What is the problem?

Look at the popular R optimizers
[`stats::nlm()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
and
[`stats::optim()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html):
The function argument in `stats::nlm()` is called `f`, in
`stats::optim()` it is `fn`. The argument for the initial values is
called `p` in `stats::nlm()`, and `par` in `stats::optim()`. The optimal
parameters and the optimal function values in the output of
`stats::nlm()` are labeled `estimate` and `minimum`, respectively, in
`stats::optim()` it is `par` and `value`. And all is different again
with, for example,
[`pracma::nelder_mead()`](https://CRAN.R-project.org/package=pracma).
This inconsistency is painful, especially if one wants to apply and
compare different optimizers.

## Our solution

Simply specify optimizers with `define_optimizer()` and apply them with
`apply_optimizer()`. The inputs and outputs will be in a standardized
format.

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

Now we optimize (with initial parameter vector `initial = c(-1,1)`):

``` r
results <- lapply(
  list(optimizer_nlm(), optimizer_optim(), optimizer_nelder_mead),
  apply_optimizer, 
  objective = f_ackley, 
  initial = c(-1, 1)
)
names(results) <- c("stats::nlm", "stats::optim", "pracma::nelder_mead")
```

In the optimization results, `value` and `parameter` consistently denote
the optimal function values and the optimal parameters, while
optimizer-specific outputs are preserved. The optimization time in
seconds, `seconds`, and the initial parameter vector, `initial`, are
added:

``` r
str(results)
#> List of 3
#>  $ stats::nlm         :List of 7
#>   ..$ value     : num 1.66e-06
#>   ..$ parameter : num [1:2] -2.91e-07 5.08e-07
#>   ..$ seconds   : num 0.0335
#>   ..$ initial   : num [1:2] -1 1
#>   ..$ gradient  : num [1:2] -0.00824 0.0144
#>   ..$ code      : int 2
#>   ..$ iterations: int 33
#>  $ stats::optim       :List of 7
#>   ..$ value      : num 3.57
#>   ..$ parameter  : num [1:2] -0.969 0.969
#>   ..$ seconds    : num 0.000742
#>   ..$ initial    : num [1:2] -1 1
#>   ..$ counts     : Named int [1:2] 45 NA
#>   .. ..- attr(*, "names")= chr [1:2] "function" "gradient"
#>   ..$ convergence: int 0
#>   ..$ message    : NULL
#>  $ pracma::nelder_mead:List of 7
#>   ..$ value      : num 0
#>   ..$ parameter  : num [1:2] 0 0
#>   ..$ seconds    : num 0.00167
#>   ..$ initial    : num [1:2] -1 1
#>   ..$ count      : num 111
#>   ..$ convergence: num 0
#>   ..$ info       :List of 2
#>   .. ..$ solver  : chr "Nelder-Mead"
#>   .. ..$ restarts: num 0
```

P.S. Surprised that the `stats::optim` result differs from the others?
It seems that this optimizer got stuck in a local minimum.

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
[Please file an
issue](https://github.com/loelschlaeger/optimizeR/issues/new/choose).
