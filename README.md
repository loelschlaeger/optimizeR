
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {optimizeR}

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/optimizeR)](https://CRAN.R-project.org/package=optimizeR)
[![R-CMD-check](https://github.com/loelschlaeger/optimizeR/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/optimizeR/actions)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/optimizeR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/optimizeR?branch=master)
<!-- badges: end -->

The {optimizeR} package provides a unified framework for numerical
optimizers in R, particularly for their inputs and outputs.

## What is the problem?

Look at the popular R optimizers
[`nlm()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
and
[`optim()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html):
The function argument in `nlm()` is called `f`, in `optim()` it is `fn`.
The argument for the initial values is called `p` in `nlm()`, and `par`
in `optim()`. The optimal parameters and the optimal function values in
the output of `nlm()` are labeled `estimate` and `minimum`,
respectively, in `optim()` it is `par` and `value`. And all is different
again with
[`pracma::nelder_mead()`](https://CRAN.R-project.org/package=pracma).
This inconsistency is painful, especially if one wants to apply and
compare different optimizers.

## Our solution

Simply specify optimizers with `set_optimizer()` and execute them with
`apply_optimizer()`. The outputs are in a standardized format.

For demonstration, say we want to minimize the [Ackley
function](https://en.wikipedia.org/wiki/Ackley_function)…

``` r
f_ackley <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) -
    exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
}
```

… and compare three optimizers: `nlm()`, `optim()`, and
`pracma::nelder_mead()`. The first two are already pre-specified…

``` r
# library("optimizeR")
devtools::load_all()
#> ℹ Loading optimizeR
optimizer_nlm()
#> <optimizer 'stats::nlm'>
optimizer_optim()
#> <optimizer 'stats::optim'>
```

… and for the latter (as for any other optimizer) we can use the general
constructor:

``` r
optimizer_nelder_mead <- set_optimizer(
    opt_fun = pracma::nelder_mead,
    f = "fn",
    p = "x0",
    v = "fmin",
    z = "xmin"
  )
```

Now we optimize (with initial parameter vector `p = c(-1,1)`):

``` r
res <- lapply(
  list(optimizer_nlm(), optimizer_optim(), optimizer_nelder_mead),
  apply_optimizer, 
  f = f_ackley, 
  p = c(-1,1)
)
names(res) <- c("nlm", "optim", "nelder_mead")
```

In the optimization results, `v` and `z` consistently denote the optimal
function values and the optimal parameters, while optimizer-specific
outputs are preserved:

``` r
str(res)
#> List of 3
#>  $ nlm        :List of 6
#>   ..$ v         : num 1.66e-06
#>   ..$ z         : num [1:2] -2.91e-07 5.08e-07
#>   ..$ time      : 'difftime' num 0.000730991363525391
#>   .. ..- attr(*, "units")= chr "secs"
#>   ..$ gradient  : num [1:2] -0.00824 0.0144
#>   ..$ code      : int 2
#>   ..$ iterations: int 33
#>  $ optim      :List of 6
#>   ..$ v          : num 3.57
#>   ..$ z          : num [1:2] -0.969 0.969
#>   ..$ time       : 'difftime' num 0.000398159027099609
#>   .. ..- attr(*, "units")= chr "secs"
#>   ..$ counts     : Named int [1:2] 45 NA
#>   .. ..- attr(*, "names")= chr [1:2] "function" "gradient"
#>   ..$ convergence: int 0
#>   ..$ message    : NULL
#>  $ nelder_mead:List of 6
#>   ..$ v          : num 0
#>   ..$ z          : num [1:2] 0 0
#>   ..$ time       : 'difftime' num 0.00134801864624023
#>   .. ..- attr(*, "units")= chr "secs"
#>   ..$ count      : num 111
#>   ..$ convergence: num 0
#>   ..$ info       :List of 2
#>   .. ..$ solver  : chr "Nelder-Mead"
#>   .. ..$ restarts: num 0
```

P.S. Surprised that the `optim()` result is different from the others?
It seems that this optimizer got stuck in a local minimum.

## Installation

You can install the released version of {optimizeR} from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("optimizeR")
```

…and the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("loelschlaeger/optimizeR")
```

## Contact

Have a question, found a bug, request a feature, want to contribute?
[Please file an
issue](https://github.com/loelschlaeger/optimizeR/issues/new/choose).
