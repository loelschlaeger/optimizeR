
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

The `{optimizeR}` package provides an object-oriented framework for
optimizer functions and offers some convenience when minimizing or
maximizing in R.

**You won’t need the package if you…**

- already know which optimizer you want to use and if you are happy with
  its constraints,
- want to compare optimizers that are covered by
  [`{optimx}`](https://CRAN.R-project.org/package=optimx),
- or search for optimization algorithms (because the package does not
  implement any optimizer functions itself).

**But you might find the package useful if you want to…**

- compare any optimizer function (already implemented in R or
  implemented by yourself),
- have consistently named inputs and outputs across different
  optimizers,
- view optimizers as objects (which can be helpful when implementing
  packages that depend on optimization),
- use optimizers for both minimization and maximization,
- optimize over more than one function argument,
- measure computation time or set a time limit for long optimization
  tasks.

The [package
vignette](https://loelschlaeger.de/optimizeR/articles/optimizeR.html)
has more details on these benefits and how we implemented them.

## Usage

This demonstration is a bit artificial but showcases the package
purpose. Assume you want to

- maximize a function,
- over two of its arguments,
- interrupt optimization if it exceeds 10 seconds,
- and compare the performance between `stats::nlm` and
  `pracma::nelder_mead`.

This task can very easily be achieved with `{optimizeR}`:

**1. Define the objective function**

``` r
f <- function(a, b, x, y) {
  a * exp(-0.2 * sqrt(0.5 * (x^2 + y^2))) +
    exp(0.5 * (cos(2 * pi * x) + cos(2 * pi * y))) - exp(1) - b
}
```

For `a = b = 20`, this is the inverted [Ackley
function](https://en.wikipedia.org/wiki/Ackley_function). We want to
keep `a` and `b` fixed and optimize over `x` and `y`, which are both
single numeric values.

``` r
objective <- Objective$new(
  objective = f, 
  target = c("x", "y"), 
  npar = c(1, 1), 
  "a" = 20, 
  "b" = 20
)
```

**2. Create the optimizer objects**

``` r
nlm <- Optimizer$new(which = "stats::nlm")
```

Explain custom.

``` r
nelder_mead <- Optimizer$new(which = "custom")
#> Please use method `$definition()` next to define a custom optimizer.
nelder_mead$definition(
  algorithm = pracma::nelder_mead, # the optimization function
  arg_objective = "fn",            # the argument name for the objective function
  arg_initial = "x0",              # the argument name for the initial values
  out_value = "fmin",              # the element for the optimal function value in the output
  out_parameter = "xmin",          # the element for the optimal parameters in the output
  direction = "min"                # the optimizer minimizes
)
```

**3. Set a time limit**

Time limit optional. Here it makes no sense.

``` r
nlm$seconds <- 10
nelder_mead$seconds <- 10
```

**4. Maximize the target function**

Comment on outcome.

``` r
initial <- c(3, 3)
nlm$maximize(objective, initial)
#> $value
#> [1] -6.559645
#> 
#> $parameter
#> [1] 1.974451 1.974451
#> 
#> $seconds
#> [1] 0.03366303
#> 
#> $initial
#> [1] 3 3
#> 
#> $gradient
#> [1] 5.577962e-08 5.577962e-08
#> 
#> $code
#> [1] 1
#> 
#> $iterations
#> [1] 6
nelder_mead$maximize(objective, initial)
#> $value
#> [1] 0
#> 
#> $parameter
#> [1] 0 0
#> 
#> $seconds
#> [1] 0.01516891
#> 
#> $initial
#> [1] 3 3
#> 
#> $count
#> [1] 105
#> 
#> $convergence
#> [1] 0
#> 
#> $info
#> $info$solver
#> [1] "Nelder-Mead"
#> 
#> $info$restarts
#> [1] 0
```

## Installation

You can install the released package version from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("optimizeR")
```

## Roadmap

- [ ] The package already provides a dictionary that stores optimizers
  together with information about names of their inputs and outputs. We
  want to extend this dictionary with more optimizers that are commonly
  used.

- [ ] We want to use alias for optimizers in the dictionary that group
  optimizers into classes (such as “Unconstrained optimization”,
  “Constrained Optimization” etc.). This would help to find alternative
  optimizers for a given task.

- [ ] We want to implement a `$summary()` method for an optimizer object
  that gives an overview of the optimizer, its arguments, and its
  properties.

## Contact

You have a question, found a bug, request a feature, give feedback, want
to contribute? We are happy to hear from you, [please file an issue on
GitHub](https://github.com/loelschlaeger/optimizeR/issues/new/choose).
