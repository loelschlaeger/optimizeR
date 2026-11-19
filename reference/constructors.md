# Create optimizer and objective objects

The function `optimizer()` creates an
[`Optimizer`](https://loelschlaeger.de/optimizeR/reference/Optimizer.md)
object and is equivalent to `Optimizer$new()`.

The function `objective()` creates an
[`Objective`](https://loelschlaeger.de/optimizeR/reference/Objective.md)
object and is equivalent to `Objective$new()`.

## Usage

``` r
optimizer(which, ...)

objective(f, target = NULL, npar, ...)
```

## Arguments

- which:

  \[`character(1)`\]  
  Either:

  - one of `optimizer_dictionary$keys` (the package of the optimizer
    must be installed)

  - or `"custom"`, in which case `$definition()` must be used to define
    the optimizer details.

- ...:

  For `optimizer()`, named arguments to pass to the optimizer algorithm.
  Additionally, `.verbose = FALSE` can be set to suppress the status
  message for `which = "custom"`.

  For `objective()`, optionally additional function arguments that are
  fixed during the optimization.

- f:

  \[`function`\]  
  A `function` to be optimized.

- target:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  The argument name(s) to optimize.

  If `NULL` (default), the first function argument is selected.

- npar:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The length of each target argument.

## Value

For `optimizer()`: An
[`Optimizer`](https://loelschlaeger.de/optimizeR/reference/Optimizer.md)
object.

For `objective()`: An
[`Objective`](https://loelschlaeger.de/optimizeR/reference/Objective.md)
object.

## Examples

``` r
### the Rosenbrock function
rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2

### minimize with 'stats::nlm'
nlm <- optimizer("stats::nlm", iterlim = 200)
nlm$minimize(rosenbrock, initial = c(-1.2, 1))
#> $value
#> [1] 3.973765e-12
#> 
#> $parameter
#> [1] 0.999998 0.999996
#> 
#> $seconds
#> [1] 0.01949906
#> 
#> $initial
#> [1] -1.2  1.0
#> 
#> $error
#> [1] FALSE
#> 
#> $gradient
#> [1] -6.539235e-07  3.335976e-07
#> 
#> $code
#> [1] 1
#> 
#> $iterations
#> [1] 23
#> 

### evaluate the objective
rosenbrock_objective <- objective(rosenbrock, npar = 2)
rosenbrock_objective$evaluate(c(1, 1))
#> [1] 0
```
