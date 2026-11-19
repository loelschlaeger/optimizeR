# optimizeR: Unified Framework for Numerical Optimizers

Provides a unified object-oriented framework for numerical optimizers in
R. Supports minimization and maximization with any optimizer,
optimization over more than one function argument, computation time
measurement, and time limits for long optimization tasks.

## See also

Useful links:

- <https://loelschlaeger.de/optimizeR/>

- Report bugs at <https://github.com/loelschlaeger/optimizeR/issues>

## Author

**Maintainer**: Lennart Oelschläger <oelschlaeger.lennart@gmail.com>
([ORCID](https://orcid.org/0000-0001-5421-9313))

Authors:

- Lennart Oelschläger <oelschlaeger.lennart@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-5421-9313))

Other contributors:

- Marius Ötting <marius.oetting@uni-bielefeld.de>
  ([ORCID](https://orcid.org/0000-0002-9373-0365)) \[contributor\]

## Examples

``` r
### Task: compare minimization with 'stats::nlm' and 'pracma::nelder_mead'

# 1. define objective function (the Ackley function) and initial values
objective <- function(x) {
  -20 * exp(-0.2 * sqrt(mean(x^2))) - exp(mean(cos(2 * pi * x))) +
    20 + exp(1)
}
initial <- c(3, 3)

# 2. get overview of optimizers available in dictionary
optimizer_dictionary$keys
#>  [1] "lbfgsb3c::lbfgsb3c" "lbfgsb3c::lbfgsb3"  "lbfgsb3c::lbfgsb3f"
#>  [4] "lbfgsb3c::lbfgsb3x" "stats::nlm"         "stats::nlminb"     
#>  [7] "stats::optim"       "ucminf::ucminf"     "nloptr::nloptr"    
#> [10] "vntrs::vntrs"       "ao::ao"             "optimx::optimr"    
#> [13] "DEoptim::DEoptim"   "GenSA::GenSA"      

# 3. define 'nlm' optimizer
nlm <- Optimizer$new(which = "stats::nlm")

# 4. define the 'pracma::nelder_mead' optimizer
#    (not contained in the dictionary)
nelder_mead <- Optimizer$new(which = "custom")
#> Use method `$definition()` next to define a custom optimizer.
nelder_mead$definition(
  algorithm = pracma::nelder_mead, # optimization function
  arg_objective = "fn",            # objective function argument name
  arg_initial = "x0",              # argument name for the initial values
  out_value = "fmin",              # element for the optimal function value
  out_parameter = "xmin",          # element for the optimal parameters
  direction = "min"                # optimizer minimizes
)

# 5. compare the minimization results
nlm$minimize(objective, initial)
#> $value
#> [1] 6.559645
#> 
#> $parameter
#> [1] 1.974451 1.974451
#> 
#> $seconds
#> [1] 0.005558014
#> 
#> $initial
#> [1] 3 3
#> 
#> $error
#> [1] FALSE
#> 
#> $gradient
#> [1] 5.757896e-08 5.757896e-08
#> 
#> $code
#> [1] 1
#> 
#> $iterations
#> [1] 6
#> 
nelder_mead$minimize(objective, initial)
#> $value
#> [1] 4.440892e-16
#> 
#> $parameter
#> [1] 0 0
#> 
#> $seconds
#> [1] 0.02347732
#> 
#> $initial
#> [1] 3 3
#> 
#> $error
#> [1] FALSE
#> 
#> $count
#> [1] 105
#> 
#> $info
#> $info$solver
#> [1] "Nelder-Mead"
#> 
#> $info$restarts
#> [1] 0
#> 
#> 

## ------------------------------------------------
## Method `Optimizer$definition()`
## ------------------------------------------------

### define 'stats::optimize', which does not take initial values
optimize <- Optimizer$new(which = "custom", .verbose = FALSE)
optimize$definition(
  algorithm = stats::optimize,
  arg_objective = "f",
  arg_initial = NA,
  arg_lower = "lower",
  arg_upper = "upper",
  out_value = "objective",
  out_parameter = "minimum",
  direction = "min"
)
optimize$minimize(
  objective = function(x) x^4 + 3*x - 5, initial = 2, lower = -5,
  upper = 5
)
#> $value
#> [1] -7.044261
#> 
#> $parameter
#> [1] -0.9085611
#> 
#> $seconds
#> [1] 0.002953291
#> 
#> $initial
#> [1] 2
#> 
#> $error
#> [1] FALSE
#> 

## ------------------------------------------------
## Method `Optimizer$minimize()`
## ------------------------------------------------

Optimizer$new("stats::nlm")$
  minimize(objective = function(x) x^4 + 3*x - 5, initial = 2)
#> $value
#> [1] -7.044261
#> 
#> $parameter
#> [1] -0.9085614
#> 
#> $seconds
#> [1] 0.003938437
#> 
#> $initial
#> [1] 2
#> 
#> $error
#> [1] FALSE
#> 
#> $gradient
#> [1] -6.067147e-06
#> 
#> $code
#> [1] 1
#> 
#> $iterations
#> [1] 7
#> 

## ------------------------------------------------
## Method `Optimizer$maximize()`
## ------------------------------------------------

Optimizer$new("stats::nlm")$
  maximize(objective = function(x) -x^4 + 3*x - 5, initial = 2)
#> $value
#> [1] -2.955739
#> 
#> $parameter
#> [1] 0.9085598
#> 
#> $seconds
#> [1] 0.004786253
#> 
#> $initial
#> [1] 2
#> 
#> $error
#> [1] FALSE
#> 
#> $gradient
#> [1] -3.801404e-07
#> 
#> $code
#> [1] 1
#> 
#> $iterations
#> [1] 8
#> 

## ------------------------------------------------
## Method `Optimizer$optimize()`
## ------------------------------------------------

objective <- function(x) sin(x)
nlm <- Optimizer$new("stats::nlm")
nlm$optimize(objective = objective, initial = 2, direction = "min")
#> $value
#> [1] -1
#> 
#> $parameter
#> [1] 4.712387
#> 
#> $seconds
#> [1] 0.003632069
#> 
#> $initial
#> [1] 2
#> 
#> $error
#> [1] FALSE
#> 
#> $gradient
#> [1] -2.355968e-11
#> 
#> $code
#> [1] 1
#> 
#> $iterations
#> [1] 7
#> 
nlm$optimize(objective = objective, initial = 2, direction = "max")
#> $value
#> [1] 1
#> 
#> $parameter
#> [1] 1.570796
#> 
#> $seconds
#> [1] 0.00199461
#> 
#> $initial
#> [1] 2
#> 
#> $error
#> [1] FALSE
#> 
#> $gradient
#> [1] 1.166204e-08
#> 
#> $code
#> [1] 1
#> 
#> $iterations
#> [1] 3
#> 
```
