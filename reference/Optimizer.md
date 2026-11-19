# Specify numerical optimizer object

The `Optimizer` object defines a numerical optimizer based on any
optimization algorithm implemented in R. The main advantage of working
with an `Optimizer` object instead of using the optimization function
directly lies in the standardized inputs and outputs.

Any R function that fulfills the following two constraints can be
defined as an `Optimizer` object:

1.  It must have an input for a `function`, the objective function to be
    optimized.

2.  Its output must include the optimal function value and the optimal
    parameter vector.

## Active bindings

- `label`:

  \[`character(1)`\]  
  The label for the optimizer.

- `algorithm`:

  \[`function`\]  
  The optimization algorithm.

- `arg_objective`:

  \[`character(1)`\]  
  The argument name for the objective function in `algorithm`.

- `arg_initial`:

  \[`character(1)` \| `NA`\]  
  The argument name for the initial values in `algorithm`.

  Can be `NA` if `algorithm` does not take initial values.

- `arg_lower`:

  \[`character(1)` \| `NA`\]  
  Optionally the argument name for the lower parameter bound in
  `algorithm`.

  Can be `NA` if not available.

- `arg_upper`:

  \[`character(1)` \| `NA`\]  
  Optionally the argument name for the upper parameter bound in
  `algorithm`.

  Can be `NA` if not available.

- `arg_gradient`:

  \[`character(1)` \| `NA`\]  
  Optionally the argument name for the gradient function in `algorithm`.

  Can be `NA` if not available.

- `arg_hessian`:

  \[`character(1)` \| `NA`\]  
  Optionally the argument name for the Hessian function in `algorithm`.

  Can be `NA` if not available.

- `gradient_as_attribute`:

  \[`logical(1)`\]  
  Only relevant if `arg_gradient` is not `NA`.

  In that case, does `algorithm` expect that the gradient is an
  attribute of the objective function output (as for example in
  [`nlm`](https://rdrr.io/r/stats/nlm.html))? In that case,
  `arg_gradient` defines the attribute name.

- `hessian_as_attribute`:

  \[`logical(1)`\]  
  Only relevant if `arg_hessian` is not `NA`.

  In that case, does `algorithm` expect that the Hessian is an attribute
  of the objective function output (as for example in
  [`nlm`](https://rdrr.io/r/stats/nlm.html))? In that case,
  `arg_hessian` defines the attribute name.

- `out_value`:

  \[[`character()`](https://rdrr.io/r/base/character.html) \|
  `function`\]  
  The element name for the optimal function value in the output of
  `algorithm`.

  Can be a vector of names if the output is nested.

  Alternatively, it can also be a `function` that receives the output of
  `algorithm` and returns the optimal function value.

- `out_parameter`:

  \[[`character()`](https://rdrr.io/r/base/character.html) \|
  `function`\]  
  The element name for the optimal parameters in the output of
  `algorithm`.

  Can be a vector of names if the output is nested.

  Alternatively, it can also be a `function` that receives the output of
  `algorithm` and returns the optimal parameters.

- `direction`:

  \[`character(1)`\]  
  Either `"min"` (if the optimizer minimizes) or `"max"` (if the
  optimizer maximizes).

- `arguments`:

  \[[`list()`](https://rdrr.io/r/base/list.html)\]  
  Custom arguments for `algorithm`.

  Defaults are used for arguments that are not specified.

- `seconds`:

  \[`numeric(1)`\]  
  A time limit in seconds.

  Optimization is interrupted prematurely if `seconds` is exceeded.

  No time limit if `seconds = Inf` (the default).

  Note the limitations documented in
  [`setTimeLimit`](https://rdrr.io/r/base/setTimeLimit.html).

  The time limit refers to elapsed time and is also checked each time
  the optimizer evaluates the objective function.

- `hide_warnings`:

  \[`logical(1)`\]  
  Hide warnings during optimization?

- `output_ignore`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Elements to ignore (not include) in the optimization output.

## Methods

### Public methods

- [`Optimizer$new()`](#method-Optimizer-initialize)

- [`Optimizer$definition()`](#method-Optimizer-definition)

- [`Optimizer$set_arguments()`](#method-Optimizer-set_arguments)

- [`Optimizer$minimize()`](#method-Optimizer-minimize)

- [`Optimizer$maximize()`](#method-Optimizer-maximize)

- [`Optimizer$optimize()`](#method-Optimizer-optimize)

- [`Optimizer$print()`](#method-Optimizer-print)

- [`Optimizer$clone()`](#method-Optimizer-clone)

------------------------------------------------------------------------

### `Optimizer$new()`

Initializes a new `Optimizer` object.

#### Usage

    Optimizer$new(which, ..., .verbose = TRUE)

#### Arguments

- `which`:

  \[`character(1)`\]  
  Either:

  - one of `optimizer_dictionary$keys`

  - or `"custom"`, in which case `$definition()` must be used to define
    the optimizer details.

- `...`:

  \[`any`\]  
  In `Optimizer$new()` and `$set_arguments()`, named arguments to pass
  to the optimizer algorithm. In `$minimize()`, `$maximize()`, and
  `$optimize()`, named arguments to pass to the objective function.

- `.verbose`:

  \[`logical(1)`\]  
  Print status messages?

#### Returns

A new `Optimizer` object.

------------------------------------------------------------------------

### `Optimizer$definition()`

Defines an optimizer.

#### Usage

    Optimizer$definition(
      algorithm,
      arg_objective,
      arg_initial,
      arg_lower = NA,
      arg_upper = NA,
      arg_gradient = NA,
      arg_hessian = NA,
      gradient_as_attribute = FALSE,
      hessian_as_attribute = FALSE,
      out_value,
      out_parameter,
      direction
    )

#### Arguments

- `algorithm`:

  \[`function`\]  
  The optimization algorithm.

- `arg_objective`:

  \[`character(1)`\]  
  The argument name for the objective function in `algorithm`.

- `arg_initial`:

  \[`character(1)` \| `NA`\]  
  The argument name for the initial values in `algorithm`.

  Can be `NA` if `algorithm` does not take initial values.

- `arg_lower`:

  \[`character(1)` \| `NA`\]  
  Optionally the argument name for the lower parameter bound in
  `algorithm`.

  Can be `NA` if not available.

- `arg_upper`:

  \[`character(1)` \| `NA`\]  
  Optionally the argument name for the upper parameter bound in
  `algorithm`.

  Can be `NA` if not available.

- `arg_gradient`:

  \[`character(1)` \| `NA`\]  
  Optionally the argument name for the gradient function in `algorithm`.

  Can be `NA` if not available.

- `arg_hessian`:

  \[`character(1)` \| `NA`\]  
  Optionally the argument name for the Hessian function in `algorithm`.

  Can be `NA` if not available.

- `gradient_as_attribute`:

  \[`logical(1)`\]  
  Only relevant if `arg_gradient` is not `NA`.

  In that case, does `algorithm` expect that the gradient is an
  attribute of the objective function output (as for example in
  [`nlm`](https://rdrr.io/r/stats/nlm.html))? In that case,
  `arg_gradient` defines the attribute name.

- `hessian_as_attribute`:

  \[`logical(1)`\]  
  Only relevant if `arg_hessian` is not `NA`.

  In that case, does `algorithm` expect that the Hessian is an attribute
  of the objective function output (as for example in
  [`nlm`](https://rdrr.io/r/stats/nlm.html))? In that case,
  `arg_hessian` defines the attribute name.

- `out_value`:

  \[[`character()`](https://rdrr.io/r/base/character.html) \|
  `function`\]  
  The element name for the optimal function value in the output of
  `algorithm`.

  Can be a vector of names if the output is nested (e.g.,
  `c("optim", "bestval")` for the element `bestval` of the element
  `optim`). If the output is an S4 object, the names refer to its slots.

  Alternatively, it can also be a `function` that receives the output of
  `algorithm` and returns the optimal function value.

- `out_parameter`:

  \[[`character()`](https://rdrr.io/r/base/character.html) \|
  `function`\]  
  The element name for the optimal parameters in the output of
  `algorithm`.

  Can be a vector of names if the output is nested (e.g.,
  `c("optim", "bestmem")` for the element `bestmem` of the element
  `optim`). If the output is an S4 object, the names refer to its slots.

  Alternatively, it can also be a `function` that receives the output of
  `algorithm` and returns the optimal parameters.

- `direction`:

  \[`character(1)`\]  
  Either `"min"` (if the optimizer minimizes) or `"max"` (if the
  optimizer maximizes).

#### Returns

Invisibly the `Optimizer` object.

#### Examples

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

------------------------------------------------------------------------

### `Optimizer$set_arguments()`

Sets optimizer arguments.

The arguments for the objective function and the initial values cannot
be set here, since they are specified when calling `$minimize()`,
`$maximize()`, or `$optimize()`.

#### Usage

    Optimizer$set_arguments(...)

#### Arguments

- `...`:

  \[`any`\]  
  In `Optimizer$new()` and `$set_arguments()`, named arguments to pass
  to the optimizer algorithm. In `$minimize()`, `$maximize()`, and
  `$optimize()`, named arguments to pass to the objective function.

#### Returns

Invisibly the `Optimizer` object.

------------------------------------------------------------------------

### `Optimizer$minimize()`

Perform minimization.

#### Usage

    Optimizer$minimize(objective, initial, lower = NA, upper = NA, ...)

#### Arguments

- `objective`:

  \[`function` \| `Objective`\]  
  A `function` to be optimized that

  1.  receives a `numeric` `vector` as its first argument (the so-called
      target argument)

  2.  and returns a single `numeric` value.

  Alternatively, it can also be an
  [`Objective`](https://loelschlaeger.de/optimizeR/reference/Objective.md)
  object for more flexibility (e.g., for another or more than one target
  argument, or for a gradient and Hessian function).

- `initial`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Starting parameter values for the optimization (without missing
  values).

  If the optimizer does not take initial values, they only define the
  number of parameters.

- `lower`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Lower bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

  Ignored (with a warning) for optimizers that do not support lower
  bounds.

- `upper`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Upper bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

  Ignored (with a warning) for optimizers that do not support upper
  bounds.

- `...`:

  \[`any`\]  
  In `Optimizer$new()` and `$set_arguments()`, named arguments to pass
  to the optimizer algorithm. In `$minimize()`, `$maximize()`, and
  `$optimize()`, named arguments to pass to the objective function.

#### Returns

A named `list`, containing at least these five elements:

- `value`:

  A `numeric`, the minimum function value.

- `parameter`:

  A `numeric` vector, the parameter vector where the minimum is
  obtained.

- `seconds`:

  A `numeric`, the optimization time in seconds.

- `initial`:

  A `numeric`, the initial parameter values.

- `error`:

  A `logical` flag: `TRUE` if an error occurred, otherwise `FALSE`.

Additional output elements from the optimizer are appended. If the
optimizer output is not a named `list`, it is appended as element
`output`.

If an error occurred, the error message is also appended as element
`error_message`.

If the time limit was exceeded, this counts as an error. In addition,
the flag `time_out = TRUE` is appended.

#### Examples

    Optimizer$new("stats::nlm")$
      minimize(objective = function(x) x^4 + 3*x - 5, initial = 2)

------------------------------------------------------------------------

### `Optimizer$maximize()`

Perform maximization.

#### Usage

    Optimizer$maximize(objective, initial, lower = NA, upper = NA, ...)

#### Arguments

- `objective`:

  \[`function` \| `Objective`\]  
  A `function` to be optimized that

  1.  receives a `numeric` `vector` as its first argument (the so-called
      target argument)

  2.  and returns a single `numeric` value.

  Alternatively, it can also be an
  [`Objective`](https://loelschlaeger.de/optimizeR/reference/Objective.md)
  object for more flexibility (e.g., for another or more than one target
  argument, or for a gradient and Hessian function).

- `initial`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Starting parameter values for the optimization (without missing
  values).

  If the optimizer does not take initial values, they only define the
  number of parameters.

- `lower`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Lower bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

  Ignored (with a warning) for optimizers that do not support lower
  bounds.

- `upper`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Upper bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

  Ignored (with a warning) for optimizers that do not support upper
  bounds.

- `...`:

  \[`any`\]  
  In `Optimizer$new()` and `$set_arguments()`, named arguments to pass
  to the optimizer algorithm. In `$minimize()`, `$maximize()`, and
  `$optimize()`, named arguments to pass to the objective function.

#### Returns

A named `list`, containing at least these five elements:

- `value`:

  A `numeric`, the maximum function value.

- `parameter`:

  A `numeric` vector, the parameter vector where the maximum is
  obtained.

- `seconds`:

  A `numeric`, the optimization time in seconds.

- `initial`:

  A `numeric`, the initial parameter values.

- `error`:

  A `logical` flag: `TRUE` if an error occurred, otherwise `FALSE`.

Additional output elements from the optimizer are appended. If the
optimizer output is not a named `list`, it is appended as element
`output`.

If an error occurred, the error message is also appended as element
`error_message`.

If the time limit was exceeded, this also counts as an error. In
addition, the flag `time_out = TRUE` is appended.

#### Examples

    Optimizer$new("stats::nlm")$
      maximize(objective = function(x) -x^4 + 3*x - 5, initial = 2)

------------------------------------------------------------------------

### `Optimizer$optimize()`

Perform minimization or maximization.

#### Usage

    Optimizer$optimize(
      objective,
      initial,
      lower = NA,
      upper = NA,
      direction = "min",
      ...
    )

#### Arguments

- `objective`:

  \[`function` \| `Objective`\]  
  A `function` to be optimized that

  1.  receives a `numeric` `vector` as its first argument (the so-called
      target argument)

  2.  and returns a single `numeric` value.

  Alternatively, it can also be an
  [`Objective`](https://loelschlaeger.de/optimizeR/reference/Objective.md)
  object for more flexibility (e.g., for another or more than one target
  argument, or for a gradient and Hessian function).

- `initial`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Starting parameter values for the optimization (without missing
  values).

  If the optimizer does not take initial values, they only define the
  number of parameters.

- `lower`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Lower bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

  Ignored (with a warning) for optimizers that do not support lower
  bounds.

- `upper`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Upper bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

  Ignored (with a warning) for optimizers that do not support upper
  bounds.

- `direction`:

  \[`character(1)`\]  
  Either `"min"` for minimization or `"max"` for maximization.

- `...`:

  \[`any`\]  
  In `Optimizer$new()` and `$set_arguments()`, named arguments to pass
  to the optimizer algorithm. In `$minimize()`, `$maximize()`, and
  `$optimize()`, named arguments to pass to the objective function.

#### Returns

A named `list`, containing at least these five elements:

- `value`:

  A `numeric`, the optimized function value.

- `parameter`:

  A `numeric` vector, the parameter vector where the optimum is
  obtained.

- `seconds`:

  A `numeric`, the optimization time in seconds.

- `initial`:

  A `numeric`, the initial parameter values.

- `error`:

  A `logical` flag: `TRUE` if an error occurred, otherwise `FALSE`.

Additional output elements from the optimizer are appended. If the
optimizer output is not a named `list`, it is appended as element
`output`.

If an error occurred, the error message is also appended as element
`error_message`.

If the time limit was exceeded, this also counts as an error. In
addition, the flag `time_out = TRUE` is appended.

#### Examples

    objective <- function(x) sin(x)
    nlm <- Optimizer$new("stats::nlm")
    nlm$optimize(objective = objective, initial = 2, direction = "min")
    nlm$optimize(objective = objective, initial = 2, direction = "max")

------------------------------------------------------------------------

### `Optimizer$print()`

Prints the optimizer label.

#### Usage

    Optimizer$print(...)

#### Arguments

- `...`:

  \[`any`\]  
  In `Optimizer$new()` and `$set_arguments()`, named arguments to pass
  to the optimizer algorithm. In `$minimize()`, `$maximize()`, and
  `$optimize()`, named arguments to pass to the objective function.

#### Returns

Invisibly the `Optimizer` object.

------------------------------------------------------------------------

### `Optimizer$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Optimizer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

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
#> [1] 0.03781247
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
#> [1] 0.02239609
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
#> [1] 0.002808571
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
#> [1] 0.003777027
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
#> [1] 0.004621506
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
#> [1] 0.003465652
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
#> [1] 0.001915693
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
