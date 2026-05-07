# Specify numerical optimizer object

The `Optimizer` object defines a numerical optimizer based on any
optimization algorithm implemented in R. The main advantage of working
with an `Optimizer` object instead of using the optimization function
directly lies in the standardized inputs and outputs.

Any R function that fulfills the following four constraints can be
defined as an `Optimizer` object:

1.  It must have an input for a `function`, the objective function to be
    optimized.

2.  It must have an input for a `numeric` vector, the initial values
    where the optimizer starts.

3.  It must have a `...` argument for additional parameters passed on to
    the objective function.

4.  The output must be a named `list`, including the optimal function
    value and the optimal parameter vector.

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

  \[`character(1)`\]  
  The argument name for the initial values in `algorithm`.

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

  \[`character(1)`\]  
  The element name for the optimal function value in the output `list`
  of `algorithm`.

- `out_parameter`:

  \[`character(1)`\]  
  The element name for the optimal parameters in the output `list` of
  `algorithm`.

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

- `hide_warnings`:

  \[`logical(1)`\]  
  Hide warnings during optimization?

- `output_ignore`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Elements to ignore (not include) in the optimization output.

## Methods

### Public methods

- [`Optimizer$new()`](#method-Optimizer-new)

- [`Optimizer$definition()`](#method-Optimizer-definition)

- [`Optimizer$set_arguments()`](#method-Optimizer-set_arguments)

- [`Optimizer$minimize()`](#method-Optimizer-minimize)

- [`Optimizer$maximize()`](#method-Optimizer-maximize)

- [`Optimizer$optimize()`](#method-Optimizer-optimize)

- [`Optimizer$print()`](#method-Optimizer-print)

- [`Optimizer$clone()`](#method-Optimizer-clone)

------------------------------------------------------------------------

### Method `new()`

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

### Method `definition()`

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

  \[`character(1)`\]  
  The argument name for the initial values in `algorithm`.

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

  \[`character(1)`\]  
  The element name for the optimal function value in the output `list`
  of `algorithm`.

- `out_parameter`:

  \[`character(1)`\]  
  The element name for the optimal parameters in the output `list` of
  `algorithm`.

- `direction`:

  \[`character(1)`\]  
  Either `"min"` (if the optimizer minimizes) or `"max"` (if the
  optimizer maximizes).

------------------------------------------------------------------------

### Method `set_arguments()`

Sets optimizer arguments.

#### Usage

    Optimizer$set_arguments(...)

#### Arguments

- `...`:

  \[`any`\]  
  In `Optimizer$new()` and `$set_arguments()`, named arguments to pass
  to the optimizer algorithm. In `$minimize()`, `$maximize()`, and
  `$optimize()`, named arguments to pass to the objective function.

#### Returns

The `Optimizer` object.

------------------------------------------------------------------------

### Method `minimize()`

Perform minimization.

#### Usage

    Optimizer$minimize(objective, initial, lower = NA, upper = NA, ...)

#### Arguments

- `objective`:

  \[`function` \| `Objective`\]  
  A `function` to be optimized that

  1.  has at least one argument that receives a `numeric` `vector`

  2.  and returns a single `numeric` value.

  Alternatively, it can also be an
  [`Objective`](https://loelschlaeger.de/optimizeR/reference/Objective.md)
  object for more flexibility.

- `initial`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Starting parameter values for the optimization.

- `lower`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Lower bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

- `upper`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Upper bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

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

Additional output elements from the optimizer are appended.

If an error occurred, the error message is also appended as element
`error_message`.

If the time limit was exceeded, this counts as an error. In addition,
the flag `time_out = TRUE` is appended.

#### Examples

    Optimizer$new("stats::nlm")$
      minimize(objective = function(x) x^4 + 3*x - 5, initial = 2)

------------------------------------------------------------------------

### Method `maximize()`

Perform maximization.

#### Usage

    Optimizer$maximize(objective, initial, lower = NA, upper = NA, ...)

#### Arguments

- `objective`:

  \[`function` \| `Objective`\]  
  A `function` to be optimized that

  1.  has at least one argument that receives a `numeric` `vector`

  2.  and returns a single `numeric` value.

  Alternatively, it can also be an
  [`Objective`](https://loelschlaeger.de/optimizeR/reference/Objective.md)
  object for more flexibility.

- `initial`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Starting parameter values for the optimization.

- `lower`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Lower bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

- `upper`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Upper bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

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

Additional output elements from the optimizer are appended.

If an error occurred, the error message is also appended as element
`error_message`.

If the time limit was exceeded, this also counts as an error. In
addition, the flag `time_out = TRUE` is appended.

#### Examples

    Optimizer$new("stats::nlm")$
      maximize(objective = function(x) -x^4 + 3*x - 5, initial = 2)

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

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

  1.  has at least one argument that receives a `numeric` `vector`

  2.  and returns a single `numeric` value.

  Alternatively, it can also be an
  [`Objective`](https://loelschlaeger.de/optimizeR/reference/Objective.md)
  object for more flexibility.

- `initial`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Starting parameter values for the optimization.

- `lower`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Lower bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

- `upper`:

  \[`NA` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  `numeric(1)`\]  
  Upper bounds on the parameters.

  If a single number, this will be applied to all parameters.

  Can be `NA` to not define any bounds.

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

Additional output elements from the optimizer are appended.

If an error occurred, the error message is also appended as element
`error_message`.

If the time limit was exceeded, this also counts as an error. In
addition, the flag `time_out = TRUE` is appended.

#### Examples

    objective <- function(x) -x^4 + 3*x - 5
    optimizer <- Optimizer$new("stats::nlm")
    optimizer$optimize(
      objective = objective, initial = 2, direction = "min"
    )
    optimizer$optimize(
      objective = objective, initial = 2, direction = "max"
    )

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Optimizer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
### Task: compare minimization with 'stats::nlm' and 'pracma::nelder_mead'

# 1. define objective function and initial values
objective <- TestFunctions::TF_ackley
initial <- c(3, 3)

# 2. get overview of optimizers available in dictionary
optimizer_dictionary$keys
#> [1] "lbfgsb3c::lbfgsb3c" "lbfgsb3c::lbfgsb3"  "lbfgsb3c::lbfgsb3f"
#> [4] "lbfgsb3c::lbfgsb3x" "stats::nlm"         "stats::nlminb"     
#> [7] "stats::optim"       "ucminf::ucminf"    

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
#> [1] 0.03229809
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
#> [1] 0.02242208
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
## Method `Optimizer$minimize`
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
#> [1] 0.003331423
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
## Method `Optimizer$maximize`
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
#> [1] 0.004034758
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
## Method `Optimizer$optimize`
## ------------------------------------------------

objective <- function(x) -x^4 + 3*x - 5
optimizer <- Optimizer$new("stats::nlm")
optimizer$optimize(
  objective = objective, initial = 2, direction = "min"
)
#> $value
#> [1] -1.012458e+16
#> 
#> $parameter
#> [1] 10031
#> 
#> $seconds
#> [1] 0.002794027
#> 
#> $initial
#> [1] 2
#> 
#> $error
#> [1] FALSE
#> 
#> $gradient
#> [1] -4.037322e+12
#> 
#> $code
#> [1] 5
#> 
#> $iterations
#> [1] 6
#> 
optimizer$optimize(
  objective = objective, initial = 2, direction = "max"
)
#> $value
#> [1] -2.955739
#> 
#> $parameter
#> [1] 0.9085598
#> 
#> $seconds
#> [1] 0.004099846
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
```
