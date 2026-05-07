# Specify objective function object

The `Objective` object specifies the framework for an objective function
for numerical optimization.

## Value

An `Objective` object.

## Active bindings

- `objective_name`:

  \[`character(1)`\]  
  The label for the objective function.

- `fixed_arguments`:

  \[[`character()`](https://rdrr.io/r/base/character.html),
  read-only\]  
  The name(s) of the fixed argument(s) (if any).

- `seconds`:

  \[`numeric(1)`\]  
  A time limit in seconds. Computations are interrupted prematurely if
  `seconds` is exceeded.

  No time limit if `seconds = Inf` (the default).

  Note the limitations documented in
  [`setTimeLimit`](https://rdrr.io/r/base/setTimeLimit.html).

- `hide_warnings`:

  \[`logical(1)`\]  
  Hide warnings when evaluating the objective function?

- `verbose`:

  \[`logical(1)`\]  
  Print status messages?

- `npar`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html), read-only\]  
  The length of each target argument.

- `target`:

  \[[`character()`](https://rdrr.io/r/base/character.html),
  read-only\]  
  The argument name(s) to optimize.

- `gradient_specified`:

  \[`logical(1)`, read-only\]  
  Whether a gradient function has been specified via `$set_gradient()`.

- `hessian_specified`:

  \[`logical(1)`, read-only\]  
  Whether a Hessian function has been specified via `$set_hessian()`.

## Methods

### Public methods

- [`Objective$new()`](#method-Objective-new)

- [`Objective$set_argument()`](#method-Objective-set_argument)

- [`Objective$get_argument()`](#method-Objective-get_argument)

- [`Objective$remove_argument()`](#method-Objective-remove_argument)

- [`Objective$set_gradient()`](#method-Objective-set_gradient)

- [`Objective$set_hessian()`](#method-Objective-set_hessian)

- [`Objective$evaluate()`](#method-Objective-evaluate)

- [`Objective$evaluate_gradient()`](#method-Objective-evaluate_gradient)

- [`Objective$evaluate_gradient_numeric()`](#method-Objective-evaluate_gradient_numeric)

- [`Objective$evaluate_hessian()`](#method-Objective-evaluate_hessian)

- [`Objective$evaluate_hessian_numeric()`](#method-Objective-evaluate_hessian_numeric)

- [`Objective$print()`](#method-Objective-print)

- [`Objective$clone()`](#method-Objective-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new `Objective` object.

#### Usage

    Objective$new(f, target = NULL, npar, ...)

#### Arguments

- `f`:

  \[`function`\]  
  A `function` to be optimized.

  It is expected that `f` has at least one `numeric` argument.

  Also, the return value of `f` is expected to have the structure
  `numeric(1)`, i.e. a single `numeric` value.

- `target`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  The argument name(s) to optimize.

  All target arguments must be `numeric`.

  If `NULL` (default), the first function argument is selected.

- `npar`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The length of each target argument, i.e., the length(s) of the
  `numeric` `vector` argument(s) specified by `target`.

- `...`:

  Optionally additional function arguments that are fixed during the
  optimization.

------------------------------------------------------------------------

### Method `set_argument()`

Set a function argument that remains fixed during optimization.

#### Usage

    Objective$set_argument(..., .overwrite = TRUE, .verbose = self$verbose)

#### Arguments

- `...`:

  Optionally additional function arguments that are fixed during the
  optimization.

- `.overwrite`:

  \[`logical(1)`\]  
  Overwrite existing values?

- `.verbose`:

  \[`logical(1)`\]  
  Print status messages?

------------------------------------------------------------------------

### Method `get_argument()`

Get a fixed function argument.

#### Usage

    Objective$get_argument(argument_name, .verbose = self$verbose)

#### Arguments

- `argument_name`:

  \[`character(1)`\]  
  A function argument name.

- `.verbose`:

  \[`logical(1)`\]  
  Print status messages?

------------------------------------------------------------------------

### Method `remove_argument()`

Remove a fixed function argument.

#### Usage

    Objective$remove_argument(argument_name, .verbose = self$verbose)

#### Arguments

- `argument_name`:

  \[`character(1)`\]  
  A function argument name.

- `.verbose`:

  \[`logical(1)`\]  
  Print status messages?

------------------------------------------------------------------------

### Method `set_gradient()`

Set a gradient function.

#### Usage

    Objective$set_gradient(
      gradient,
      target = self$target,
      npar = self$npar,
      ...,
      .verbose = self$verbose
    )

#### Arguments

- `gradient`:

  \[`function`\]  
  A `function` that computes the gradient of the objective function `f`.

  It is expected that `gradient` has the same call as `f`, and that
  `gradient` returns a `numeric` `vector` of length `sum(self$npar)`.

- `target`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  The argument name(s) to optimize.

  All target arguments must be `numeric`.

  If `NULL` (default), the first function argument is selected.

- `npar`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The length of each target argument, i.e., the length(s) of the
  `numeric` `vector` argument(s) specified by `target`.

- `...`:

  Optionally additional function arguments that are fixed during the
  optimization.

- `.verbose`:

  \[`logical(1)`\]  
  Print status messages?

------------------------------------------------------------------------

### Method `set_hessian()`

Set a Hessian function.

#### Usage

    Objective$set_hessian(
      hessian,
      target = self$target,
      npar = self$npar,
      ...,
      .verbose = self$verbose
    )

#### Arguments

- `hessian`:

  \[`function`\]  
  A `function` that computes the Hessian of the objective function `f`.

  It is expected that `hessian` has the same call as `f`, and that
  `hessian` returns a `numeric` `matrix` of dimension `sum(self$npar)`
  times `sum(self$npar)`.

- `target`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  The argument name(s) to optimize.

  All target arguments must be `numeric`.

  If `NULL` (default), the first function argument is selected.

- `npar`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The length of each target argument, i.e., the length(s) of the
  `numeric` `vector` argument(s) specified by `target`.

- `...`:

  Optionally additional function arguments that are fixed during the
  optimization.

- `.verbose`:

  \[`logical(1)`\]  
  Print status messages?

------------------------------------------------------------------------

### Method `evaluate()`

Evaluate the objective function.

#### Usage

    Objective$evaluate(
      .at,
      .negate = FALSE,
      .gradient_as_attribute = FALSE,
      .gradient_attribute_name = "gradient",
      .gradient_numeric = FALSE,
      .hessian_as_attribute = FALSE,
      .hessian_attribute_name = "hessian",
      .hessian_numeric = FALSE,
      ...
    )

#### Arguments

- `.at`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The values for the target argument(s), written in a single vector.

  Must be of length `sum(self$npar)`.

- `.negate`:

  \[`logical(1)`\]  
  Negate the function return value?

- `.gradient_as_attribute`:

  \[`logical(1)`\]  
  Add the value of the gradient function as an attribute to the output?

  The attribute name is defined via the `.gradient_attribute_name`
  argument.

  Ignored if `$gradient_specified` and `.gradient_numeric` are `FALSE`.

- `.gradient_attribute_name`:

  \[`character(1)`\]  
  Only relevant if `.gradient_as_attribute = TRUE`.

  In that case, the attribute name for the gradient (if available).

- `.gradient_numeric`:

  \[`logical(1)`\]  
  Calculate the gradient via the numerical approximation
  [`grad`](https://rdrr.io/pkg/numDeriv/man/grad.html)?

- `.hessian_as_attribute`:

  \[`logical(1)`\]  
  Add the value of the Hessian function as an attribute to the output?

  The attribute name is defined via the `.hessian_attribute_name`
  argument.

  Ignored if `$hessian_specified` and `hessian_numeric` are `FALSE`.

- `.hessian_attribute_name`:

  \[`character(1)`\]  
  Only relevant if `.hessian_as_attribute = TRUE`.

  In that case, the attribute name for the Hessian (if available).

- `.hessian_numeric`:

  \[`logical(1)`\]  
  Calculate the Hessian via the numerical approximation
  [`hessian`](https://rdrr.io/pkg/numDeriv/man/hessian.html)?

- `...`:

  Optionally additional function arguments that are fixed during the
  optimization.

------------------------------------------------------------------------

### Method `evaluate_gradient()`

Evaluate the gradient function.

Returns a `numeric` vector of `NA_real_` values if the evaluation fails
or returns an unexpected result.

#### Usage

    Objective$evaluate_gradient(.at, .negate = FALSE, ...)

#### Arguments

- `.at`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The values for the target argument(s), written in a single vector.

  Must be of length `sum(self$npar)`.

- `.negate`:

  \[`logical(1)`\]  
  Negate the function return value?

- `...`:

  Optionally additional function arguments that are fixed during the
  optimization.

------------------------------------------------------------------------

### Method `evaluate_gradient_numeric()`

Evaluate the numerical gradient
[`grad`](https://rdrr.io/pkg/numDeriv/man/grad.html).

Returns a `numeric` vector of `NA_real_` values if the calculation fails
or returns an unexpected result.

#### Usage

    Objective$evaluate_gradient_numeric(.at, .negate = FALSE, ...)

#### Arguments

- `.at`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The values for the target argument(s), written in a single vector.

  Must be of length `sum(self$npar)`.

- `.negate`:

  \[`logical(1)`\]  
  Negate the function return value?

- `...`:

  Optionally additional function arguments that are fixed during the
  optimization.

------------------------------------------------------------------------

### Method `evaluate_hessian()`

Evaluate the Hessian function.

Returns an `NA_real_` value or matrix if the evaluation fails or returns
an unexpected result.

#### Usage

    Objective$evaluate_hessian(.at, .negate = FALSE, ...)

#### Arguments

- `.at`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The values for the target argument(s), written in a single vector.

  Must be of length `sum(self$npar)`.

- `.negate`:

  \[`logical(1)`\]  
  Negate the function return value?

- `...`:

  Optionally additional function arguments that are fixed during the
  optimization.

------------------------------------------------------------------------

### Method `evaluate_hessian_numeric()`

Evaluate the numerical Hessian
[`hessian`](https://rdrr.io/pkg/numDeriv/man/hessian.html).

Returns an `NA_real_` matrix if the calculation fails or returns an
unexpected result.

#### Usage

    Objective$evaluate_hessian_numeric(.at, .negate = FALSE, ...)

#### Arguments

- `.at`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The values for the target argument(s), written in a single vector.

  Must be of length `sum(self$npar)`.

- `.negate`:

  \[`logical(1)`\]  
  Negate the function return value?

- `...`:

  Optionally additional function arguments that are fixed during the
  optimization.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print details of the `Objective` object.

#### Usage

    Objective$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Objective$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
### define log-likelihood function of Gaussian mixture model
llk <- function(mu, sd, lambda, data) {
  sd <- exp(sd)
  lambda <- plogis(lambda)
  cluster_1 <- lambda * dnorm(data, mu[1], sd[1])
  cluster_2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
  sum(log(cluster_1 + cluster_2))
}

### optimize over the first three arguments, the 'data' argument is constant
objective <- Objective$new(
  f = llk, target = c("mu", "sd", "lambda"), npar = c(2, 2, 1),
  data = faithful$eruptions
)

### evaluate at 1:5 (1:2 is passed to mu, 3:4 to sd, and 5 to lambda)
objective$evaluate(1:5)
#> [1] -1069.623
```
