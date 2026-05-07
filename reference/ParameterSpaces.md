# Switch between parameter spaces

The `ParameterSpaces` object manages two related parameter spaces:

- the Optimization Space (for optimization)

- the Interpretation Space (for easier interpretation).

In the Optimization Space, parameters are stored as a `numeric`
`vector`, the standard format for numerical optimizers.

In the Interpretation Space, parameters are stored as a `list` and can
take different formats (e.g., `matrix`).

Users can define transformation functions (not necessarily bijective) to
switch between these spaces via the `$o2i()` and `$i2o()` methods.

## Methods

### Public methods

- [`ParameterSpaces$new()`](#method-ParameterSpaces-new)

- [`ParameterSpaces$print()`](#method-ParameterSpaces-print)

- [`ParameterSpaces$switch()`](#method-ParameterSpaces-switch)

- [`ParameterSpaces$o2i()`](#method-ParameterSpaces-o2i)

- [`ParameterSpaces$i2o()`](#method-ParameterSpaces-i2o)

- [`ParameterSpaces$clone()`](#method-ParameterSpaces-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a new `ParameterSpaces` object.

#### Usage

    ParameterSpaces$new(parameter_names, parameter_lengths_in_o_space)

#### Arguments

- `parameter_names`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Unique names for the parameters.

- `parameter_lengths_in_o_space`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The length of each parameter in the optimization space.

#### Returns

A new `ParameterSpaces` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print an overview of the parameter spaces.

#### Usage

    ParameterSpaces$print(show_transformer = FALSE)

#### Arguments

- `show_transformer`:

  \[`logical(1)`\]  
  Show transformer functions in the output?

------------------------------------------------------------------------

### Method [`switch()`](https://rdrr.io/r/base/switch.html)

Switch between Optimization Space and Interpretation Space.

#### Usage

    ParameterSpaces$switch(x, to = NULL)

#### Arguments

- `x`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  [`list()`](https://rdrr.io/r/base/list.html)\]  
  The parameters, either as a `numeric vector` (will be switched to
  Interpretation Space), or as a
  [`list()`](https://rdrr.io/r/base/list.html) (will be switched to
  Optimization Space).

- `to`:

  \[`character(1)` \| `NULL`\]  
  Explicitly switch to a specific space, either

  - `"o"`: Optimization Space

  - `"i"`: Interpretation Space

  If `NULL`, the function will switch to the other space.

------------------------------------------------------------------------

### Method `o2i()`

Define transformation functions when switching from Optimization Space
to Interpretation Space.

#### Usage

    ParameterSpaces$o2i(...)

#### Arguments

- `...`:

  \[`function`\]  
  One or more transformation functions, named according to the
  parameters.

  Transformers from Optimization Space to Interpretation Space (o2i)
  **must receive** a `numeric`. The default is the identity.

------------------------------------------------------------------------

### Method `i2o()`

Define transformation functions when switching from Interpretation Space
to Optimization Space.

#### Usage

    ParameterSpaces$i2o(...)

#### Arguments

- `...`:

  \[`function`\]  
  One or more transformation functions, named according to the
  parameters.

  Transformers from Interpretation Space to Optimization Space (i2o)
  **must return** a `numeric`. The default is
  [`as.vector()`](https://rdrr.io/r/base/vector.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ParameterSpaces$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
### Log-likelihood function of two-class Gaussian mixture model with
### parameter vector `theta` that consists of
### - `mu`, mean vector of length 2
### - `sd`, standard deviation vector of length 2, must be positive
### - `lambda`, class probability of length 1, must be between 0 and 1

normal_mixture_llk <- function(theta, data) {
  mu <- theta[1:2]
  sd <- exp(theta[3:4])
  lambda <- plogis(theta[5])
  c1 <- lambda * dnorm(data, mu[1], sd[1])
  c2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
  sum(log(c1 + c2))
}

### define parameter spaces
### - `mu` needs no transformation
### - `sd` needs to be real in optimization space and positive in
###    interpretation space
### - `lambda` needs to be real and of length `1` in optimization space, and
###    a probability vector of length `2` in interpretation space

normal_mixture_spaces <- ParameterSpaces$
  new(
    parameter_names = c("mu", "sd", "lambda"),
    parameter_lengths_in_o_space = c(2, 2, 1)
  )$
  o2i(
    "mu" = function(x) x,
    "sd" = function(x) exp(x),
    "lambda" = function(x) c(plogis(x), 1 - plogis(x))
  )$
  i2o(
    "mu" = function(x) x,
    "sd" = function(x) log(x),
    "lambda" = function(x) qlogis(x[1])
  )

### switch between parameter spaces

par <- list(              # parameters in interpretation space
  "mu" = c(2, 4),
  "sd" = c(0.5, 1),
  "lambda" = c(0.4, 0.6)
)
(x <- normal_mixture_spaces$switch(par)) # switch to optimization space
#> [1]  2.0000000  4.0000000 -0.6931472  0.0000000 -0.4054651
normal_mixture_llk(
  theta = x, data = datasets::faithful$eruptions
)
#> [1] -382.1072
normal_mixture_spaces$switch(x)          # switch back
#> $mu
#> [1] 2 4
#> 
#> $sd
#> [1] 0.5 1.0
#> 
#> $lambda
#> [1] 0.4 0.6
#> 
```
