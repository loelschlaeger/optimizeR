# Dictionary of optimizer functions

A dictionary of currently included numerical optimizer functions in the
`{optimizeR}` package.

## Usage

``` r
optimizer_dictionary
```

## Format

An `R6` object of class
[`Dictionary`](http://loelschlaeger.de/oeli/reference/Dictionary.md).

## Examples

``` r
print(optimizer_dictionary)
#> <Dictionary> optimization algorithms 
#> Keys: 
#> - lbfgsb3c::lbfgsb3c
#> - lbfgsb3c::lbfgsb3
#> - lbfgsb3c::lbfgsb3f
#> - lbfgsb3c::lbfgsb3x
#> - stats::nlm
#> - stats::nlminb
#> - stats::optim
#> - ucminf::ucminf
```
