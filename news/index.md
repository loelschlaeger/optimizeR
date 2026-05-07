# Changelog

## optimizeR 1.3.0

- Documentation improvements.

- Removed renv.

- Added a safeguard if computation of (numerical) gradient or Hessian
  fails. ([\#5](https://github.com/loelschlaeger/optimizeR/issues/5))

- Additional arguments passed to optimization methods now go only to the
  objective function.
  ([\#1](https://github.com/loelschlaeger/optimizeR/issues/1))

## optimizeR 1.2.1

CRAN release: 2025-06-26

- Small fixes.

- Added private helper function `.check_arguments_complete()` to the
  `Objective` object. It checks whether all required arguments are
  specified.

- Added functionality to the `Objective` object that can compute a
  numerical gradient and Hessian.

## optimizeR 1.2.0

CRAN release: 2025-04-24

- Gradient, Hessian, and parameter bounds can now be passed to
  `Optimizer` objects.

- The old S3 interface has been removed. The functions
  `define_optimizer()`, `optimizer_nlm()`, `optimizer_optim()`,
  `new_optimizer()`, `validate_optimizer()`, and `apply_optimizer()` are
  no longer available. Please use the R6 interface via `Optimizer$new()`
  and the corresponding methods. See
  [`help("Optimizer", "optimizeR")`](https://loelschlaeger.de/optimizeR/reference/Optimizer.md)
  for details.

## optimizeR 1.1.3

CRAN release: 2025-03-27

- `ParameterSpaces` now allows users to specify parameters that are not
  used for transformations. This can be useful when one general
  `ParameterSpaces` object is defined for nested sets of parameters.

## optimizeR 1.1.2

CRAN release: 2024-11-27

- Added the `ParameterSpaces` R6 object to transform parameters between
  optimization and interpretation space.

## optimizeR 1.1.1

CRAN release: 2024-06-19

- The `Objective` object now allows different output structures for the
  objective function. A template can be provided via the
  `$output_template` field.

## optimizeR 1.1.0

CRAN release: 2024-05-27

- The `Optimizer` object has a new method `optimize`, which can be used
  for minimization and maximization by setting the argument `direction`.

- In method `Objective$initialize()`, renamed argument `objective` to
  `f`.

- In method `Objective$initialize()`, if `target = NULL` (the new
  default), the first argument from `f` is taken as the target argument.

## optimizeR 1.0.5

CRAN release: 2024-02-28

- Removed `install_optimizer_packages()`.

## optimizeR 1.0.4

CRAN release: 2024-02-05

- Fixed a bug and added test cases for fixed arguments that are `NULL`.

## optimizeR 1.0.3

CRAN release: 2024-01-31

- Fixed a bug in creating the error message for a timeout.

- Added checks for the objective, initial, and ellipsis arguments of the
  optimizer function.

- Added a `verbose` argument to the `$get_argument()` and
  `$remove_argument()` methods of the `Objective` object.

- Added field `fixed_arguments` to the `Objective` object. It returns
  the names of the fixed arguments.

- Fixed a bug in backward compatibility.

## optimizeR 1.0.2

CRAN release: 2024-01-14

- Small bug fixes.

- Backward compatibility with
  [optimizeR](https://loelschlaeger.de/optimizeR/) version 0.3.3.

## optimizeR 1.0.1

CRAN release: 2023-11-08

- Fixed wrong values for `"out_value"` and `"out_parameter"` in the
  optimizer dictionary entry for
  [`stats::optim`](https://rdrr.io/r/stats/optim.html).

## optimizeR 1.0.0

CRAN release: 2023-11-04

- Instead of using S3 classes, we treat optimizers as R6 objects now.

## optimizeR 0.3.3

CRAN release: 2023-09-27

- Removed package start-up message.

- Added the `.direction` argument to `define_optimizer()` to specify
  whether the optimizer minimizes or maximizes.

- Added a dot before the argument names for `define_optimizer()` to
  avoid clashes with the `...` argument.

- Removed dependency on [glue](https://glue.tidyverse.org/).

- Used
  [`TestFunctions::TF_ackley`](https://rdrr.io/pkg/TestFunctions/man/TF_ackley.html)
  instead of the package’s own implementation of the Ackley function for
  validation.

## optimizeR 0.3.2

CRAN release: 2023-03-25

- Added the package logo and created the package website
  (<https://loelschlaeger.de/optimizeR>) with
  [{pkgdown}](https://pkgdown.r-lib.org/). No code changes.

## optimizeR 0.3.1

CRAN release: 2023-03-05

- In `define_optimizer()`, now `validate = FALSE` by default.

## optimizeR 0.3.0

CRAN release: 2023-01-22

- Function `set_optimizer()` was renamed to `define_optimizer()`.

- Inputs for `define_optimizer()` were renamed for clarity:

  - `opt_fun` -\> `optimizer`
  - `f` -\> `objective`
  - `p` -\> `initial`
  - `v` -\> `value`
  - `z` -\> `parameter`
  - `out_ign` -\> `output_ignore`
  - `test_par` -\> `validation_settings`

- Elements for input `validation_settings` have been simplified. Please
  see the documentation of `define_optimizer()`.

- Elements in the output of `apply_optimizer()` were renamed for
  clarity:

  - `v` -\> `value`
  - `z` -\> `parameter`
  - `time` -\> `seconds` (now just a `numeric`)

- Element `initial` (the starting parameter vector) was added to the
  output of `apply_optimizer()`.

## optimizeR 0.2.0

CRAN release: 2022-12-02

- Some functions were renamed:

  - [`optimizeR()`](https://loelschlaeger.de/optimizeR/reference/optimizeR-package.md)
    -\> `apply_optimizer()`
  - `set_optimizer_nlm()` -\> `optimizer_nlm()`
  - `set_optimizer_optim()` -\> `optimizer_optim()`

- Functions `is_number()`, `try_silent()`, `timed()`, and
  `do.call_timed()` are no longer exported.

- Function `try_silent_timed()` was removed.

## optimizeR 0.1.0

CRAN release: 2022-08-23

- Initial CRAN release.
