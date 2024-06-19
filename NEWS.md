# optimizerR 1.1.1

* The `Objective` object now allows for different types of output structures of the objective function, for which a template can be provided via the `$output_template` field. 

# optimizerR 1.1.0

* The `Optimizer` object has a new method `optimize` which can be used for minimization and maximization by setting the argument `direction`.

* In method `Objective$initialize()`, renamed argument `objective` -> `f`.

* In method `Objective$initialize()`, if `target = NULL` (the new default), the first argument from `f` is taken as target argument.

# optimizeR 1.0.5

* Removed `install_optimizer_packages()`.

# optimizeR 1.0.4

* Fixed bug and added test cases when having fixed arguments that are `NULL`.

# optimizeR 1.0.3

* Fixed bug in creating error message in case of a time out.

* Added checks for objective, initial, and ellipsis argument for the optimizer function.

* Added `verbose` argument to methods `$get_argument()` and `$remove_argument()` of `Objective` object.

* Added field `fixed_arguments` to `Objective` object which returns the names of the fixed arguments.

* Fixed bug in proper backwards compatibility.

# optimizeR 1.0.2

* Small bug fixes.

* Backward compatibility with `{optimizeR}` version 0.3.3.

# optimizeR 1.0.1

* Fixed wrong values for `"out_value"` and `"out_parameter"` in the optimizer dictionary entry for `stats::optim`.

# optimizeR 1.0.0

* Instead of using S3 classes, we treat optimizers as R6 objects now.

# optimizeR 0.3.3

* Removed package start-up message.

* Added the `.direction` argument to `define_optimizer()` to specify whether the optimizer minimizes or maximizes.

* Added a dot before the argument names for `define_optimizer()` to avoid clashes with the `...` argument.

* Removed dependency on `{glue}`.

* Using `TestFunctions::TF_ackley` instead of own implementation of Ackley function (for the validation).

# optimizeR 0.3.2

* Just added package logo and created package website (https://loelschlaeger.de/optimizeR) with the [{pkgdown}](https://pkgdown.r-lib.org/) package. No code changes.

# optimizeR 0.3.1

* In `define_optimizer()`, now `validate = FALSE` by default.

# optimizeR 0.3.0

* Function `set_optimizer()` was renamed to `define_optimizer()`.

* Inputs for `define_optimizer()` were renamed for clarity:
  
  * `opt_fun` -> `optimizer`
  * `f` -> `objective`
  * `p` -> `initial`
  * `v` -> `value`
  * `z` -> `parameter`
  * `out_ign` -> `output_ignore`
  * `test_par` -> `validation_settings`
  
* Elements for input `validation_settings` have been simplified, please see the documentation of `define_optimizer()`.

* Elements in the output of `apply_optimizer()` were renamed for clarity:

  * `v` -> `value` 
  * `z` -> `parameter`
  * `time` -> `seconds` (now just a `numeric`) 
  
* Element `initial` (the starting parameter vector) was added to the output of `apply_optimizer()`.

# optimizeR 0.2.0

* Some functions were renamed:

  * `optimizeR()` -> `apply_optimizer()`
  * `set_optimizer_nlm()` -> `optimizer_nlm()`
  * `set_optimizer_optim()` -> `optimizer_optim()`

* Functions `is_number()`, `try_silent()`, `timed()`, and `do.call_timed()` are no longer exported.

* Function `try_silent_timed()` was removed.

# optimizeR 0.1.0

* Initial CRAN release.
