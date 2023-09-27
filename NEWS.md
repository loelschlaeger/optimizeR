# version 0.3.3

* Removed package start-up message.

* Added the `.direction` argument to `define_optimizer()` to specify whether the optimizer minimizes or maximizes.

* Added a dot before the argument names for `define_optimizer()` to avoid clashes with the `...` argument.

* Removed dependency on `{glue}`.

* Using `TestFunctions::TF_ackley` instead of own implementation of Ackley function (for the validation).

# version 0.3.2

* Just added package logo and created package website (https://loelschlaeger.de/optimizeR) with the [{pkgdown}](https://pkgdown.r-lib.org/) package. No code changes.

# version 0.3.1

* In `define_optimizer()`, now `validate = FALSE` by default.

# version 0.3.0

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

# version 0.2.0

* Some functions were renamed:

  * `optimizeR()` -> `apply_optimizer()`
  * `set_optimizer_nlm()` -> `optimizer_nlm()`
  * `set_optimizer_optim()` -> `optimizer_optim()`

* Functions `is_number()`, `try_silent()`, `timed()`, and `do.call_timed()` are no longer exported.

* Function `try_silent_timed()` was removed.

# version 0.1.0

* Initial CRAN release.