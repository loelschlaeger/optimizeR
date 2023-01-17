# optimizeR 0.2.0.9000

* Inputs for `set_optimizer()` were renamed for clarity:
  
  * `opt_fun` -> `optimizer`
  * `f` -> `objective`
  * `p` -> `initial`
  * `v` -> `value`
  * `z` -> `parameter`
  * `out_ign` -> `output_ignore`
  * `test_par` -> `validation_settings`
  
* Elements for input `validation_settings` have been simplified, please see
the documentation of `set_optimizer()`.

* Elements in the output of `apply_optimizer()` were renamed for clarity:

  * `v` -> `value` 
  * `z` -> `parameter`
  * `time` -> `seconds` (now just a `numeric`) 
  
* Element `initial` (the starting parameter vector) was added to the output
of `apply_optimizer()`.

# optimizeR 0.2.0

* Some functions were renamed:

  * `optimizeR()` -> `apply_optimizer()`
  * `set_optimizer_nlm()` -> `optimizer_nlm()`
  * `set_optimizer_optim()` -> `optimizer_optim()`

* Functions `is_number()`, `try_silent()`, `timed()`, and `do.call_timed()` are no longer exported.

* Function `try_silent_timed()` was removed.

# optimizeR 0.1.0

* Initial version.
