# test various cli messages [plain]

    Code
      objective$set_argument(a = -2)
    Message
      > Setting argument "a".

---

    Code
      objective$set_argument(a = -3)
    Message
      > Overwriting argument "a".

---

    Code
      objective$get_argument("a")
    Message
      v Required argument "a" is specified.
      > Returning argument "a".
    Output
      [1] -3

---

    Code
      objective$remove_argument("a")
    Message
      v Required argument "a" is specified.
      > Removing argument "a".

---

    Code
      objective$set_argument(a = -2)
    Message
      > Setting argument "a".

---

    Code
      objective$set_gradient(f_gradient)
    Message
      > Setting gradient function.

---

    Code
      objective$set_hessian(f_hessian)
    Message
      > Setting Hessian function.

---

    Code
      objective$set_argument(a = 1)
    Message
      > Setting argument "a".
      > Synchronized arguments with gradient function.
      > Synchronized arguments with Hessian function.

# test various cli messages [ansi]

    Code
      objective$set_argument(a = -2)
    Message
      > Setting argument [34m[34m"a"[34m[39m.

---

    Code
      objective$set_argument(a = -3)
    Message
      > Overwriting argument [34m[34m"a"[34m[39m.

---

    Code
      objective$get_argument("a")
    Message
      [32mv[39m Required argument [34m[34m"a"[34m[39m is specified.
      > Returning argument [34m[34m"a"[34m[39m.
    Output
      [1] -3

---

    Code
      objective$remove_argument("a")
    Message
      [32mv[39m Required argument [34m[34m"a"[34m[39m is specified.
      > Removing argument [34m[34m"a"[34m[39m.

---

    Code
      objective$set_argument(a = -2)
    Message
      > Setting argument [34m[34m"a"[34m[39m.

---

    Code
      objective$set_gradient(f_gradient)
    Message
      > Setting gradient function.

---

    Code
      objective$set_hessian(f_hessian)
    Message
      > Setting Hessian function.

---

    Code
      objective$set_argument(a = 1)
    Message
      > Setting argument [34m[34m"a"[34m[39m.
      > Synchronized arguments with gradient function.
      > Synchronized arguments with Hessian function.

# test various cli messages [unicode]

    Code
      objective$set_argument(a = -2)
    Message
      → Setting argument "a".

---

    Code
      objective$set_argument(a = -3)
    Message
      → Overwriting argument "a".

---

    Code
      objective$get_argument("a")
    Message
      ✔ Required argument "a" is specified.
      → Returning argument "a".
    Output
      [1] -3

---

    Code
      objective$remove_argument("a")
    Message
      ✔ Required argument "a" is specified.
      → Removing argument "a".

---

    Code
      objective$set_argument(a = -2)
    Message
      → Setting argument "a".

---

    Code
      objective$set_gradient(f_gradient)
    Message
      → Setting gradient function.

---

    Code
      objective$set_hessian(f_hessian)
    Message
      → Setting Hessian function.

---

    Code
      objective$set_argument(a = 1)
    Message
      → Setting argument "a".
      → Synchronized arguments with gradient function.
      → Synchronized arguments with Hessian function.

# test various cli messages [fancy]

    Code
      objective$set_argument(a = -2)
    Message
      → Setting argument [34m[34m"a"[34m[39m.

---

    Code
      objective$set_argument(a = -3)
    Message
      → Overwriting argument [34m[34m"a"[34m[39m.

---

    Code
      objective$get_argument("a")
    Message
      [32m✔[39m Required argument [34m[34m"a"[34m[39m is specified.
      → Returning argument [34m[34m"a"[34m[39m.
    Output
      [1] -3

---

    Code
      objective$remove_argument("a")
    Message
      [32m✔[39m Required argument [34m[34m"a"[34m[39m is specified.
      → Removing argument [34m[34m"a"[34m[39m.

---

    Code
      objective$set_argument(a = -2)
    Message
      → Setting argument [34m[34m"a"[34m[39m.

---

    Code
      objective$set_gradient(f_gradient)
    Message
      → Setting gradient function.

---

    Code
      objective$set_hessian(f_hessian)
    Message
      → Setting Hessian function.

---

    Code
      objective$set_argument(a = 1)
    Message
      → Setting argument [34m[34m"a"[34m[39m.
      → Synchronized arguments with gradient function.
      → Synchronized arguments with Hessian function.

# objective with one target argument can be evaluated

    Code
      print(objective)
    Output
      * Function: f
      * Targets (length): x (1)
      * Fixed arguments specified: b, a

# objective with more than one target argument can be evaluated

    Code
      print(objective)
    Output
      * Function: llk
      * Targets (length): mu (2), sd (2), lambda (1)
      * Fixed arguments specified: data

# objective with NULL argument can be evaluated

    Code
      print(obj)
    Output
      * Function: f
      * Targets (length): x (3)
      * Fixed arguments specified: none

