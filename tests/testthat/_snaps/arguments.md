# warns informatively about protected arguments

    Code
      .res <- check_eng_args(args = list(a = 1, b = 2, c = 3, e = 5), obj = obj,
      core_args = core_args)
    Condition
      Warning:
      The arguments `a`, `b`, and `c` cannot be manually modified and were removed.

---

    Code
      .res <- check_eng_args(args = list(b = 2, c = 3, e = 5), obj = obj, core_args = core_args)
    Condition
      Warning:
      The arguments `b` and `c` cannot be manually modified and were removed.

---

    Code
      .res <- check_eng_args(args = list(c = 3, e = 5), obj = obj, core_args = core_args)
    Condition
      Warning:
      The argument `c` cannot be manually modified and was removed.

