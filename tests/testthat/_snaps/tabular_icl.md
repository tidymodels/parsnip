# tabular_icl() accepts its modes

    Code
      tabular_icl(mode = "censored regression")
    Condition
      Error in `tabular_icl()`:
      ! "censored regression" is not a known mode for model `tabular_icl()`.

# check_args.tabular_icl() validates input values

    Code
      check_args(tabular_icl(mode = "classification", softmax_temperature = -1))
    Condition
      Error:
      ! `softmax_temperature` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      check_args(tabular_icl(mode = "classification", num_estimators = 5.5))
    Condition
      Error:
      ! `num_estimators` must be a whole number or `NULL`, not the number 5.5.

