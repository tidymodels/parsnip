# tabular_rln() is regression only

    Code
      tabular_rln(mode = "classification")
    Condition
      Error in `tabular_rln()`:
      ! "classification" is not a known mode for model `tabular_rln()`.

# check_args.tabular_rln() validates input values

    Code
      check_args(tabular_rln(penalty_type = "L3"))
    Condition
      Error:
      ! `penalty_type` must be one of "L1" or "L2", not "L3".
      i Did you mean "L1"?

---

    Code
      check_args(tabular_rln(penalty_average = -1))
    Condition
      Error:
      ! `penalty_average` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      check_args(tabular_rln(step_rate = -5))
    Condition
      Error:
      ! `step_rate` must be a number larger than or equal to 0 or `NULL`, not the number -5.

