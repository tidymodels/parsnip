# tabular_resnet() accepts its modes

    Code
      tabular_resnet(mode = "quantile regression")
    Condition
      Error in `tabular_resnet()`:
      ! "quantile regression" is not a known mode for model `tabular_resnet()`.

# check_args.tabular_resnet() validates input values

    Code
      check_args(tabular_resnet(mode = "regression", penalty = -1))
    Condition
      Error:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      check_args(tabular_resnet(mode = "regression", mixture = 2))
    Condition
      Error:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number 2.

---

    Code
      check_args(tabular_resnet(mode = "regression", dropout = 1.5))
    Condition
      Error:
      ! `dropout` must be a number between 0 and 1 or `NULL`, not the number 1.5.

---

    Code
      check_args(tabular_resnet(mode = "regression", stop_iter = 0L))
    Condition
      Error:
      ! `stop_iter` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

