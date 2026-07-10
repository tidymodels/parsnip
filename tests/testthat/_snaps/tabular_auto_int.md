# tabular_auto_int() accepts its modes

    Code
      tabular_auto_int(mode = "quantile regression")
    Condition
      Error in `tabular_auto_int()`:
      ! "quantile regression" is not a known mode for model `tabular_auto_int()`.

# check_args.tabular_auto_int() validates input values

    Code
      check_args(tabular_auto_int(mode = "regression", penalty = -1))
    Condition
      Error:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      check_args(tabular_auto_int(mode = "regression", mixture = 2))
    Condition
      Error:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number 2.

---

    Code
      check_args(tabular_auto_int(mode = "regression", dropout = 1.5))
    Condition
      Error:
      ! `dropout` must be a number between 0 and 1 or `NULL`, not the number 1.5.

---

    Code
      check_args(tabular_auto_int(mode = "regression", dropout_attn = 1.5))
    Condition
      Error:
      ! `dropout_attn` must be a number between 0 and 1 or `NULL`, not the number 1.5.

---

    Code
      check_args(tabular_auto_int(mode = "regression", dropout_embedding = -0.1))
    Condition
      Error:
      ! `dropout_embedding` must be a number between 0 and 1 or `NULL`, not the number -0.1.

---

    Code
      check_args(tabular_auto_int(mode = "regression", epochs = 2.5))
    Condition
      Error:
      ! `epochs` must be a whole number or `NULL`, not the number 2.5.

---

    Code
      check_args(tabular_auto_int(mode = "regression", num_attn_heads = 0L))
    Condition
      Error:
      ! `num_attn_heads` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      check_args(tabular_auto_int(mode = "regression", stop_iter = 0L))
    Condition
      Error:
      ! `stop_iter` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

