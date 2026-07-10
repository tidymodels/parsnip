# tabular_saint() accepts its modes

    Code
      tabular_saint(mode = "quantile regression")
    Condition
      Error in `tabular_saint()`:
      ! "quantile regression" is not a known mode for model `tabular_saint()`.

# check_args.tabular_saint() validates input values

    Code
      check_args(tabular_saint(mode = "regression", attention_type = "diagonal"))
    Condition
      Error:
      ! `attention_type` must be one of "column", "row", or "both", not "diagonal".

---

    Code
      check_args(tabular_saint(mode = "regression", mixture = -0.5))
    Condition
      Error:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number -0.5.

---

    Code
      check_args(tabular_saint(mode = "regression", dropout_attn = 1.5))
    Condition
      Error:
      ! `dropout_attn` must be a number between 0 and 1 or `NULL`, not the number 1.5.

---

    Code
      check_args(tabular_saint(mode = "regression", dropout_hidden = 2))
    Condition
      Error:
      ! `dropout_hidden` must be a number between 0 and 1 or `NULL`, not the number 2.

---

    Code
      check_args(tabular_saint(mode = "regression", dropout_last = -0.1))
    Condition
      Error:
      ! `dropout_last` must be a number between 0 and 1 or `NULL`, not the number -0.1.

---

    Code
      check_args(tabular_saint(mode = "regression", penalty = -1))
    Condition
      Error:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      check_args(tabular_saint(mode = "regression", epochs = -1L))
    Condition
      Error:
      ! `epochs` must be a whole number larger than or equal to 1 or `NULL`, not the number -1.

---

    Code
      check_args(tabular_saint(mode = "regression", num_attn_heads = 0L))
    Condition
      Error:
      ! `num_attn_heads` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

