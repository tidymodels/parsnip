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
      ! `attention_type` must be one of "column", "row", or "both".

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
      check_args(tabular_saint(mode = "regression", num_attn_heads = 0L))
    Condition
      Error:
      ! `num_attn_heads` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

# check_args.tabular_saint() rejects both penalty and dropout

    Code
      check_args(spec)
    Condition
      Error:
      ! Both weight decay and dropout should not be specified.

