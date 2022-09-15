# extract parameter set from model with no loaded implementation

    Code
      extract_parameter_set_dials(bt_mod)
    Condition
      Error:
      ! parsnip could not locate an implementation for `bag_tree` regression model specifications.
      i The parsnip extension package baguette implements support for this specification.
      i Please install (if needed) and load to continue.

---

    Code
      extract_parameter_dials(bt_mod, parameter = "min_n")
    Condition
      Error:
      ! parsnip could not locate an implementation for `bag_tree` regression model specifications.
      i The parsnip extension package baguette implements support for this specification.
      i Please install (if needed) and load to continue.

