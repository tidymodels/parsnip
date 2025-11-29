# extract

    Code
      extract_spec_parsnip(x_no_spec)
    Condition
      Error in `extract_spec_parsnip()`:
      ! The model fit does not have a model spec.
      i This is an internal error that was detected in the parsnip package.
        Please report it at <https://github.com/tidymodels/parsnip/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

---

    Code
      extract_fit_engine(x_no_fit)
    Condition
      Error in `extract_fit_engine()`:
      ! The model fit does not have an engine fit.
      i This is an internal error that was detected in the parsnip package.
        Please report it at <https://github.com/tidymodels/parsnip/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

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

# extract single parameter from model with no parameters

    Code
      extract_parameter_dials(lm_model, parameter = "none there")
    Condition
      Error in `extract_parameter_dials()`:
      ! No parameter exists with id "none there".

# extract_fit_time() works

    Code
      extract_fit_time(lm_fit)
    Condition
      Error in `extract_fit_time()`:
      ! This model was fit before `extract_fit_time()` was added.

