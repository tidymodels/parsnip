# test mode setting for quantile regression

    Code
      linear_reg() %>% set_engine("quantreg") %>% set_mode("regression")
    Condition
      Error in `set_mode()`:
      ! Available modes for engine quantreg are: "unknown" and "quantile regression".

---

    Code
      linear_reg() %>% set_engine("quantreg") %>% set_mode("quantile regression")
    Condition
      Error in `set_mode()`:
      ! `quantile_levels` cannot be NULL.

---

    Code
      linear_reg() %>% set_engine("quantreg") %>% set_mode("quantile regression",
        quantile_level = 2)
    Condition
      Error in `set_mode()`:
      ! `quantile_levels` must be a number between 0 and 1, not the number 2.

---

    Code
      linear_reg() %>% set_engine("quantreg") %>% set_mode("quantile regression",
        quantile_level = 1:2)
    Condition
      Error in `set_mode()`:
      ! `quantile_levels` must be a number between 0 and 1, not the number 2.

---

    Code
      linear_reg() %>% set_engine("quantreg") %>% set_mode("quantile regression",
        quantile_level = NA_real_)
    Condition
      Error in `set_mode()`:
      ! `quantile_levels` must be a number, not a numeric `NA`.

