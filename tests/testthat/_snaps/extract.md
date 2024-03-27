# extract_fit_time() works

    Code
      extract_fit_time(lm_fit, summarize = FALSE)
    Condition
      Error in `extract_fit_time()`:
      ! `summarize = FALSE` is not supported for `model_fit` objects.

---

    Code
      extract_fit_time(lm_fit)
    Condition
      Error in `extract_fit_time()`:
      ! This model was fit before `extract_fit_time()` was added.

