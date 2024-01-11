# .filter_eval_time()

    Code
      parsnip:::.filter_eval_time(times_duplicated)
    Condition
      Warning:
      There were 11 inappropriate evaluation time points that were removed. They were:
      * 11 duplicate values.
    Output
       [1]  0  1  2  3  4  5  6  7  8  9 10

---

    Code
      parsnip:::.filter_eval_time(-1)
    Condition
      Error:
      ! There were no usable evaluation times (finite, non-missing, and >= 0).

---

    Code
      parsnip:::.filter_eval_time(times_remove_plural)
    Condition
      Warning:
      There were 3 inappropriate evaluation time points that were removed. They were:
      * 1 missing value.
      * 1 infinite value.
      * 1 negative value.
    Output
       [1]  0  1  2  3  4  5  6  7  8  9 10

---

    Code
      parsnip:::.filter_eval_time(times_remove_singular)
    Condition
      Warning:
      There was 1 inappropriate evaluation time point that was removed. It was:
      * 1 negative value.
    Output
       [1]  0  1  2  3  4  5  6  7  8  9 10

