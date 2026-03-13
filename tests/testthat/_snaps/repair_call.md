# repair_call errors for non-model_fit objects

    Code
      repair_call(list(a = 1), mtcars)
    Condition
      Error in `repair_call()`:
      ! `x` should be a fitted parsnip model, not a list.

---

    Code
      repair_call(mtcars, mtcars)
    Condition
      Error in `repair_call()`:
      ! `x` should be a fitted parsnip model, not a data frame.

