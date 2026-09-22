# protected arguments in `opts` are dropped with a warning

    Code
      res <- predict_raw(lm_fit, mtcars, opts = list(newdata = mtcars))
    Condition
      Warning:
      The argument `newdata` in `opts` is protected and will be ignored.

---

    Code
      res <- predict_raw(lm_fit, mtcars, opts = list(newdata = mtcars, object = 1))
    Condition
      Warning:
      The arguments `newdata` and `object` in `opts` are protected and will be ignored.

---

    Code
      res <- predict(lm_fit, mtcars, type = "raw", opts = list(newdata = mtcars))
    Condition
      Warning:
      The argument `newdata` in `opts` is protected and will be ignored.

