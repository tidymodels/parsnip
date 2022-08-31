# set_* functions error when input isn't model_spec

    Code
      set_mode(mtcars, "regression")
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'set_mode' applied to an object of class "data.frame"

---

    Code
      set_args(mtcars, blah = "blah")
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'set_args' applied to an object of class "data.frame"

