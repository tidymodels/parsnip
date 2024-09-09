# misspecified formula argument

    Code
      fit(linear_reg(), rec, mtcars)
    Condition
      Error in `fit()`:
      ! The `formula` argument must be a formula.
      i To fit a model with a recipe preprocessor, please use a workflow (`?workflows::workflow()`).

---

    Code
      fit(linear_reg(), "boop", mtcars)
    Condition
      Error in `fit()`:
      ! `formula` must be a formula, not the string "boop".

# No loaded engines

    ! parsnip could not locate an implementation for `cubist_rules` model specifications.
    i The parsnip extension package rules implements support for this specification.
    i Please install (if needed) and load to continue.
    

---

    ! parsnip could not locate an implementation for `poisson_reg` model specifications.
    i The parsnip extension packages multilevelmod, poissonreg, and agua implement support for this specification.
    i Please install (if needed) and load to continue.
    

---

    ! parsnip could not locate an implementation for `cubist_rules` model specifications using the `Cubist` engine.
    i The parsnip extension package rules implements support for this specification.
    i Please install (if needed) and load to continue.
    

