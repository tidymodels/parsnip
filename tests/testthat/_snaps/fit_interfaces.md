# wrong args

    Code
      tester_xy(NULL, x = sprk, y = hpc, model = rmod)
    Condition
      Error in `tester_xy()`:
      ! `x` should be a <data.frame/matrix>, not an <integer> object.

---

    Code
      tester(NULL, f, data = as.matrix(hpc[, 1:4]))
    Condition
      Error in `tester()`:
      ! `data` should be a <data.frame/dgCMatrix/tbl_spark>, not a double matrix.

# unknown modes

    Code
      fit(mars_spec, am ~ ., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please set the mode in the model specification (`?parsnip::model_spec()`).

---

    Code
      fit_xy(mars_spec, x = mtcars[, -1], y = mtcars[, 1])
    Condition
      Error in `fit_xy()`:
      ! Please set the mode in the model specification (`?parsnip::model_spec()`).

---

    Code
      fit_xy(mars_spec, x = lending_club[, 1:2], y = lending_club$Class)
    Condition
      Error in `fit_xy()`:
      ! Please set the mode in the model specification (`?parsnip::model_spec()`).

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
    

