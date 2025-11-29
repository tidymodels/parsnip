# regression

    Code
      xy_res <- fit_xy(reg_mod, x = mtcars[, 1:5], y = mtcars$mpg, control = ctrl)
    Condition
      Error in `fit_xy()`:
      ! Please use `fit()` rather than `fit_xy()` to train generalized additive models with the "mgcv" engine.
      i See `?model_formula()` to learn more.

# classification

    Code
      xy_res <- fit_xy(cls_mod, x = two_class_dat[, 2:3], y = two_class_dat$Class,
      control = ctrl)
    Condition
      Error in `fit_xy()`:
      ! Please use `fit()` rather than `fit_xy()` to train generalized additive models with the "mgcv" engine.
      i See `?model_formula()` to learn more.

