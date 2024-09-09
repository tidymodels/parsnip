# updating

    Code
      linear_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10) %>% update(
        mixture = tune(), nlambda = tune())
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        mixture = tune()
      
      Engine-Specific Arguments:
        nlambda = tune()
      
      Computational engine: glmnet 
      

# bad input

    Code
      linear_reg(mode = "classification")
    Condition
      Error in `linear_reg()`:
      ! "classification" is not a known mode for model `linear_reg()`.

---

    Code
      translate(linear_reg(), engine = "wat?")
    Condition
      Error in `translate.default()`:
      x Engine "wat?" is not supported for `linear_reg()`
      i See `show_engines("linear_reg")`.

---

    Code
      translate(linear_reg(), engine = NULL)
    Condition
      Error in `translate.default()`:
      ! Please set an engine.

---

    Code
      translate(linear_reg(formula = y ~ x))
    Condition
      Error in `linear_reg()`:
      ! unused argument (formula = y ~ x)

---

    Code
      translate(linear_reg(x = hpc[, 1:3], y = hpc$class) %>% set_engine("glmnet"))
    Condition
      Error in `linear_reg()`:
      ! unused arguments (x = hpc[, 1:3], y = hpc$class)

---

    Code
      translate(linear_reg(formula = y ~ x) %>% set_engine("lm"))
    Condition
      Error in `linear_reg()`:
      ! unused argument (formula = y ~ x)

# lm execution

    Code
      res <- fit_xy(hpc_basic, x = hpc[, num_pred], y = hpc$class, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be `numeric`, not a `factor`.

---

    Code
      res <- fit_xy(hpc_basic, x = hpc[, num_pred], y = as.character(hpc$class),
      control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be `numeric`, not a `character`.

---

    Code
      res <- fit(hpc_basic, hpc_bad_form, data = hpc, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be `numeric`, not a `factor`.

---

    Code
      lm_form_catch <- fit(hpc_basic, hpc_bad_form, data = hpc, control = caught_ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be `numeric`, not a `factor`.

# glm execution

    Code
      res <- fit_xy(hpc_glm, x = hpc[, num_pred], y = hpc$class, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be `numeric`, not a `factor`.

---

    Code
      res <- fit(hpc_glm, hpc_bad_form, data = hpc, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be `numeric`, not a `factor`.

---

    Code
      lm_form_catch <- fit(hpc_glm, hpc_bad_form, data = hpc, control = caught_ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be `numeric`, not a `factor`.

# newdata error trapping

    Code
      predict(res_xy, newdata = hpc[1:3, num_pred])
    Condition
      Error in `predict()`:
      ! Please use `new_data` instead of `newdata`.

# show engine

    Code
      show_engines("linear_re")
    Condition
      Error in `show_engines()`:
      ! No results found for model function "x".

# check_args() works

    Code
      spec <- linear_reg(mixture = -1) %>% set_engine("lm") %>% set_mode("regression")
      fit(spec, compounds ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- linear_reg(penalty = -1) %>% set_engine("lm") %>% set_mode("regression")
      fit(spec, compounds ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

