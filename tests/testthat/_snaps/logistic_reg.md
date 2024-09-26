# updating

    Code
      logistic_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10) %>% update(
        mixture = tune(), nlambda = tune())
    Output
      Logistic Regression Model Specification (classification)
      
      Main Arguments:
        mixture = tune()
      
      Engine-Specific Arguments:
        nlambda = tune()
      
      Computational engine: glmnet 
      

# bad input

    Code
      logistic_reg(mode = "regression")
    Condition
      Error in `logistic_reg()`:
      ! "regression" is not a known mode for model `logistic_reg()`.

---

    Code
      translate(logistic_reg(mixture = 0.5) %>% set_engine(engine = "LiblineaR"))
    Condition
      Error in `translate()`:
      ! For the LiblineaR engine, `mixture` must be 0 or 1.

---

    Code
      res <- mtcars %>% dplyr::mutate(cyl = as.factor(cyl)) %>% fit(logistic_reg(),
      cyl ~ mpg, data = .)
    Condition
      Warning:
      ! Logistic regression is intended for modeling binary outcomes, but there are 3 levels in the outcome.
      i If this is unintended, adjust outcome levels accordingly or see the `multinom_reg()` function.
      Warning:
      glm.fit: algorithm did not converge
      Warning:
      glm.fit: fitted probabilities numerically 0 or 1 occurred

# glm execution

    Code
      res <- fit(lc_basic, funded_amnt ~ term, data = lending_club, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `integer`.

---

    Code
      glm_form_catch <- fit(lc_basic, funded_amnt ~ term, data = lending_club,
      control = caught_ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `integer`.

---

    Code
      glm_xy_catch <- fit_xy(lc_basic, control = caught_ctrl, x = lending_club[,
        num_pred], y = lending_club$total_bal_il)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `integer`.

# liblinear execution

    Code
      res <- fit(ll_basic, funded_amnt ~ term, data = lending_club, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `integer`.

---

    Code
      glm_form_catch <- fit(ll_basic, funded_amnt ~ term, data = lending_club,
      control = caught_ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `integer`.

---

    Code
      glm_xy_catch <- fit_xy(ll_basic, control = caught_ctrl, x = lending_club[,
        num_pred], y = lending_club$total_bal_il)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `integer`.

# check_args() works

    Code
      spec <- logistic_reg(mixture = -1) %>% set_engine("glm") %>% set_mode(
        "classification")
      fit(spec, Class ~ ., lending_club)
    Condition
      Error in `fit()`:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- logistic_reg(penalty = -1) %>% set_engine("glm") %>% set_mode(
        "classification")
      fit(spec, Class ~ ., lending_club)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      spec <- logistic_reg(mixture = 0.5) %>% set_engine("LiblineaR") %>% set_mode(
        "classification")
      fit(spec, Class ~ ., lending_club)
    Condition
      Error in `fit()`:
      x For the LiblineaR engine, mixture must be 0 or 1, not 0.5.
      i Choose a pure ridge model with `mixture = 0` or a pure lasso model with `mixture = 1`.
      ! The Liblinear engine does not support other values.

---

    Code
      spec <- logistic_reg(penalty = 0) %>% set_engine("LiblineaR") %>% set_mode(
        "classification")
      fit(spec, Class ~ ., lending_club)
    Condition
      Error in `fit()`:
      ! For the LiblineaR engine, `penalty` must be `> 0`, not 0.

