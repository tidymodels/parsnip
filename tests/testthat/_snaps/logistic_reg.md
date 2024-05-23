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

