# parsnip objects

    Code
      multi_predict(lm_fit, mtcars)
    Condition
      Error in `multi_predict()`:
      ! No `multi_predict()` method exists for objects with classes <_lm/model_fit>.

---

    Code
      multi_predict(extract_fit_engine(mars_fit), mtcars)
    Condition
      Error in `multi_predict()`:
      ! No `multi_predict()` method exists for objects with classes <earth>.

# combine_words helper works

    Code
      combine_words(1)
    Output
      1

---

    Code
      combine_words(1:2)
    Output
      1 and 2

---

    Code
      combine_words(1:3)
    Output
      1, 2, and 3

---

    Code
      combine_words(1:4)
    Output
      1, 2, 3, and 4

# model type functions message informatively with unknown implementation

    Code
      bag_tree() %>% set_engine("rpart") %>% set_mode("regression")
    Message
      ! parsnip could not locate an implementation for `bag_tree` regression model specifications using the `rpart` engine.
      i The parsnip extension package baguette implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree() %>% set_mode("censored regression")
    Message
      ! parsnip could not locate an implementation for `bag_tree` censored regression model specifications.
      i The parsnip extension package censored implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (censored regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree()
    Message
      ! parsnip could not locate an implementation for `bag_tree` model specifications.
      i The parsnip extension packages censored and baguette implement support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (unknown mode)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree() %>% set_engine("rpart")
    Message
      ! parsnip could not locate an implementation for `bag_tree` model specifications using the `rpart` engine.
      i The parsnip extension packages censored and baguette implement support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (unknown mode)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

# missing implementation checks prompt conservatively with old objects

    Code
      bt
    Message
      ! parsnip could not locate an implementation for `bag_tree` model specifications.
      i The parsnip extension packages censored and baguette implement support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

# set_engine works as a generic

    Code
      set_engine(mtcars, "rpart")
    Condition
      Error in `set_engine()`:
      ! `set_engine()` expected a model specification to be supplied to the `object` argument, but received a(n) `data.frame` object.

# check_for_newdata points out correct context

    Code
      fn(newdata = "boop!")
    Condition
      Error in `fn()`:
      ! Please use `new_data` instead of `newdata`.

# check_outcome works as expected

    Code
      check_outcome(factor(1:2), reg_spec)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be `numeric`, not a `factor`.

---

    Code
      check_outcome(NULL, reg_spec)
    Condition
      Error:
      ! `linear_reg()` was unable to find an outcome.
      i Ensure that you have specified an outcome column and that it hasn't been removed in pre-processing.

---

    Code
      check_outcome(tibble::new_tibble(list(), nrow = 10), reg_spec)
    Condition
      Error:
      ! `linear_reg()` was unable to find an outcome.
      i Ensure that you have specified an outcome column and that it hasn't been removed in pre-processing.

---

    Code
      fit(reg_spec, ~mpg, mtcars)
    Condition
      Error:
      ! `linear_reg()` was unable to find an outcome.
      i Ensure that you have specified an outcome column and that it hasn't been removed in pre-processing.

---

    Code
      fit_xy(reg_spec, data.frame(x = 1:5), y = NULL)
    Condition
      Error:
      ! `linear_reg()` was unable to find an outcome.
      i Ensure that you have specified an outcome column and that it hasn't been removed in pre-processing.

---

    Code
      check_outcome(1:2, class_spec)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `integer`.

---

    Code
      check_outcome(NULL, class_spec)
    Condition
      Error:
      ! `logistic_reg()` was unable to find an outcome.
      i Ensure that you have specified an outcome column and that it hasn't been removed in pre-processing.

---

    Code
      check_outcome(tibble::new_tibble(list(), nrow = 10), class_spec)
    Condition
      Error:
      ! `logistic_reg()` was unable to find an outcome.
      i Ensure that you have specified an outcome column and that it hasn't been removed in pre-processing.

---

    Code
      fit(class_spec, ~mpg, mtcars)
    Condition
      Error:
      ! `logistic_reg()` was unable to find an outcome.
      i Ensure that you have specified an outcome column and that it hasn't been removed in pre-processing.

---

    Code
      check_outcome(1:2, cens_spec)
    Condition
      Error in `check_outcome()`:
      ! For a censored regression model, the outcome should be a `Surv` object, not a `integer`.

