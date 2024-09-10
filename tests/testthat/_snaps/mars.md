# updating

    Code
      expr1 %>% update(num_terms = tune(), nk = tune())
    Output
      MARS Model Specification (unknown mode)
      
      Main Arguments:
        num_terms = tune()
      
      Engine-Specific Arguments:
        nk = tune()
      
      Computational engine: earth 
      

# bad input

    Code
      translate(mars(mode = "regression") %>% set_engine())
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {earth} and regression {earth}.

---

    Code
      translate(mars() %>% set_engine("wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `mars()`
      i See `show_engines("mars")`.

---

    Code
      translate(mars(formula = y ~ x))
    Condition
      Error in `mars()`:
      ! unused argument (formula = y ~ x)

# mars execution

    Code
      res <- fit(hpc_basic, hpc_bad_form, data = hpc, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be `numeric`, not a `factor`.

# submodel prediction

    Code
      multi_predict(reg_fit, newdata = mtcars[1:4, -1], num_terms = 5)
    Condition
      Error in `multi_predict()`:
      ! Please use `new_data` instead of `newdata`.

# check_args() works

    Code
      spec <- mars(prod_degree = 0) %>% set_engine("earth") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `prod_degree` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      spec <- mars(num_terms = 0) %>% set_engine("earth") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `num_terms` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      spec <- mars(prune_method = 2) %>% set_engine("earth") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `prune_method` must be a single string or `NULL`, not the number 2.

