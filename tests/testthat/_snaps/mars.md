# updating

    Code
      update(expr1, num_terms = tune(), nk = tune())
    Output
      MARS Model Specification (unknown mode)
      
      Main Arguments:
        num_terms = tune()
      
      Engine-Specific Arguments:
        nk = tune()
      
      Computational engine: earth 
      

# bad input

    Code
      translate(set_engine(mars(mode = "regression")))
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {earth} and regression {earth}.

---

    Code
      translate(set_engine(mars(), "wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `mars()`
      i See `show_engines("mars")`.

# submodel prediction

    Code
      multi_predict(reg_fit, newdata = mtcars[1:4, -1], num_terms = 5)
    Condition
      Error in `multi_predict()`:
      ! Please use `new_data` instead of `newdata`.

# check_args() works

    Code
      spec <- set_mode(set_engine(mars(prod_degree = 0), "earth"), "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `prod_degree` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      spec <- set_mode(set_engine(mars(num_terms = 0), "earth"), "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `num_terms` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      spec <- set_mode(set_engine(mars(prune_method = 2), "earth"), "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `prune_method` must be a single string or `NULL`, not the number 2.

