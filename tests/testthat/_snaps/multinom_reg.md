# updating

    Code
      multinom_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10) %>% update(
        mixture = tune(), nlambda = tune())
    Output
      Multinomial Regression Model Specification (classification)
      
      Main Arguments:
        mixture = tune()
      
      Engine-Specific Arguments:
        nlambda = tune()
      
      Computational engine: glmnet 
      

# bad input

    Code
      multinom_reg(mode = "regression")
    Condition
      Error in `multinom_reg()`:
      ! "regression" is not a known mode for model `multinom_reg()`.

---

    Code
      translate(multinom_reg(penalty = 0.1) %>% set_engine("wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `multinom_reg()`
      i See `show_engines("multinom_reg")`.

---

    Code
      multinom_reg(penalty = 0.1) %>% set_engine()
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {glmnet, spark, keras, nnet, brulee}.

# check_args() works

    Code
      spec <- multinom_reg(mixture = -1) %>% set_engine("keras") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- multinom_reg(penalty = -1) %>% set_engine("keras") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

