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

