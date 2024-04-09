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
      

# newdata error trapping

    Code
      predict(res_xy, newdata = hpc[1:3, num_pred])
    Condition
      Error in `predict()`:
      ! Please use `new_data` instead of `newdata`.

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
      ! The amount of regularization, `penalty`, should be `>= 0`.

