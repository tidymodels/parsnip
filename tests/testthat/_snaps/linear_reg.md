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

