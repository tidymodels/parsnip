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
      

# submodel prediction

    Code
      multi_predict(reg_fit, newdata = mtcars[1:4, -1], num_terms = 5)
    Condition
      Error in `multi_predict()`:
      ! Please use `new_data` instead of `newdata`.

