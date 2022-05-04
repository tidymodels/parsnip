# updating

    Code
      svm_poly(mode = "regression", degree = 2) %>% set_engine("kernlab", cross = 10) %>%
        update(degree = tune(), cross = tune())
    Output
      Polynomial Support Vector Machine Specification (regression)
      
      Main Arguments:
        degree = tune()
      
      Engine-Specific Arguments:
        cross = tune()
      
      Computational engine: kernlab 
      

