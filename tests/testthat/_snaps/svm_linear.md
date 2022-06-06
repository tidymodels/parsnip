# updating

    Code
      svm_linear(mode = "regression", cost = 2) %>% set_engine("kernlab", cross = 10) %>%
        update(cross = tune(), cost = tune())
    Output
      Linear Support Vector Machine Model Specification (regression)
      
      Main Arguments:
        cost = tune()
      
      Engine-Specific Arguments:
        cross = tune()
      
      Computational engine: kernlab 
      

