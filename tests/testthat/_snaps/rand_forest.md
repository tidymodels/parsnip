# updating

    Code
      rand_forest(mode = "regression", mtry = 2) %>% set_engine("randomForest",
        sampsize = 10) %>% update(mtry = tune(), sampsize = tune())
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = tune()
      
      Engine-Specific Arguments:
        sampsize = tune()
      
      Computational engine: randomForest 
      

