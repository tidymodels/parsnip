# updating

    Code
      mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE) %>%
        update(hidden_units = tune(), Hess = tune())
    Output
      Single Layer Neural Network Model Specification (classification)
      
      Main Arguments:
        hidden_units = tune()
      
      Engine-Specific Arguments:
        Hess = tune()
      
      Computational engine: nnet 
      

