# updating

    Code
      surv_reg() %>% set_engine("flexsurv", cl = 0.99) %>% update(cl = tune())
    Output
      Parametric Survival Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        cl = tune()
      
      Computational engine: flexsurv 
      

