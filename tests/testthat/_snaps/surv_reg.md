# updating

    Code
      expr1 %>% update(dist = "lnorm", cl = 0.99)
    Output
      Parametric Survival Regression Model Specification (regression)
      
      Main Arguments:
        dist = lnorm
      
      Engine-Specific Arguments:
        cl = 0.99
      
      Computational engine: flexsurv 
      

---

    Code
      expr1 %>% update(param_tibb)
    Output
      Parametric Survival Regression Model Specification (regression)
      
      Main Arguments:
        dist = weibull
      
      Engine-Specific Arguments:
        cl = tune()
      
      Computational engine: flexsurv 
      

---

    Code
      expr1 %>% update(param_list)
    Output
      Parametric Survival Regression Model Specification (regression)
      
      Main Arguments:
        dist = weibull
      
      Engine-Specific Arguments:
        cl = tune()
      
      Computational engine: flexsurv 
      

