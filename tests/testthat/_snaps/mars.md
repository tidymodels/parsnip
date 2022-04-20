# updating

    Code
      expr1 %>% update(num_terms = 1)
    Output
      MARS Model Specification (unknown)
      
      Main Arguments:
        num_terms = 1
      
      Engine-Specific Arguments:
        model = FALSE
      
      Computational engine: earth 
      

---

    Code
      expr2 %>% update(nk = 10)
    Output
      MARS Model Specification (unknown)
      
      Main Arguments:
        num_terms = tune()
      
      Engine-Specific Arguments:
        nk = 10
      
      Computational engine: earth 
      

---

    Code
      expr3 %>% update(num_terms = 1, fresh = TRUE)
    Output
      MARS Model Specification (unknown)
      
      Main Arguments:
        num_terms = 1
      
      Engine-Specific Arguments:
        nk = tune()
      
      Computational engine: earth 
      

---

    Code
      expr4 %>% update(param_tibb)
    Output
      MARS Model Specification (unknown)
      
      Main Arguments:
        num_terms = 3
        prod_degree = 1
      
      Engine-Specific Arguments:
        nk = 10
      
      Computational engine: earth 
      

---

    Code
      expr4 %>% update(param_list)
    Output
      MARS Model Specification (unknown)
      
      Main Arguments:
        num_terms = 3
        prod_degree = 1
      
      Engine-Specific Arguments:
        nk = 10
      
      Computational engine: earth 
      

---

    Code
      expr4 %>% update(nk = tune()) %>% extract_parameter_set_dials()
    Output
      Collection of 1 parameters for tuning
      
       identifier type    object
               nk   nk nparam[+]
      

