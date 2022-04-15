# updating

    Code
      expr1 %>% update(degree = 1)
    Output
      Polynomial Support Vector Machine Specification (regression)
      
      Main Arguments:
        degree = 1
      
      Engine-Specific Arguments:
        cross = 10
      
      Computational engine: kernlab 
      

---

    Code
      expr1 %>% update(param_tibb)
    Output
      Polynomial Support Vector Machine Specification (regression)
      
      Main Arguments:
        cost = 10
        degree = 3
      
      Engine-Specific Arguments:
        cross = 10
      
      Computational engine: kernlab 
      

---

    Code
      expr1 %>% update(param_list)
    Output
      Polynomial Support Vector Machine Specification (regression)
      
      Main Arguments:
        cost = 10
        degree = 3
      
      Engine-Specific Arguments:
        cross = 10
      
      Computational engine: kernlab 
      

---

    Code
      expr2 %>% update(scale_factor = 1, cross = 10)
    Output
      Polynomial Support Vector Machine Specification (regression)
      
      Main Arguments:
        degree = tune()
        scale_factor = 1
      
      Engine-Specific Arguments:
        cross = 10
      
      Computational engine: kernlab 
      

---

    Code
      expr3 %>% update(degree = 3, fresh = TRUE)
    Output
      Polynomial Support Vector Machine Specification (regression)
      
      Main Arguments:
        degree = 3
      
      Computational engine: kernlab 
      

