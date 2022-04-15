# updating

    Code
      expr1 %>% update(cost = 3)
    Output
      Linear Support Vector Machine Specification (regression)
      
      Main Arguments:
        cost = 3
      
      Engine-Specific Arguments:
        type = 12
      
      Computational engine: LiblineaR 
      

---

    Code
      expr1 %>% update(param_tibb)
    Output
      Linear Support Vector Machine Specification (regression)
      
      Main Arguments:
        cost = 10
        margin = 0.05
      
      Engine-Specific Arguments:
        type = 12
      
      Computational engine: LiblineaR 
      

---

    Code
      expr1 %>% update(param_list)
    Output
      Linear Support Vector Machine Specification (regression)
      
      Main Arguments:
        cost = 10
        margin = 0.05
      
      Engine-Specific Arguments:
        type = 12
      
      Computational engine: LiblineaR 
      

---

    Code
      expr2 %>% update(type = 13)
    Output
      Linear Support Vector Machine Specification (regression)
      
      Engine-Specific Arguments:
        type = 13
      
      Computational engine: LiblineaR 
      

---

    Code
      expr3 %>% update(cost = 5, fresh = TRUE)
    Output
      Linear Support Vector Machine Specification (regression)
      
      Main Arguments:
        cost = 5
      
      Computational engine: LiblineaR 
      

---

    Code
      expr4 %>% update(cost = 2)
    Output
      Linear Support Vector Machine Specification (regression)
      
      Main Arguments:
        cost = 2
      
      Engine-Specific Arguments:
        cross = 10
      
      Computational engine: kernlab 
      

---

    Code
      expr5 %>% update(cross = 10)
    Output
      Linear Support Vector Machine Specification (regression)
      
      Engine-Specific Arguments:
        cross = 10
      
      Computational engine: kernlab 
      

