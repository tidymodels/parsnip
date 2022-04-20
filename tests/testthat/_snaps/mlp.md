# updating

    Code
      expr1 %>% update(hidden_units = 2)
    Output
      Single Layer Neural Network Specification (regression)
      
      Main Arguments:
        hidden_units = 2
      
      Engine-Specific Arguments:
        Hess = FALSE
        abstol = tune()
      
      Computational engine: nnet 
      

---

    Code
      expr2 %>% update(Hess = FALSE)
    Output
      Single Layer Neural Network Specification (regression)
      
      Engine-Specific Arguments:
        Hess = FALSE
      
      Computational engine: nnet 
      

---

    Code
      expr3 %>% update(hidden_units = 2, fresh = TRUE)
    Output
      Single Layer Neural Network Specification (regression)
      
      Main Arguments:
        hidden_units = 2
      
      Computational engine: keras 
      

---

    Code
      expr4 %>% update(abstol = 0.001)
    Output
      Single Layer Neural Network Specification (classification)
      
      Main Arguments:
        hidden_units = 2
      
      Engine-Specific Arguments:
        Hess = FALSE
        abstol = 0.001
      
      Computational engine: nnet 
      

---

    Code
      expr4 %>% update(param_tibb)
    Output
      Single Layer Neural Network Specification (classification)
      
      Main Arguments:
        hidden_units = 3
        dropout = 0.1
      
      Engine-Specific Arguments:
        Hess = FALSE
        abstol = tune()
      
      Computational engine: nnet 
      

---

    Code
      expr4 %>% update(param_list)
    Output
      Single Layer Neural Network Specification (classification)
      
      Main Arguments:
        hidden_units = 3
        dropout = 0.1
      
      Engine-Specific Arguments:
        Hess = FALSE
        abstol = tune()
      
      Computational engine: nnet 
      

