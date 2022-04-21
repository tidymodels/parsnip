# updating

    Code
      expr1 %>% update(cost_complexity = 0.1)
    Output
      Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = 0.1
      
      Engine-Specific Arguments:
        model = FALSE
      
      Computational engine: rpart 
      

---

    Code
      expr1 %>% update(param_tibb)
    Output
      Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = 0.1
        min_n = 1
      
      Engine-Specific Arguments:
        model = FALSE
      
      Computational engine: rpart 
      

---

    Code
      expr1 %>% update(param_list)
    Output
      Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = 0.1
        min_n = 1
      
      Engine-Specific Arguments:
        model = FALSE
      
      Computational engine: rpart 
      

---

    Code
      expr2 %>% update(model = FALSE)
    Output
      Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = tune()
      
      Engine-Specific Arguments:
        model = FALSE
      
      Computational engine: rpart 
      

---

    Code
      expr3 %>% update(cost_complexity = 1, fresh = TRUE)
    Output
      Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = 1
      
      Computational engine: rpart 
      

