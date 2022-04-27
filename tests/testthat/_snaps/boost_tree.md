# updating

    Code
      expr1 %>% update(trees = 10)
    Output
      Boosted Tree Model Specification (unknown)
      
      Main Arguments:
        trees = 10
      
      Engine-Specific Arguments:
        verbose = 0
      
      Computational engine: xgboost 
      

---

    Code
      expr1 %>% update(param_tibb)
    Output
      Boosted Tree Model Specification (unknown)
      
      Main Arguments:
        mtry = 1
        trees = 7
      
      Engine-Specific Arguments:
        verbose = 0
      
      Computational engine: xgboost 
      

---

    Code
      expr1 %>% update(param_list)
    Output
      Boosted Tree Model Specification (unknown)
      
      Main Arguments:
        mtry = 1
        trees = 7
      
      Engine-Specific Arguments:
        verbose = 0
      
      Computational engine: xgboost 
      

---

    Code
      expr2 %>% update(bands = 10)
    Output
      Boosted Tree Model Specification (unknown)
      
      Main Arguments:
        trees = tune()
      
      Engine-Specific Arguments:
        bands = 10
      
      Computational engine: C5.0 
      

---

    Code
      expr3 %>% update(trees = 1, fresh = TRUE)
    Output
      Boosted Tree Model Specification (unknown)
      
      Main Arguments:
        trees = 1
      
      Computational engine: xgboost 
      

---

    Code
      expr4 %>% update(noGlobalPruning = TRUE)
    Output
      Boosted Tree Model Specification (unknown)
      
      Engine-Specific Arguments:
        noGlobalPruning = TRUE
      
      Computational engine: C5.0 
      

