# updating

    Code
      expr1 %>% update(mtry = 2)
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = 2
      
      Engine-Specific Arguments:
        norm.votes = FALSE
        sampsize = tune()
      
      Computational engine: randomForest 
      

---

    Code
      expr3 %>% update(mtry = 2, fresh = TRUE)
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = 2
      
      Computational engine: randomForest 
      

---

    Code
      expr4 %>% update(sampsize = 10, norm.votes = TRUE)
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = 2
      
      Engine-Specific Arguments:
        norm.votes = TRUE
        sampsize = 10
      
      Computational engine: randomForest 
      

---

    Code
      expr4 %>% update(param_tibb)
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = 3
        trees = 10
      
      Engine-Specific Arguments:
        norm.votes = FALSE
        sampsize = tune()
      
      Computational engine: randomForest 
      

---

    Code
      expr4 %>% update(param_list)
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = 3
        trees = 10
      
      Engine-Specific Arguments:
        norm.votes = FALSE
        sampsize = tune()
      
      Computational engine: randomForest 
      

---

    Code
      expr5 %>% update(norm.votes = TRUE)
    Output
      Random Forest Model Specification (regression)
      
      Engine-Specific Arguments:
        norm.votes = TRUE
      
      Computational engine: randomForest 
      

