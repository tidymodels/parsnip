# updating

    Code
      expr1 %>% update(mixture = 0)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        mixture = 0
      
      Engine-Specific Arguments:
        model = FALSE
      
      Computational engine: lm 
      

---

    Code
      expr2 %>% update(nlambda = 10)
    Output
      Linear Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr3 %>% update(mixture = 1, fresh = TRUE, nlambda = 10)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        mixture = 1
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr3 %>% update(nlambda = 10)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = tune()
        mixture = 0
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr4 %>% update(param_tibb)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 1
        mixture = 0.333333333333333
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr4 %>% update(param_list)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 1
        mixture = 0.333333333333333
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr5 %>% update(family = "poisson")
    Output
      Linear Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        family = poisson
      
      Computational engine: glm 
      

