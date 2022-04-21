# updating

    Code
      expr1 %>% update(neighbors = 5, scale = FALSE)
    Output
      K-Nearest Neighbor Model Specification (unknown)
      
      Main Arguments:
        neighbors = 5
      
      Engine-Specific Arguments:
        scale = FALSE
      
      Computational engine: kknn 
      

---

    Code
      expr1 %>% update(param_tibb)
    Output
      K-Nearest Neighbor Model Specification (unknown)
      
      Main Arguments:
        neighbors = 7
        dist_power = 1
      
      Engine-Specific Arguments:
        scale = FALSE
      
      Computational engine: kknn 
      

---

    Code
      expr1 %>% update(param_list)
    Output
      K-Nearest Neighbor Model Specification (unknown)
      
      Main Arguments:
        neighbors = 7
        dist_power = 1
      
      Engine-Specific Arguments:
        scale = FALSE
      
      Computational engine: kknn 
      

---

    Code
      expr1 %>% update(param_list, neighbors = 3)
    Output
      K-Nearest Neighbor Model Specification (unknown)
      
      Main Arguments:
        neighbors = 7
        dist_power = 1
      
      Engine-Specific Arguments:
        scale = FALSE
      
      Computational engine: kknn 
      

---

    Code
      expr2 %>% update(weight_func = "triangular", scale = FALSE)
    Output
      K-Nearest Neighbor Model Specification (unknown)
      
      Main Arguments:
        neighbors = tune()
        weight_func = triangular
      
      Engine-Specific Arguments:
        scale = FALSE
      
      Computational engine: kknn 
      

---

    Code
      expr3 %>% update(neighbors = 3, fresh = TRUE, scale = FALSE)
    Output
      K-Nearest Neighbor Model Specification (unknown)
      
      Main Arguments:
        neighbors = 3
      
      Engine-Specific Arguments:
        scale = FALSE
      
      Computational engine: kknn 
      

