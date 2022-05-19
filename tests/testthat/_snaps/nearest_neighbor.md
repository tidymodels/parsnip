# updating

    Code
      nearest_neighbor(neighbors = 5) %>% set_engine("kknn", scale = FALSE) %>%
        update(neighbors = tune(), scale = tune())
    Output
      K-Nearest Neighbor Model Specification (unknown)
      
      Main Arguments:
        neighbors = tune()
      
      Engine-Specific Arguments:
        scale = tune()
      
      Computational engine: kknn 
      

