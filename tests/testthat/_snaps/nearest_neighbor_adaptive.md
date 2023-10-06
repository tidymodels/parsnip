# updating

    Code
      nearest_neighbor_adaptive(neighbors = 5) %>% set_engine("dann",
        neighborhood_size = 10) %>% update(neighbors = tune(), neighborhood_size = tune())
    Output
      nearest neighbor adaptive Model Specification (classification)
      
      Main Arguments:
        neighbors = tune()
      
      Engine-Specific Arguments:
        neighborhood_size = tune()
      
      Computational engine: dann 
      

