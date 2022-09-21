# updating

    Code
      boost_tree(trees = 1) %>% set_engine("C5.0", noGlobalPruning = TRUE) %>% update(
        trees = tune(), noGlobalPruning = tune())
    Output
      Boosted Tree Model Specification (unknown mode)
      
      Main Arguments:
        trees = tune()
      
      Engine-Specific Arguments:
        noGlobalPruning = tune()
      
      Computational engine: C5.0 
      

