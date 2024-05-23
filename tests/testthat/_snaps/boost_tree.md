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
      

# bad input

    Code
      boost_tree(mode = "bogus")
    Condition
      Error in `boost_tree()`:
      ! "bogus" is not a known mode for model `boost_tree()`.

# check_args() works

    Code
      spec <- boost_tree(trees = -1) %>% set_engine("xgboost") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `trees` must be a whole number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      spec <- boost_tree(sample_size = -10) %>% set_engine("xgboost") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `sample_size` must be a number between 0 and 1 or `NULL`, not the number -10.

---

    Code
      spec <- boost_tree(tree_depth = -10) %>% set_engine("xgboost") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `tree_depth` must be a whole number larger than or equal to 0 or `NULL`, not the number -10.

---

    Code
      spec <- boost_tree(min_n = -10) %>% set_engine("xgboost") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `min_n` must be a whole number larger than or equal to 0 or `NULL`, not the number -10.

