# bad input

    Code
      boost_tree(mode = "bogus")
    Condition
      Error in `boost_tree()`:
      ! "bogus" is not a known mode for model `boost_tree()`.

---

    Code
      bt <- boost_tree(trees = -1) %>% set_engine("xgboost") %>% set_mode(
        "classification")
    Condition
      Error in `new_model_spec()`:
      ! `trees` must be a whole number larger than or equal to 0 or `NULL`, not the number -1.
    Code
      fit(bt, class ~ ., hpc)
    Condition
      Error:
      ! object 'bt' not found

---

    Code
      bt <- boost_tree(sample_size = -10) %>% set_engine("xgboost") %>% set_mode(
        "classification")
    Condition
      Error in `new_model_spec()`:
      ! `sample_size` must be a number between 0 and 1 or `NULL`, not the number -10.
    Code
      fit(bt, class ~ ., hpc)
    Condition
      Error:
      ! object 'bt' not found

---

    Code
      bt <- boost_tree(tree_depth = -10) %>% set_engine("xgboost") %>% set_mode(
        "classification")
    Condition
      Error in `new_model_spec()`:
      ! `tree_depth` must be a whole number larger than or equal to 0 or `NULL`, not the number -10.
    Code
      fit(bt, class ~ ., hpc)
    Condition
      Error:
      ! object 'bt' not found

---

    Code
      bt <- boost_tree(min_n = -10) %>% set_engine("xgboost") %>% set_mode(
        "classification")
    Condition
      Error in `new_model_spec()`:
      ! `min_n` must be a whole number larger than or equal to 0 or `NULL`, not the number -10.
    Code
      fit(bt, class ~ ., hpc)
    Condition
      Error:
      ! object 'bt' not found

