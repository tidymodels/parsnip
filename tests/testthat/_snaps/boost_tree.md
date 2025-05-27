# check_args() works

    Code
      spec <- set_mode(set_engine(boost_tree(trees = -1), "xgboost"),
      "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `trees` must be a whole number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      spec <- set_mode(set_engine(boost_tree(sample_size = -10), "xgboost"),
      "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `sample_size` must be a number between 0 and 1 or `NULL`, not the number -10.

---

    Code
      spec <- set_mode(set_engine(boost_tree(tree_depth = -10), "xgboost"),
      "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `tree_depth` must be a whole number larger than or equal to 0 or `NULL`, not the number -10.

---

    Code
      spec <- set_mode(set_engine(boost_tree(min_n = -10), "xgboost"),
      "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `min_n` must be a whole number larger than or equal to 0 or `NULL`, not the number -10.

