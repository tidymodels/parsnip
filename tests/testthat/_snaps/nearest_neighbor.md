# updating

    Code
      nearest_neighbor(neighbors = 5) %>% set_engine("kknn", scale = FALSE) %>%
        update(neighbors = tune(), scale = tune())
    Output
      K-Nearest Neighbor Model Specification (unknown mode)
      
      Main Arguments:
        neighbors = tune()
      
      Engine-Specific Arguments:
        scale = tune()
      
      Computational engine: kknn 
      

# bad input

    Code
      nearest_neighbor(mode = "reallyunknown")
    Condition
      Error in `nearest_neighbor()`:
      ! "reallyunknown" is not a known mode for model `nearest_neighbor()`.

---

    Code
      nearest_neighbor() %>% set_engine(NULL)
    Condition
      Error in `if (object$engine == "liquidSVM") ...`:
      ! argument is of length zero

# check_args() works

    Code
      spec <- nearest_neighbor(neighbors = -1) %>% set_engine("kknn") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `neighbors` must be a whole number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      spec <- nearest_neighbor(weight_func = 2) %>% set_engine("kknn") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `weight_func` must be a single string or `NULL`, not the number 2.

