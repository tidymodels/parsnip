# required packages

    Code
      expect_equal(req_pkgs(glmn), "glmnet")
    Condition
      Error:
      ! `req_pkgs()` was deprecated in parsnip 0.1.8 and is now defunct.
      i Please use `required_pkgs()` instead.

---

    Code
      expect_equal(req_pkgs(lm_fit), "stats")
    Condition
      Error:
      ! `req_pkgs()` was deprecated in parsnip 0.1.8 and is now defunct.
      i Please use `required_pkgs()` instead.

# missing packages

    Code
      predict(mars_model, mtcars[1:3, -1])
    Condition
      Error in `predict()`:
      ! Please install the rootveggie package to use this engine.

