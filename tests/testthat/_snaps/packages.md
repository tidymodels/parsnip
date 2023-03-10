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

