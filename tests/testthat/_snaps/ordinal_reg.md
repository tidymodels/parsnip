# odds_link

    Code
      . <- translate(tidy_spec)
    Condition
      Warning:
      ! The "polr" engine uses the cumulative link odds link; `odds_link` will be ignored.

# parallel_reg is validated

    Code
      check_args(ordinal_reg(parallel_reg = NA))
    Condition
      Error:
      ! `parallel_reg` must be `TRUE`, `FALSE`, or `NULL`, not `NA`.

---

    Code
      check_args(ordinal_reg(parallel_reg = c(TRUE, FALSE)))
    Condition
      Error:
      ! `parallel_reg` must be `TRUE`, `FALSE`, or `NULL`, not a logical vector.

---

    Code
      check_args(ordinal_reg(parallel_reg = 1))
    Condition
      Error:
      ! `parallel_reg` must be `TRUE`, `FALSE`, or `NULL`, not the number 1.

# VGAM arguments are translated

    Code
      match_ordinal_link_vglm("loglog")
    Condition
      Error in `match_ordinal_link_vglm()`:
      ! The VGAM engines do not support the log-log ordinal link.
      i See `?VGAM::Links` for provided link functions.

---

    Code
      translate_ordinal_vgam_args(list(link = rlang::quo("logistic"), family = rlang::quo(
        "adjacent_categories"), Thresh = NULL))
    Condition
      Error in `check_ordinal_link_family_vglm()`:
      ! The "adjacent_categories" family is not compatible with the "logitlink" link function.
      i Use "cauchitlink" or "identitylink" instead.

# unsupported non-parallel models give engine guidance

    Code
      check_ordinal_reg_parallel(spec, "polr")
    Condition
      Error:
      ! The "polr" engine does not support relaxing the parallel regression assumption.
      i Use the "clm", "vglm", or "ordinalNet" engine for non-parallel models.

