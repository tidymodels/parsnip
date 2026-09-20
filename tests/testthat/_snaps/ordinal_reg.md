# odds_link

    Code
      check_args(tidy_spec)
    Condition
      Error:
      ! The "polr" engine supports only the cumulative odds link.
      i Use the "vglm" or "ordinalNet" engine for alternative odds links.

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

# parallel_reg cannot be combined with a nominal engine argument

    Code
      check_args(set_engine(ordinal_reg(parallel_reg = FALSE), "clm", nominal = ~x))
    Condition
      Error:
      ! `parallel_reg` and the `nominal` engine argument cannot both be used.
      i `nominal` relaxes the parallel regression assumption for the predictors it names; omit `parallel_reg` to use it.

# unsupported non-parallel models give engine guidance

    Code
      check_ordinal_reg_parallel(spec, "polr")
    Condition
      Error:
      ! The "polr" engine does not support relaxing the parallel regression assumption.
      i Use the "clm", "vglm", or "ordinalNet" engine for non-parallel models.

