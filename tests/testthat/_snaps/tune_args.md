# find_tune_id() errors with multiple tune() in one arg

    Code
      find_tune_id(quote(c(tune(), tune())))
    Condition
      Error:
      ! Only one tunable value is currently allowed per argument. The current argument has: `c(tune(), tune())`.

