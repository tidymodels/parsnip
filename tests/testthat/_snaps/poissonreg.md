# misspelled engine names give informative error for extension packages

    Code
      set_mode(set_engine(poisson_reg(), "gml"), "regression")
    Condition
      Error in `set_engine()`:
      x Engine "gml" is not supported for `poisson_reg()`.
      i See `show_engines("poisson_reg")`.

