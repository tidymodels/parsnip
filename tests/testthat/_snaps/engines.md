# NULL engines

    Code
      set_engine(nearest_neighbor(), NULL)
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {kknn} and regression {kknn}.

# misspelled engine names give informative error

    Code
      set_mode(set_engine(poisson_reg(), "gml"), "regression")
    Condition
      Error in `set_engine()`:
      x Engine "gml" is not supported for `poisson_reg()`.
      i See `show_engines("poisson_reg")`.

