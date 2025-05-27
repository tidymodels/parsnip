# case weights with formula method -- unregistered model spec

    Code
      fit(bag_mars("regression"), Sale_Price ~ Longitude + Latitude, data = ames,
      case_weights = wts)
    Condition
      Error in `case_weights_allowed()`:
      ! Error in getting model information for model bag_mars with engine earth and mode regression.

