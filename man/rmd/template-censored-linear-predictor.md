Since risk regression and parametric survival models are modeling different characteristics (e.g. relative hazard versus event time), their linear predictors will be going in opposite directions. 

For example, for parametric models, the linear predictor _increases with time_. For proportional hazards models the linear predictor _decreases with time_ (since hazard is increasing). As such, the linear predictors for these two quantities will have opposite signs.

tidymodels does not treat different models differently when computing performance metrics.  To standardize across model types, the default for proportional hazards models is to have _increasing values with time_. As a result, the sign of the linear predictor will be the opposite of the value produced by the `predict()` method in the engine package. 

This behavior can be changed by using the `increasing` argument when calling `predict()` on a \pkg{parsnip} model object. 
