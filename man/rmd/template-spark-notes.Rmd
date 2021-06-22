For models created using the `"spark"` engine, there are several things to consider. 

* Only the formula interface to via `fit()` is available; using `fit_xy()` will generate an error. 
* The predictions will always be in a Spark table format. The names will be the same as documented but without the dots. 
* There is no equivalent to factor columns in Spark tables so class predictions are returned as character columns. 
* To retain the model object for a new R session (via `save()`), the `model$fit` element of the parsnip object should be serialized via `ml_save(object$fit)` and separately saved to disk. In a new session, the object can be reloaded and reattached to the parsnip object.
