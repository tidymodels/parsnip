The `stop_iter()`  argument allows the model to prematurely stop training if the objective function does not improve within `early_stop` iterations. 

The best way to use this feature is in conjunction with an _internal validation set_. To do this, pass the `validation` parameter of \\code{\\link[=xgb_train]{xgb_train()}} via the parsnip \\code{\\link[=set_engine]{set_engine()}} function. This is the proportion of the training set that should be reserved for measuring performance (and stopping early). 

If the model specification has `early_stop >= trees`, `early_stop` is converted to `trees - 1` and a warning is issued. 
