To use the h2o engine with tidymodels, please run `h2o::h2o.init()` first. By default, This connects R to the local h2o server. This needs to be done in every new R session. You can also connect to a remote h2o server with an IP address, for more details see [h2o::h2o.init()]. 

h2o will automatically shut down the local h2o instance started by R when R is terminated. To manually stop the h2o server, run `h2o::h2o.shutdown()`. 
