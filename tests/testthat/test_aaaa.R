# keras is pretty tenacious about writing output to the R session. By default
# this gets inserted into the test results. To make this less hideous, we
# try to trigger the output here so that it shows up at the top of the testing.

library(testthat)

# setting keras environment
Sys.setenv(TF_CPP_MIN_LOG_LEVEL = '3')
k_bk <- try(keras:::backend(), silent = TRUE)
