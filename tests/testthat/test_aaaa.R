# keras is pretty tenacious about writing output to the R session. By default
# this gets inserted into the test results. To make this less hideous, we
# try to trigger the output here so that it shows up at the top of the testing.

library(testthat)

context("setting keras environment")

Sys.setenv(TF_CPP_MIN_LOG_LEVEL = '3')
k_bk <- try(keras:::backend(), silent = TRUE)

## -----------------------------------------------------------------------------

context("checking testing environment")

cat("testing environment:\n")

print(capabilities())

lps <- .libPaths()
cat("Library paths:\n")
print(lps)

installed <- lapply(lps, function(x) rownames(installed.packages(x)))

has_rstanarm <- lapply(installed, function(x) any(x == "rstanarm"))
if (any(unlist(has_rstanarm))) {
  cat("rstanarm installed in:" ,
      paste0(lps[unlist(has_rstanarm)], collapse = ", "),
      "\n")
} else {
  cat("rstanarm not installed\n")
}






