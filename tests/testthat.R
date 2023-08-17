library(testthat)
library(parsnip)

# Added this check on 2023-08-17 because the debian check service was allowing
# data.tabe to use too many cores. This led to notes of about user time
# exceeding cpu time. We explained for 4 submissions that this is unrelated to
# parsnip. Our messages were ignored and the submission was deleted.
# The underlying issue is shown here:
# https://github.com/Rdatatable/data.table/issues/5658

if (!parsnip:::is_cran_check()) {
  test_check("parsnip")
}
