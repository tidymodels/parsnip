#' Loan Data
#'
#' @details These data were downloaded from the Lending Club
#'  access site (see below) and are from the first quarter of 2016.
#'  A subset of the rows and variables are included here. The
#'  outcome is in the variable `Class` and is either "good" (meaning
#'  that the loan was fully paid back or currently on-time) or "bad"
#'  (charged off, defaulted, of 21-120 days late). A data dictionary
#'  can be found on the source website.
#'
#' @name lending_club
#' @aliases lending_club
#' @docType data
#' @return \item{lending_club}{a data frame}
#'
#' @source Lending Club Statistics https://www.lendingclub.com/info/download-data.action
#'
#' @keywords datasets
#' @examples
#' data(lending_club)
#' str(lending_club)
NULL


#' Watson Churn Data
#'
#' @details These data were downloaded from the IBM Watson site
#'  (see below) in September 2018. The data contain a factor for
#'  whether a customer churned or not. Alternatively, the `tenure`
#'  column presumably contains information on how long the customer
#'  has had an account. A survival analysis can be done on this
#'  column using the `churn` outcome as the censoring information. A
#'  data dictionary can be found on the source website.
#'
#' @name wa_churn
#' @aliases wa_churn
#' @docType data
#' @return \item{wa_churn}{a data frame}
#'
#' @source IBM Watson Analytics https://ibm.co/2sOvyvy
#'
#' @keywords datasets
#' @examples
#' data(wa_churn)
#' str(wa_churn)
NULL

#' Execution Time Data
#'
#' These data were collected from the CRAN web page for 13,626 R
#' packages. The time to complete the standard package checking
#' routine was collected In some cases, the package checking
#' process is stopped due to errors and these data are treated as
#' censored. It is less than 1 percent.
#'
#' As predictors, the associated package source code were
#' downloaded and parsed to create predictors, including
#'
#' * `authors`: The number of authors in the author field.
#' * `imports`: The number of imported packages.
#' * `suggests`: The number of packages suggested.
#' * `depends`: The number of hard dependencies.
#' * `Roxygen`: a binary indicator for whether Roxygen was used
#'   for documentation.
#' * `gh`: a binary indicator for whether the URL field contained
#'   a GitHub link.
#' * `rforge`: a binary indicator for whether the URL field
#'   contained a link to R-forge.
#' * `descr`: The number of characters (or, in some cases, bytes)
#'   in the description field.
#' * `r_count`: The number of R files in the R directory.
#' * `r_size`: The total disk size of the R files.
#' * `ns_import`: Estimated number of imported functions or methods.
#' * `ns_export`: Estimated number of exported functions or methods.
#' * `s3_methods`: Estimated number of S3 methods.
#' * `s4_methods`: Estimated number of S4 methods.
#' * `doc_count`: How many Rmd or Rnw files in the vignettes
#'   directory.
#' * `doc_size`: The disk size of the Rmd or Rnw files.
#' * `src_count`: The number of files in the `src` directory.
#' * `src_size`: The size on disk of files in the `src` directory.
#' * `data_count`  The number of files in the `data` directory.
#' * `data_size`: The size on disk of files in the `data` directory.
#' * `testthat_count`: The number of files in the `testthat`
#'   directory.
#' * `testthat_size`: The size on disk of files in the `testthat`
#'   directory.
#' * `check_time`: The time (in seconds) to run `R CMD check`
#'   using the "r-devel-windows-ix86+x86_64` flavor.
#' * `status`: An indicator for whether the tests completed.
#'
#' Data were collected on 2019-01-20.
#' @name check_times
#' @aliases check_times
#' @docType data
#' @return \item{check_times}{a data frame}
#'
#' @source CRAN
#'
#' @keywords datasets
#' @examples
#' data(check_times)
#' str(check_times)
NULL

