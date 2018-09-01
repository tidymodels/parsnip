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
