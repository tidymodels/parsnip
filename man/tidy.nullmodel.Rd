% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nullmodel.R
\name{tidy.nullmodel}
\alias{tidy.nullmodel}
\title{Tidy method for null models}
\usage{
\method{tidy}{nullmodel}(x, ...)
}
\arguments{
\item{x}{A \code{nullmodel} object.}

\item{...}{Not used.}
}
\value{
A tibble with column \code{value}.
}
\description{
Return the results of \code{nullmodel} as a tibble
}
\examples{

nullmodel(mtcars[,-1], mtcars$mpg) \%>\% tidy()
}
\keyword{internal}