% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mars.R
\name{mars}
\alias{mars}
\title{Multivariate adaptive regression splines (MARS)}
\usage{
mars(
  mode = "unknown",
  engine = "earth",
  num_terms = NULL,
  prod_degree = NULL,
  prune_method = NULL
)
}
\arguments{
\item{mode}{A single character string for the prediction outcome mode.
Possible values for this model are "unknown", "regression", or
"classification".}

\item{engine}{A single character string specifying what computational engine
to use for fitting.}

\item{num_terms}{The number of features that will be retained in the
final model, including the intercept.}

\item{prod_degree}{The highest possible interaction degree.}

\item{prune_method}{The pruning method.}
}
\description{
\code{mars()} defines a generalized linear model that uses artificial features for
some predictors. These features resemble hinge functions and the result is
a model that is a segmented regression in small dimensions. This function can
fit classification and regression models.

\Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("mars")}

More information on how \pkg{parsnip} is used for modeling is at
\url{https://www.tidymodels.org/}.
}
\details{
This function only defines what \emph{type} of model is being fit. Once an engine
is specified, the \emph{method} to fit the model is also defined. See
\code{\link[=set_engine]{set_engine()}} for more on setting the engine, including how to set engine
arguments.

The model is not trained or fit until the \code{\link[=fit.model_spec]{fit()}} function is used
with the data.

Each of the arguments in this function other than \code{mode} and \code{engine} are
captured as \link[rlang:topic-quosure]{quosures}. To pass values
programmatically, use the \link[rlang:injection-operator]{injection operator} like so:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{value <- 1
mars(argument = !!value)
}\if{html}{\out{</div>}}
}
\examples{
\dontshow{if (!parsnip:::is_cran_check()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
show_engines("mars")

mars(mode = "regression", num_terms = 5)
\dontshow{\}) # examplesIf}
}
\references{
\url{https://www.tidymodels.org}, \href{https://www.tmwr.org/}{\emph{Tidy Modeling with R}}, \href{https://www.tidymodels.org/find/parsnip/}{searchable table of parsnip models}
}
\seealso{
\Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("mars")}
}
