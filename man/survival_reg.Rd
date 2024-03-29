% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survival_reg.R
\name{survival_reg}
\alias{survival_reg}
\title{Parametric survival regression}
\usage{
survival_reg(mode = "censored regression", engine = "survival", dist = NULL)
}
\arguments{
\item{mode}{A single character string for the prediction outcome mode.
The only possible value for this model is "censored regression".}

\item{engine}{A single character string specifying what computational engine
to use for fitting.}

\item{dist}{A character string for the probability distribution of the
outcome. The default is "weibull".}
}
\description{
\code{survival_reg()} defines a parametric survival model. This function can fit
censored regression models.

\Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("survival_reg")}

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
survival_reg(argument = !!value)
}\if{html}{\out{</div>}}

Since survival models typically involve censoring (and require the use of
\code{\link[survival:Surv]{survival::Surv()}} objects), the \code{\link[=fit.model_spec]{fit.model_spec()}} function will require that the
survival model be specified via the formula interface.
}
\examples{
\dontshow{if (!parsnip:::is_cran_check()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
show_engines("survival_reg")

survival_reg(mode = "censored regression", dist = "weibull")
\dontshow{\}) # examplesIf}
}
\references{
\url{https://www.tidymodels.org}, \href{https://www.tmwr.org/}{\emph{Tidy Modeling with R}}, \href{https://www.tidymodels.org/find/parsnip/}{searchable table of parsnip models}
}
\seealso{
\Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("survival_reg")}
}
\keyword{internal}
