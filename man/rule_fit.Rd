% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rule_fit.R
\name{rule_fit}
\alias{rule_fit}
\title{RuleFit models}
\usage{
rule_fit(
  mode = "unknown",
  mtry = NULL,
  trees = NULL,
  min_n = NULL,
  tree_depth = NULL,
  learn_rate = NULL,
  loss_reduction = NULL,
  sample_size = NULL,
  stop_iter = NULL,
  penalty = NULL,
  engine = "xrf"
)
}
\arguments{
\item{mode}{A single character string for the prediction outcome mode.
Possible values for this model are "unknown", "regression", or
"classification".}

\item{mtry}{A number for the number (or proportion) of predictors that will
be randomly sampled at each split when creating the tree models
(specific engines only).}

\item{trees}{An integer for the number of trees contained in
the ensemble.}

\item{min_n}{An integer for the minimum number of data points
in a node that is required for the node to be split further.}

\item{tree_depth}{An integer for the maximum depth of the tree (i.e. number
of splits) (specific engines only).}

\item{learn_rate}{A number for the rate at which the boosting algorithm adapts
from iteration-to-iteration (specific engines only). This is sometimes referred to
as the shrinkage parameter.}

\item{loss_reduction}{A number for the reduction in the loss function required
to split further (specific engines only).}

\item{sample_size}{A number for the number (or proportion) of data that is
exposed to the fitting routine. For \code{xgboost}, the sampling is done at
each iteration while \code{C5.0} samples once during training.}

\item{stop_iter}{The number of iterations without improvement before
stopping (specific engines only).}

\item{penalty}{L1 regularization parameter.}

\item{engine}{A single character string specifying what computational engine
to use for fitting.}
}
\description{
\code{rule_fit()} defines a model that derives simple feature rules from a tree
ensemble and uses them as features in a regularized model. This function can
fit classification and regression models.

\Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("rule_fit")}

More information on how \pkg{parsnip} is used for modeling is at
\url{https://www.tidymodels.org/}.
}
\details{
The RuleFit model creates a regression model of rules in two stages. The
first stage uses a tree-based model that is used to generate a set of rules
that can be filtered, modified, and simplified. These rules are then added
as predictors to a regularized generalized linear model that can also
conduct feature selection during model training.

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
rule_fit(argument = !!value)
}\if{html}{\out{</div>}}
}
\examples{
\dontshow{if (!parsnip:::is_cran_check()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
show_engines("rule_fit")

rule_fit()
\dontshow{\}) # examplesIf}
}
\references{
Friedman, J. H., and Popescu, B. E. (2008). "Predictive learning
via rule ensembles." \emph{The Annals of Applied Statistics}, 2(3), 916-954.

\url{https://www.tidymodels.org}, \href{https://www.tmwr.org/}{\emph{Tidy Modeling with R}}, \href{https://www.tidymodels.org/find/parsnip/}{searchable table of parsnip models}
}
\seealso{
\code{\link[xrf:xrf.formula]{xrf::xrf.formula()}}, \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("rule_fit")}
}
