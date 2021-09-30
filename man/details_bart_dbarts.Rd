% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bart_dbarts.R
\name{details_bart_dbarts}
\alias{details_bart_dbarts}
\title{Bayesian additive regression trees via dbarts}
\description{
\code{\link[dbarts:bart]{dbarts::bart()}} creates an ensemble of tree-based model whose training
and assembly is determined using Bayesian analysis.
}
\details{
For this engine, there are multiple modes: classification and regression
\subsection{Tuning Parameters}{

This model has 4 tuning parameters:
\itemize{
\item \code{trees}: # Trees (type: integer, default: 200L)
\item \code{prior_terminal_node_coef}: Terminal Node Prior Coefficient (type:
double, default: 0.95)
\item \code{prior_terminal_node_expo}: Terminal Node Prior Exponent (type:
double, default: 2.00)
\item \code{prior_outcome_range}: Prior for Outcome Range (type: double,
default: 2.00)
}
}

\subsection{Important engine-specific options}{

Some relevant arguments that can be passed to \code{set_engine()}:
}

\subsection{Translation from parsnip to the original package (classification)}{\if{html}{\out{<div class="sourceCode r">}}\preformatted{bart(
  trees = integer(1),
  prior_terminal_node_coef = double(1),
  prior_terminal_node_expo = double(1),
  prior_outcome_range = double(1)
) \%>\% 
  set_engine("dbarts") \%>\% 
  set_mode("classification") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## BART Model Specification (classification)
## 
## Main Arguments:
##   trees = integer(1)
##   prior_terminal_node_coef = double(1)
##   prior_terminal_node_expo = double(1)
##   prior_outcome_range = double(1)
## 
## Computational engine: dbarts 
## 
## Model fit template:
## dbarts::bart(x = missing_arg(), y = missing_arg(), ntree = integer(1), 
##     base = double(1), power = double(1), k = double(1), verbose = FALSE, 
##     keeptrees = TRUE, keepcall = FALSE)
}
}

\subsection{Translation from parsnip to the original package (regression)}{\if{html}{\out{<div class="sourceCode r">}}\preformatted{bart(
  trees = integer(1),
  prior_terminal_node_coef = double(1),
  prior_terminal_node_expo = double(1),
  prior_outcome_range = double(1)
) \%>\% 
  set_engine("dbarts") \%>\% 
  set_mode("regression") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## BART Model Specification (regression)
## 
## Main Arguments:
##   trees = integer(1)
##   prior_terminal_node_coef = double(1)
##   prior_terminal_node_expo = double(1)
##   prior_outcome_range = double(1)
## 
## Computational engine: dbarts 
## 
## Model fit template:
## dbarts::bart(x = missing_arg(), y = missing_arg(), ntree = integer(1), 
##     base = double(1), power = double(1), k = double(1), verbose = FALSE, 
##     keeptrees = TRUE, keepcall = FALSE)
}
}

\subsection{Preprocessing requirements}{

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
\code{\link[=fit.model_spec]{fit.model_spec()}}, parsnip will
convert factor columns to indicators.

\code{\link[dbarts:bart]{dbarts::bart()}} will also convert the factors to
indicators of the user does not create them first.
}

\subsection{References}{
\itemize{
\item Chipman, George, McCulloch. “BART: Bayesian additive regression
trees.” \emph{Ann. Appl. Stat.} 4 (1) 266 - 298, March 2010.
}
}
}
\keyword{internal}