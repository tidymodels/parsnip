% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlp.R
\name{mlp}
\alias{mlp}
\alias{update.mlp}
\title{General Interface for Single Layer Neural Network}
\usage{
mlp(
  mode = "unknown",
  hidden_units = NULL,
  penalty = NULL,
  dropout = NULL,
  epochs = NULL,
  activation = NULL
)

\method{update}{mlp}(
  object,
  parameters = NULL,
  hidden_units = NULL,
  penalty = NULL,
  dropout = NULL,
  epochs = NULL,
  activation = NULL,
  fresh = FALSE,
  ...
)
}
\arguments{
\item{mode}{A single character string for the type of model.
Possible values for this model are "unknown", "regression", or
"classification".}

\item{hidden_units}{An integer for the number of units in the hidden model.}

\item{penalty}{A non-negative numeric value for the amount of weight
decay.}

\item{dropout}{A number between 0 (inclusive) and 1 denoting the proportion
of model parameters randomly set to zero during model training.}

\item{epochs}{An integer for the number of training iterations.}

\item{activation}{A single character string denoting the type of relationship
between the original predictors and the hidden unit layer. The activation
function between the hidden and output layers is automatically set to either
"linear" or "softmax" depending on the type of outcome. Possible values are:
"linear", "softmax", "relu", and "elu"}

\item{object}{A multilayer perceptron model specification.}

\item{parameters}{A 1-row tibble or named list with \emph{main}
parameters to update. If the individual arguments are used,
these will supersede the values in \code{parameters}. Also, using
engine arguments in this object will result in an error.}

\item{fresh}{A logical for whether the arguments should be
modified in-place of or replaced wholesale.}

\item{...}{Not used for \code{update()}.}
}
\description{
\code{mlp()}, for multilayer perceptron, is a way to generate a \emph{specification} of
a model before fitting and allows the model to be created using
different packages in R or via keras The main arguments for the
model are:
\itemize{
\item \code{hidden_units}: The number of units in the hidden layer
(default: 5).
\item \code{penalty}: The amount of L2 regularization (aka weight
decay, default is zero).
\item \code{dropout}: The proportion of parameters randomly dropped out of
the model (\code{keras} only, default is zero).
\item \code{epochs}: The number of training iterations (default: 20).
\item \code{activation}: The type of function that connects the hidden
layer and the input variables  (\code{keras} only, default is softmax).
}

If parameters need to be modified, this function can be used
in lieu of recreating the object from scratch.
}
\details{
These arguments are converted to their specific names at the
time that the model is fit. Other options and arguments can be
set using \code{set_engine()}. If left to their defaults
here (see above), the values are taken from the underlying model
functions. One exception is \code{hidden_units} when \code{nnet::nnet} is used; that
function's \code{size} argument has no default so a value of 5 units will be
used. Also, unless otherwise specified, the \code{linout} argument to
\code{nnet::nnet()} will be set to \code{TRUE} when a regression model is created.
If parameters need to be modified, \code{update()} can be used
in lieu of recreating the object from scratch.

The model can be created using the \code{fit()} function using the
following \emph{engines}:
\itemize{
\item \pkg{R}:  \code{"nnet"} (the default)
\item \pkg{keras}: \code{"keras"}
}
}
\section{Engine Details}{
Engines may have pre-set default arguments when executing the model fit
call. For this type of model, the template of the fit calls are below:
\subsection{keras}{\if{html}{\out{<div class="r">}}\preformatted{mlp() \%>\% 
  set_engine("keras") \%>\% 
  set_mode("regression") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## Single Layer Neural Network Specification (regression)
## 
## Computational engine: keras 
## 
## Model fit template:
## parsnip::keras_mlp(x = missing_arg(), y = missing_arg())
}\if{html}{\out{<div class="r">}}\preformatted{mlp() \%>\% 
  set_engine("keras") \%>\% 
  set_mode("classification") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## Single Layer Neural Network Specification (classification)
## 
## Computational engine: keras 
## 
## Model fit template:
## parsnip::keras_mlp(x = missing_arg(), y = missing_arg())
}

An error is thrown if both \code{penalty} and \code{dropout} are specified for
\code{keras} models.
}

\subsection{nnet}{\if{html}{\out{<div class="r">}}\preformatted{mlp() \%>\% 
  set_engine("nnet") \%>\% 
  set_mode("regression") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## Single Layer Neural Network Specification (regression)
## 
## Main Arguments:
##   hidden_units = 5
## 
## Computational engine: nnet 
## 
## Model fit template:
## nnet::nnet(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     size = 5, trace = FALSE, linout = TRUE)
}\if{html}{\out{<div class="r">}}\preformatted{mlp() \%>\% 
  set_engine("nnet") \%>\% 
  set_mode("classification") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## Single Layer Neural Network Specification (classification)
## 
## Main Arguments:
##   hidden_units = 5
## 
## Computational engine: nnet 
## 
## Model fit template:
## nnet::nnet(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     size = 5, trace = FALSE, linout = FALSE)
}
}

\subsection{Parameter translations}{

The standardized parameter names in parsnip can be mapped to their
original names in each engine that has main parameters. Each engine
typically has a different default value (shown in parentheses) for each
parameter.\tabular{lll}{
   \strong{parsnip} \tab \strong{keras} \tab \strong{nnet} \cr
   hidden_units \tab hidden_units (5) \tab size \cr
   penalty \tab penalty (0) \tab decay (0) \cr
   dropout \tab dropout (0) \tab NA \cr
   epochs \tab epochs (20) \tab maxit (100) \cr
   activation \tab activation (softmax) \tab NA \cr
}

}
}

\examples{
show_engines("mlp")

mlp(mode = "classification", penalty = 0.01)
# Parameters can be represented by a placeholder:
mlp(mode = "regression", hidden_units = varying())
model <- mlp(hidden_units = 10, dropout = 0.30)
model
update(model, hidden_units = 2)
update(model, hidden_units = 2, fresh = TRUE)
}
\seealso{
\code{\link[=fit]{fit()}}
}
