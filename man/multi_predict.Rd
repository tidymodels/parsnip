% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa_multi_predict.R, R/boost_tree.R,
%   R/glmnet-engines.R, R/mars.R, R/mlp.R, R/nearest_neighbor.R
\name{multi_predict}
\alias{multi_predict}
\alias{multi_predict.default}
\alias{multi_predict._xgb.Booster}
\alias{multi_predict._C5.0}
\alias{multi_predict._elnet}
\alias{multi_predict._lognet}
\alias{multi_predict._multnet}
\alias{multi_predict._glmnetfit}
\alias{multi_predict._earth}
\alias{multi_predict._torch_mlp}
\alias{multi_predict._train.kknn}
\title{Model predictions across many sub-models}
\usage{
multi_predict(object, ...)

\method{multi_predict}{default}(object, ...)

\method{multi_predict}{`_xgb.Booster`}(object, new_data, type = NULL, trees = NULL, ...)

\method{multi_predict}{`_C5.0`}(object, new_data, type = NULL, trees = NULL, ...)

\method{multi_predict}{`_elnet`}(object, new_data, type = NULL, penalty = NULL, ...)

\method{multi_predict}{`_lognet`}(object, new_data, type = NULL, penalty = NULL, ...)

\method{multi_predict}{`_multnet`}(object, new_data, type = NULL, penalty = NULL, ...)

\method{multi_predict}{`_glmnetfit`}(object, new_data, type = NULL, penalty = NULL, ...)

\method{multi_predict}{`_earth`}(object, new_data, type = NULL, num_terms = NULL, ...)

\method{multi_predict}{`_torch_mlp`}(object, new_data, type = NULL, epochs = NULL, ...)

\method{multi_predict}{`_train.kknn`}(object, new_data, type = NULL, neighbors = NULL, ...)
}
\arguments{
\item{object}{A \link[=model_fit]{model fit}.}

\item{...}{Optional arguments to pass to \code{predict.model_fit(type = "raw")}
such as \code{type}.}

\item{new_data}{A rectangular data object, such as a data frame.}

\item{type}{A single character value or \code{NULL}. Possible values are
\code{"numeric"}, \code{"class"}, \code{"prob"}, \code{"conf_int"}, \code{"pred_int"}, \code{"quantile"},
or \code{"raw"}. When \code{NULL}, \code{predict()} will choose an appropriate value
based on the model's mode.}

\item{trees}{An integer vector for the number of trees in the ensemble.}

\item{penalty}{A numeric vector of penalty values.}

\item{num_terms}{An integer vector for the number of MARS terms to retain.}

\item{epochs}{An integer vector for the number of training epochs.}

\item{neighbors}{An integer vector for the number of nearest neighbors.}
}
\value{
A tibble with the same number of rows as the data being predicted.
There is a list-column named \code{.pred} that contains tibbles with
multiple rows per sub-model. Note that, within the tibbles, the column names
follow the usual standard based on prediction \code{type} (i.e. \code{.pred_class} for
\code{type = "class"} and so on).
}
\description{
For some models, predictions can be made on sub-models in the model object.
}
