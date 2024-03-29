% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-prior.R
\name{stat_prior}
\alias{stat_prior}
\title{Draw Prior Information}
\usage{
stat_prior(fun, ..., layer_args = list())
}
\arguments{
\item{fun}{function. Prior function to use (see \link[rlang:as_function]{rlang::as_function}).}

\item{...}{passed to \code{fun}.}

\item{layer_args}{list. Override visual defaults passed to
\link[ggplot2:geom_function]{ggplot2::stat_function}.}
}
\description{
Convenience wrapper around \link[ggplot2:geom_function]{ggplot2::stat_function} to show prior densities.
}
