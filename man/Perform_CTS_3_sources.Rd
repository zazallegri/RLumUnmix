% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Perform_CTS_3_sources.R
\name{Perform_CTS_3_sources}
\alias{Perform_CTS_3_sources}
\title{Performs CTS method for three sources}
\usage{
Perform_CTS_3_sources(
  tracer_pair,
  mgeo,
  sgeo,
  crgeo,
  error_threshold = NULL,
  score_threshold = NULL,
  nb_tracers = NULL
)
}
\arguments{
\item{tracer_pair}{Pair extracted from \code{\link[fingerPro:pairs]{fingerPro::pairs()}} output}

\item{mgeo}{Data frame containing one of the dataset mixtures}

\item{sgeo}{Data frame containing the sediment sources from a dataset}

\item{crgeo}{Data frame containing the CR score for each tracer (from fingerPro::cr_ns())}

\item{error_threshold}{Select only tracers that have an error below error_threshold}

\item{score_threshold}{Select only tracers that have a score above score_threshold}

\item{nb_tracers}{Number of tracers to be selected (ordered from smallest to largest error)}
}
\value{
Data frame containing scores and errors for the best tracers
}
\description{
Performs CTS method for 3 sources. It then Selects the tracers to be used for the unmixing (fingerPro) based on score_threshold (consensus ranking method) and
error_threshold or nb_tracers (use either nb_tracers and score_threshold or error_threshold and score_threshold). More information is available at \url{https://github.com/eead-csic-eesa/fingerPro/tree/master}.
}
\examples{
see vignettes

}
