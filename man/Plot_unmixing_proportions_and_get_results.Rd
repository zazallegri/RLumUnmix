% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot_unmixing_proportions_and_get_results.R
\name{Plot_unmixing_proportions_and_get_results}
\alias{Plot_unmixing_proportions_and_get_results}
\title{Unmixing and plot results}
\usage{
Plot_unmixing_proportions_and_get_results(
  data_sol,
  source_list,
  Population_fraction,
  filename,
  error_threshold,
  score_threshold,
  nb_tracer,
  tracer_pair = NULL
)
}
\arguments{
\item{data_sol}{Data frame containing tracers used for unmixing. Output of function \code{\link[=Select_data_for_unmixing]{Select_data_for_unmixing()}}}

\item{source_list}{List of sources c("S1", "S2",...)}

\item{Population_fraction}{Expected population fraction}

\item{filename}{Name of file}

\item{error_threshold}{Select only tracers that have an error below error_threshold}

\item{score_threshold}{Select only tracers that have a score above score_threshold}

\item{nb_tracer}{Number of tracers to be selected (ordered from smallest to largest error)}

\item{tracer_pair}{Pair extracted from \code{\link[fingerPro:pairs]{fingerPro::pairs()}} output}
}
\value{
Results of unmixing process + plots of estimated proportions
}
\description{
Unmixing and plot results
}
\examples{
see vignettes
}
