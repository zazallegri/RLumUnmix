% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Compute_intensity_std_moving_window.R
\name{Compute_intensity_std_moving_window}
\alias{Compute_intensity_std_moving_window}
\title{Compute intensity standard deviation}
\usage{
Compute_intensity_std_moving_window(
  local_extrema,
  df,
  std_computation_window_size_degrees
)
}
\arguments{
\item{local_extrema}{All local extrema}

\item{df}{Curve data}

\item{std_computation_window_size_degrees}{Window size for intensity standard deviation computation (in °C)}
}
\value{
Intensity standard deviation window for every extrema
}
\description{
Retrieve standard deviation of TL intensity with window around local max
}
\examples{
see vignettes
}
