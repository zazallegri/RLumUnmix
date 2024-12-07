% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Get_sequence.R
\name{Get_sequence}
\alias{Get_sequence}
\title{Reads sequence}
\usage{
Get_sequence(file_path, lab_dose_rate)
}
\arguments{
\item{file_path}{Path to sequence file}

\item{lab_dose_rate}{Dose rate of lab instrument}
}
\value{
Sequence
}
\description{
Reads sequence file from .seq or .RData format.
.RData fromat is created when using function \code{\link[=Create_sequence_manually]{Create_sequence_manually()}} to create a sequence manually (ex from .lseq files).
Function \code{\link[=Get_sequence]{Get_sequence()}} returns a list of lists. Each list corresponds to a sequence step.
Order of the values in each sequence step is described at:
https://r-lum.github.io/RLumModel/reference/model_LuminescenceSignals.html
}
\examples{
see vignettes

}
