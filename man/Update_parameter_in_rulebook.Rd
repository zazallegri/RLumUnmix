% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Update_parameter_in_rulebook.R
\name{Update_parameter_in_rulebook}
\alias{Update_parameter_in_rulebook}
\title{Updates parameter using \code{\link[sandbox:set_Rule]{sandbox::set_Rule()}}. Simple sets population x parameter value along the whole depth.}
\usage{
Update_parameter_in_rulebook(
  book,
  parameter_name,
  depth,
  populations_parameters
)
}
\arguments{
\item{book}{Sandbox RuleBook}

\item{parameter_name}{name of parameter to be changed}

\item{depth}{\link{numeric} \link{list}, specifying the depths used for the
interpolation.}

\item{populations_parameters}{Parameters of quartz luminescence models for every population}
}
\value{
updated RuleBook
}
\description{
Updates parameter using \code{\link[sandbox:set_Rule]{sandbox::set_Rule()}}. Simple sets population x parameter value along the whole depth.
}
\examples{
see vignettes
}
