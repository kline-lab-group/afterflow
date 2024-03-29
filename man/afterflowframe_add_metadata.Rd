% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afterflow-class.R
\name{afterflowframe_add_metadata}
\alias{afterflowframe_add_metadata}
\title{AfterFlowFrame Add Metadata}
\usage{
afterflowframe_add_metadata(
  af,
  metadata = c(),
  population = c(),
  verbose = TRUE
)
}
\arguments{
\item{af}{AfterFlow object}

\item{metadata}{Named vector of additional/modified metadata - will overwrite any matching names}

\item{population}{Named vector of filename/path of FCS file with subpopulation names}

\item{verbose}{Boolean for verbose message printing, default TRUE}
}
\value{
AfterFlow object
}
\description{
Adds metadata to AfterFlowFrame object
}
\examples{
afterflowframe_add_metadata(af) -> returns an AfterFlow object with updated metadata
}
