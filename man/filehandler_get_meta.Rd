% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filehandler-class.R
\name{filehandler_get_meta}
\alias{filehandler_get_meta}
\title{Metadata}
\usage{
filehandler_get_meta(fh, exp = character())
}
\arguments{
\item{fh}{FileHandler object}
}
\value{
List of filenames
}
\description{
Returns all metadata or by exp filter
}
\examples{
filehandler_get_fcs_exp(fh) -> If CD3+ exists, return list of filenames
}
