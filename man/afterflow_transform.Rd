% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afterflow-class.R
\name{afterflow_transform}
\alias{afterflow_transform}
\title{AfterFlow Transformation}
\usage{
afterflow_transform(af, transform_method = "arcsinh", verbose = TRUE)
}
\arguments{
\item{af}{AfterFlow object}

\item{transform_method}{Options include arcsinh, lgcl_ct, lgcl_fc,
log, biexp, none. Defaults to arcsinh.}

\item{verbose}{Boolean for verbose message printing, default TRUE}
}
\value{
AfterFlow object
}
\description{
Applies the transformation specified to the AfterFlow object
}
\examples{
afterflow_transform(af) -> returns a transformed AfterFlow object with arcsinh
}
