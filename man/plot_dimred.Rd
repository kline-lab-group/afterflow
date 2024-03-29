% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figure-functions.R
\name{plot_dimred}
\alias{plot_dimred}
\title{Plot Dimensional Reduction}
\usage{
plot_dimred(fh, af, color = "cluster.id", prefix = character(), verbose = TRUE)
}
\arguments{
\item{fh}{FileHandler object}

\item{af}{AfterFlow object}

\item{color}{What to color each dot by in the plot, default "cluster.id"}
}
\value{
Nothing
}
\description{
Makes sure the passed object is a FileHandler object
}
\examples{
plot_dimred(fh, af) -> Saves the default dimensional reduction plot
}
