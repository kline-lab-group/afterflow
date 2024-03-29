% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filehandler-class.R
\name{filehandler_save_figure}
\alias{filehandler_save_figure}
\title{Figure Output}
\usage{
filehandler_save_figure(fh, ggp, title, filetype = "png", verbose = TRUE, ...)
}
\arguments{
\item{fh}{FileHandler object}

\item{ggp}{ggplot object}

\item{title}{Title for the saved plot}

\item{filetype}{Figure filetype, defaults to png}

\item{verbose}{Whether to print status messages, default TRUE}
}
\value{
Nothing
}
\description{
Saves given figure with title. Defaults to png.
}
\examples{
filehandler_save_figure(fh, ggp, "Figure") -> Saves given figure as output/Figure.png
}
