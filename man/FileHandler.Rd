% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filehandler-class.R
\name{FileHandler}
\alias{FileHandler}
\title{FileHandler Constructor}
\usage{
FileHandler(
  dir_main = NA_character_,
  dir_flow = "flow",
  dir_input = "input",
  dir_output = "output",
  dir_export = character(),
  file_meta = "metadata",
  fcs_prefix = "export",
  fcs_suffix = "",
  fcs_root = "CD3+",
  verbose = TRUE
)
}
\arguments{
\item{dir_main}{Primary working directory}

\item{dir_flow}{Directory with flow data}

\item{dir_input}{Directory with input data}

\item{dir_output}{Directory with output data}

\item{file_meta}{Metadata file}

\item{fcs_prefix}{Prefix for FCS files (default "export")}

\item{fcs_suffix}{Suffix for FCS files set in FlowJo (default "")}

\item{fcs_root}{Specifying the primary population}

\item{verbose}{Whether to print status messages, default TRUE}
}
\value{
FileHandler object
}
\description{
Constructor for the FileHandler object
}
\examples{
FileHandler() -> returns an FileHandler object after searching in working dir
}
