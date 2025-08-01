% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zr_example_files.R
\name{zr_example_files}
\alias{zr_example_files}
\title{Get path to zoneR's example files}
\usage{
zr_example_files(file = NULL)
}
\arguments{
\item{file}{The name of the example file. If NULL, then a list
of names of example files will be returned}
}
\value{
Either a list of names of the example files, or a path
to the specified file
}
\description{
zoneR has a few example OZFS files stored internally. \code{zr_example_files}
returns either a list of names of the example files, or the file path
of a specified file.
}
\examples{
zr_example_files()
zr_example_files("Paradise.parcel")

}
