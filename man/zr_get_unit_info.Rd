% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zr_get_unit_info.R
\name{zr_get_unit_info}
\alias{zr_get_unit_info}
\title{Create data frame with unit info}
\usage{
zr_get_unit_info(bldg_data)
}
\arguments{
\item{bldg_data}{either the file path to an OZFS *.bldg file or
a list created from the the *.bldg file using \code{rjson::fromJSON}}
}
\value{
a data frame with information for each unit in the building
}
\description{
\code{zr_get_unit_info()} takes a json representing a building and
returns a data frame with data for each unit in the building.
}
\examples{
bldg_file <- zr_example_files("2_fam.bldg")

zr_get_unit_info(bldg_file)
}
