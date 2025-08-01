% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zr_get_crs.R
\name{zr_get_crs}
\alias{zr_get_crs}
\title{Find EPSG code for State Plane CRS}
\usage{
zr_get_crs(geom_data, large_area = FALSE)
}
\arguments{
\item{geom_data}{Either a simple feature collection or a
path to a file containing geospatial data}

\item{large_area}{Set this to \code{TRUE} if your data may
cross multiple state planes. It will be a bit longer but
find the state plane that covers it the best}
}
\value{
Returns the appropriate epsg code as an integer
}
\description{
\code{zr_get_crs()} uses state plane geometry to find an appropriate NAD83
crs for the input geometry
}
\details{
The state plane data was compiled using \href{https://hub.arcgis.com/datasets/esri::usa-state-plane-zones-nad83/explore}{ArcGIS Hub}
and the \href{https://epsg.io/}{epsg.io} website.
}
\examples{
file <- zr_example_files("Paradise.zoning")
zr_get_crs(file)
}
