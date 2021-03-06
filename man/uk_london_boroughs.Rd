% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{uk_london_boroughs}
\alias{uk_london_boroughs}
\title{UK London Boroughs}
\format{
A \code{sf}.
}
\source{
\url{https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london}
}
\usage{
uk_london_boroughs
}
\description{
From the original dataset, some fields have been selected and renamed.
}
\details{
Since not so much detail is needed, the geometry has been simplified 20 m.
}
\examples{

# Read by:
#
# filepath <- "data/London_Borough_Excluding_MHW/London_Borough_Excluding_MHW.shp"
# uk_london_boroughs <- st_read(filepath)  \%>\%
#     dplyr::select(
#       borough = NAME,
#       gss_code = GSS_CODE
#     ) \%>\%
#   st_simplify(dTolerance = 20)

}
\keyword{datasets}
