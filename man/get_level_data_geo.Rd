% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explotation.R
\name{get_level_data_geo}
\alias{get_level_data_geo}
\alias{get_level_data_geo.geodimension}
\title{Get level data with latitude and longitude}
\usage{
get_level_data_geo(gd, level_name, inherited, add_prefix, lon_lat, crs)

\method{get_level_data_geo}{geodimension}(
  gd,
  level_name = NULL,
  inherited = FALSE,
  add_prefix = TRUE,
  lon_lat = c("intptlon", "intptlat"),
  crs = 4326
)
}
\arguments{
\item{gd}{A \code{geodimension} object.}

\item{level_name}{A string.}

\item{inherited}{A boolean.}

\item{add_prefix}{A boolean.}

\item{lon_lat}{A vector, name of longitude and latitude attributes.}

\item{crs}{A coordinate reference system: integer with the EPSG code, or
character with proj4string.}
}
\value{
A \code{tibble} object.
}
\description{
Get the data table of a given level with latitude and longitude.
}
\details{
It allows selecting whether we want only the data defined locally in the level
or also those that it inherits from other higher levels with which it is related.

In case of inheriting attributes from other levels, in the table, these can
have as a prefix the name of the level.

Additionally, we indicate the names of the fields where longitude and latitude
will be stored, as well as the crs that is used, if they are different from the
default values.
}
\examples{

ld <- gd_us |>
  get_level_data_geo(level_name = "county",
                     inherited = TRUE)

}
\seealso{
\code{\link{geodimension}}, \code{\link{geolevel}}

Other information gathering functions: 
\code{\link{get_higher_level_names}()},
\code{\link{get_level_data}()},
\code{\link{get_level_geometries}()},
\code{\link{get_level_layer}()},
\code{\link{get_level_names}()}
}
\concept{information gathering functions}
