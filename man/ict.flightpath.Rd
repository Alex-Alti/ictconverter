\name{ict.flightpath}
\alias{ict.flightpath}
\title{Plots simple flight path if latitude and longitude columns are present}
\usage{
ict.flightpath(input, lat = NULL, lon = NULL)
}
\arguments{
\item{input}{Path to the input .ict file}

\item{lat}{Specify latitude column; if empty, will search for column called "latitude" or including "lat"}

\item{lon}{Specify longitude column; if empty, will search for column called "longitude" or including "lon", "long", or "lng"}
}
\description{
Plots simple flight path if latitude and longitude columns are present.
}
\value{
A plotty-geo object
}
\examples{
ict.flightpath(input, lat = NULL, lon = NULL)
}
