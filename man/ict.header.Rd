\name{ict.header}
\alias{ict.header}
\title{Parses header of ICARTT file}
\usage{
ict.header(input, verbose = TRUE, raw = FALSE)
}
\arguments{
\item{input}{Path to the input .ict file}

\item{verbose}{Defaults to TRUE; print all fields with description. If verbose = FALSE, output excludes full variable list, 'normal comments' section, and data file details.}

\item{raw}{Defaults to FALSE. If true, print header as seen in ICARTT file, without field descriptions.}
}
\description{
Parses header of ICARTT file, with field descriptions according to official ICARTT Data Format guidelines (https://www-air.larc.nasa.gov/missions/etc/IcarttDataFormat.htm).
}
\value{
A character vector
}
\examples{
ict.header(input, verbose = TRUE, raw = FALSE)
}
