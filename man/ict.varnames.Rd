\name{ict.varnames}
\alias{ict.varnames}
\title{Prints variable names}
\usage{
ict.varnames(input, units = FALSE)
}
\arguments{
\item{input}{Path to the input .ict file}

\item{units}{Defaults to FALSE; print all variables without units. If units = TRUE, variable is followed by unit.}
}
\description{
Prints variable names (independent and dependent)
}
\value{
A character vector
}
\examples{
ict.varnames(input, units = FALSE)
}
