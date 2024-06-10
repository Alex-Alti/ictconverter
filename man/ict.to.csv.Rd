\name{ict.to.csv}
\alias{ict.to.csv}
\title{Converts an ICARTT file to a comma-separated values file}
\usage{
ict.to.csv(input, outfile = NULL)
}
\arguments{
\item{input}{Path to the input .ict file}

\item{outfile}{Defaults to NULL: output file is created in the working directory with the same base name as the input file.}
}
\description{
Converts an ICARTT file to a comma-separated values file, printing the new .csv file path and file size..
}
\value{
CSV file is saved in same directory unless outfile is specified
}
\examples{
ict.to.csv(input, outfile = NULL)
}
