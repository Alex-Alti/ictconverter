\name{ict.to.json}
\alias{ict.to.json}
\title{This function converts an ICARTT file to a JSON file}
\usage{
ict.to.json(input, outfile = NULL)
}
\arguments{
\item{input}{Path to the input .ict file}

\item{outfile}{Defaults to NULL: output file is created in the working directory with the same base name as the input file.}
}
\description{
This function converts an ICARTT file to a JSON file, printing the new .json file path and file size and (optionally) saving the metadata as a .txt .csv or .json. JSON format can be validated at: https://jsonlint.com/
}
\value{
JSON file is saved in same directory
}
\examples{
ict.to.json(input, outfile = NULL)
}
