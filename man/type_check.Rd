% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.R
\name{type_check}
\alias{type_check}
\title{Type check an R file}
\usage{
type_check(path)
}
\arguments{
\item{path}{A character string; the file path.}
}
\description{
Type check an R file
}
\examples{
file <- system.file("example/test_1.R", package = "typeChecker")
cat(paste(readLines(file), collapse = "\n"))
type_check(file)

}
