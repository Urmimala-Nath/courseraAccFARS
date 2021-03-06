% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Print "Read FARS Accident Data"}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{The function argument which stores the name of file the user wants to read, as a string object.}

\item{data}{A variable that stores the csv file read as a dataframe.}
}
\value{
This function returns the csv file read as a dataframe table object.
}
\description{
This is a basic method that reads the file corresponding to the name which a user enters during function call in a seperate function.
}
\note{
An error message will be thrown if the file does not exist.

The user may customize this message inside the "stop" command of the function
      (edit quoted statements inside  \code{stop} argument).
}
