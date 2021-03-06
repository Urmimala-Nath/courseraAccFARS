% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Print "Read Yearly Accident Information from FARS Data"}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{The function argument which stores a set of user-input years as integers.}

\item{file}{Variable that stores file names corresponding to those years as string objects.}

\item{dat}{Dataframe Table storing the file specified by "file" for all valid years.}
}
\value{
This function returns the months specified in "file", corresponding to the years entered, as a list object.

NULL returned and a warning message is thrown in case of invalid year value, using the tryCatch() function.
}
\description{
This function reads a set of years entered by the user,
and stores the months from those years after reading from the corresponding files.
}
