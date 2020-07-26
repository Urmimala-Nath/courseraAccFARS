#' Print "Read FARS Accident Data"
#'
#' This is a basic method that reads the file corresponding to the name which a user enters during function call in a seperate function.
#'
#' @param filename The function argument which stores the name of file the user wants to read, as a string object.
#' @param data A variable that stores the csv file read as a dataframe.
#'
#' @note An error message will be thrown if the file does not exist.
#' @note The user may customize this message inside the "stop" command of the function
#'       (edit quoted statements inside  \code{stop} argument).
#'
#' @return This function returns the csv file read as a dataframe table object.
#'
#' @importFrom "readr" "read_csv"
#' @importFrom "dplyr" "tbl_df"
#'
#' @example
#' \dontrun{fars_read(make_filename(2015))}
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Print "Make FILE NAME of Accident Data"
#'
#' This is a function that reads the year entered by a user during function call,
#' and prints the file name corresponding to that year.
#'
#' @param year The function argument which stores the year entered by the user, as an integer.
#'
#' @return This function prints the name of the file corresponding to the year entered as a string object.
#'
#' @note C-type formatting used here ("sprintf" to convert "year" from integer to string while printing message).
#'
#' @importFrom "readr" "read_csv"
#' @importFrom "dplyr" "tbl_df"
#'
#' @example
#' \dontrun{make_filename(2014)}
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Print "Read Yearly Accident Information from FARS Data"
#'
#' This function reads a set of years entered by the user,
#' and stores the months from those years after reading from the corresponding files.
#'
#' @param years The function argument which stores a set of user-input years as integers.
#' @param file Variable that stores file names corresponding to those years as string objects.
#' @param dat Dataframe Table storing the file specified by "file" for all valid years.
#'
#' @inheritParams make_filename To get the file-names corresponding to "years" and store in "file".
#' @inheritParams fars_read To read information from "file" and store in "dat".
#'
#' @return This function returns the months specified in "file", corresponding to the years entered, as a list object.
#' @return NULL returned and a warning message is thrown in case of invalid year value, using the tryCatch() function.
#'
#' @importFrom "dplyr" "mutate" "select" "%>%"
#'
#' @example
#' \dontrun{fars_read_years(c(2013,2014,2010))}
#'
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Print "Summarize Yearly Information on FARS Accidents in US"
#'
#' This function reads a set of user-input years,
#' and returns each occurrence of accidents in each month for all those years, after reading from the corresponding files.
#'
#' @param years The function argument which stores a set of years entered by the user as integers.
#' @param dat_list Dataframe storing information on months for all valid years.
#'
#' @inheritParams fars_read_years To read the years entered and store the return values into "dat_list".
#'
#' @return This function returns the summarized information on accidents for all months corresponding to the valid years entered
#'         The information returned is a Dataframe Table type object.
#'
#' @importFrom "dplyr" "%>%" "bind_rows" "group_by" "summarize"
#' @importFrom "tidyr" "spread"
#'
#' @example
#' \dontrun{fars_summarize_years(c(2012,2013,2015,2020))}
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Print "FARS Accident Data Mapping State-wise"
#'
#' This is a simple function which reads a State Number and a Year,
#' and graphically plots the number of accidents that occurred within the specific geography and timeframe.
#'
#' @param state.num The function argument which stores a user-input State Number as an integer.
#' @param year The function argument which stores a year specified by the user (integer datatype).
#' @param filename Stores the name of file corresponding to the year entered as a string.
#' @param data Stores the information in file specified by "filename" as Dataframe Table.
#' @param data.sub Contains information filtered from the file based on the specified State Number as a list.
#'
#' @inheritParams make_filename To get the file corresponding to the user-input year.
#' @inheritParams fars_read To read information from the specified file.
#'
#' @return This function returns a graphical plot of accidents occurring in the specified State during the year mentioned.
#'
#' @note An error message will be displayed if an invalid State Number has been specified.
#'       The user may customize this message inside "" of the \code{stop} argument
#' @note If there is no data corresponding to a valid State Number, no graphs will be plotted.
#'       The user may customize the corresponding message shown, inside "" of the \code{message} argument
#' @note For any unavailable Latitude or Longitude values of that State, a "TRUE" or "FALSE" value is assigned accordingly.
#'
#' @importFrom "dplyr" "filter"
#' @importFrom "maps" "map"
#' @importFrom "graphics" "points"
#'
#' @example
#' \dontrun{fars_map_state(56,2013)}
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
