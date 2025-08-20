#' Read FARS Data File
#'
#' Reads a CSV file containing FARS data and returns it as a tibble.
#'
#' @param filename A character string giving the name of the file to read.
#'
#' @return A tibble containing the data from the file.
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create FARS Filename
#'
#' Generates a standard FARS filename for a given year.
#'
#' @param year An integer or string representing the year.
#'
#' @return A character string with the filename in the format "accident_<year>.csv.bz2".
#' @export
#'
#' @examples
#' make_filename(2015)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS Data for Multiple Years
#'
#' Reads FARS data for a vector of years and returns a list of tibbles with month and year columns.
#'
#' @param years A numeric or character vector of years.
#'
#' @return A list of tibbles, each containing the MONTH and year columns.
#' @export
#'
#' @importFrom dplyr mutate select
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014, 2015))
#' }
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

#' Summarize FARS Data by Month and Year
#'
#' Summarizes the number of accidents per month for each year.
#'
#' @param years A numeric or character vector of years.
#'
#' @return A tibble with months as rows and years as columns, showing the number of accidents.
#' @export
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot FARS Accidents on a State Map
#'
#' Plots the locations of accidents for a given state and year on a map.
#'
#' @param state.num An integer representing the state number.
#' @param year An integer or string representing the year.
#'
#' @return A map with accident locations plotted, or NULL if no data is available.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
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
