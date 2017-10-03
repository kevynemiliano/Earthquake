#' Top-level function to setup the data for analysis
#'
#' @return DataFrame with re-formatted NOAA data
#' @export
#'
#'
#' @examples \dontrun{setup_data()}
setup_data <- function() {
  dest_file <- download_NOAA()
  raw_data <- eq_read_raw_data(dest_file)
  df <- eq_clean_data(raw_data)
}


#' Function to download NOAA data (live download not yet working)
#'
#' @return raw data from the NOAA website
#' @export
#'
#' @importFrom utils download.file
#'
#' @examples \dontrun{download_NOAA()}
download_NOAA <- function() {
  download_dadote <- lubridate::as_date(Sys.time())
  # dest_file <- paste0("inst/extdata/","noaa_data_",download_date,".tsv")
  dest_file <- "inst/extdata/noaa_data_20170915.tsv" # hard-coded for now
  # utils::download.file(url = "https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1", destfile = dest_file)
  dest_file
}

#' Creates a DataFrame with filled in DATE value and properly formatted LONGITUDE, LATITUDE and
#'
#' @param raw_data list containing raw NOAA data
#'
#' @return DataFrame containing formatted NOAA data
#'
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{eq_clean_data(raw_data)}
eq_clean_data <- function(raw_data) {
  with(raw_data, {
    df <- raw_data %>%
      as.data.frame() %>%
      # dates missing 'month' or 'day'
      tidyr::replace_na(MONTH = 1, DAY = 1, HOUR = 0, MINUTE = 0, SECOND = 0) %>%
      dplyr::mutate(DATE = ISOdate(year = YEAR, month = MONTH, day = DAY,
                                   hour = HOUR, min = MINUTE, sec = SECOND, tz = "")) %>%
      # convert longitude and latitude to numeric
      dplyr::mutate(LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE)) %>%
      # convert tsunami to a logical flag
      dplyr::mutate(FLAG_TSUNAMI = ifelse(is.na(FLAG_TSUNAMI), FALSE,TRUE)) %>%
      eq_location_clean()
    return(df)
  })
}

#' Cleans the LOCATION_NAME of a DataFrame containing NOAA data
#'
#' @param df DataFrame containing partially-cleaned NOAA data
#'
#' @return DataFrame with reformatted LOCATION_NAME column
#' @export
#'
#' @importFrom dplyr mutate mutate_all
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{eq_location_clean(df)}
eq_location_clean<-function(df){
  with(df, {
    df %>%
      dplyr::mutate(LOCATION = gsub(".*:", "", LOCATION_NAME)) %>%
      dplyr::mutate(LOCATION = stringr::str_to_title(LOCATION_NAME))
    return(df)
  })
}

#' Imports raw data from file
#' @param file the file to read in
#' @return list containing imported raw data
#' @export
#'
#' @importFrom readr read_delim
#'
#' @examples \dontrun{eq_read_raw_data()}
eq_read_raw_data <- function(file) {
  readr::read_delim(file,
                    delim = "\t",
                    escape_double = FALSE,
                    trim_ws = TRUE)
}

