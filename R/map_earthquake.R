#' Create earthquak label
#'
#' @param df A data.frame such as this obtainded from NOAA.
#'
#' @return A column of earthquak annotation; locaion, magnitude and deaths. Ignores the fiels when NA.
#' @examples
#' \dontrun{
#' # load and clean data
#' file_path <- system.file("extdata", "signif.tsv", package = "MSDR")
#' signif <- read_tsv(file_path) %>%
#'   eq_clean_date %>%
#'   eq_clean_location
#'
#' # make graph
#' signif %>%
#' filter(COUNTRY == 'MEXICO' & year(date) >= 2000) %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map()
#' }
#' @import dplyr
#' @export
eq_create_label <- function(df) {
  with(df, {
    labels <- df %>%
      dplyr::select(LOCATION, EQ_PRIMARY, DEATHS) %>%

      dplyr::mutate(LOCATION = ifelse(is.na(LOCATION), '', paste("<b>Location: </b>", LOCATION, "<br>")),
                    EQ_PRIMARY = ifelse(is.na(EQ_PRIMARY), '', paste("<b>Magnitude: </b>", EQ_PRIMARY, "<br>")),
                    DEATHS = ifelse(is.na(DEATHS), '', paste("<b>Total deaths: </b>", DEATHS))) %>%
      dplyr::mutate(popup_text = paste(LOCATION, EQ_PRIMARY, DEATHS)) %>%
      dplyr::select(popup_text) %>% unlist()
    return(labels)
  })
}

#' Map earthquak data to a leaflet tile
#'
#' @param df A data.frame such as this obtainded from NOAA.
#' @param annot_col the column to use for annotations
#'
#' @return A graph with markers for specified earthquaks and popups with info; location, magnitude and deaths.
#' @examples
#' \dontrun{
#' # load and clean data
#' file_path <- system.file("extdata", "signif.tsv", package = "MSDR")
#' signif <- read_tsv(file_path) %>%
#'   eq_clean_date %>%
#'   eq_clean_location
#'
#' # make graph
#' signif %>%
#' filter(COUNTRY == 'MEXICO' & year(date) >= 2000) %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map()
#' }
#' @import leaflet
#' @export
eq_map <- function(df, annot_col = NULL) {
  with(df, {
    circle_radius <- df$EQ_PRIMARY

    popup_labels <- df %>%
      dplyr::select(annot_col) %>%
      dplyr::transmute_all(., as.character) %>%
      unlist(., use.names = FALSE)

    leaflet::leaflet(df) %>%
      leaflet::addProviderTiles(providers$CartoDB) %>%
      leaflet::addCircleMarkers(~LONGITUDE, ~LATITUDE,
                                popup = ~popup_labels,
                                labelOptions = labelOptions(direction = 'bottom'),
                                radius = circle_radius)
  })
}
