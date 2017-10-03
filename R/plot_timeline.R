#' Stat for creating a timeline for earthquake data
#'
#' This function creates a new stat which will create a linear timeline for a
#' specified time range, and will display each earthquake as a point on the timeline.
#'
#' This code is based on input from the Extending ggplot2 vignette:
#' https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
#'
#' @param mapping A set of aesthetic mappings created by aes or aes_.  If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mappint at the top level of the plot.  You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.  There are three options:
#'
#' If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#'
#' A data.frame, or other object, will override the plot data.  All objects will be fortified to produce a data frame.
#' See fortify for which variables will be created.
#'
#' A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data.
#' @param geom The geom to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param ... Other arguments passed on layer.  Thes are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3.
#' They may also be parameters to the paired geom/stat.
#' @param na.rm If FALSE, the default, missing values are mremoved with a warning.  If TRUE, missing values are silently removed.
#' @param show.legend logical.  Should this layer be included in the legends?  NA, the default, incldes if any aesthetics are mapped.  False never includes,
#' and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.  This is most usefule for helper functions that
#' define both data and aesthetics and shouldn't inherit behavior from the default plot specification, e.t. borders.
#' @param x Date of the earthquake (required)
#' #@param y Factor indicating some striatification in which case multiple time lines will be ploted for each level of the factor (e.g. country).
#' @param xmindate Minimum year to display on the timeline
#' @param xmaxdate Maximum year to display on the timeline
#'
#' @return This function has no return value
#'
#' @importFrom ggplot2 ggproto layer Stat
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{stat_timeline(data = earthquakes_clean,
#'                        aes(x = DATE),
#'                        xmindate = 2000, xmaxdate = 2017)}
#'
#' @keywords internal
#'
stat_timeline <- function(mapping = NULL, data = NULL, geom = "point",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,
                          x = NULL, xmindate = NULL, xmaxdate = NULL,
                          ...) {
  ggplot2::layer(
    stat = StatTimeline,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      xmindate = xmindate,
      xmaxdate = xmaxdate,
      na.rm = na.rm, ...)
  )
}


StatTimeline <- ggplot2::ggproto("StatTimeline", ggplot2::Stat,
                                 required_aes = "x",
                                 optional_aes = c("n_max", "xmindata", "xmaxdate"),
                                 default_aes = ggplot2::aes(
                                   xmindate = NULL, xmaxdate = NULL),


                                 compute_group = function(data, scales, xmindate, xmaxdate) {
                                   #browser() # in StatTimeline
                                   if (!is.null(xmindate) & !is.null(data)) {
                                     data <- data %>% dplyr::filter(lubridate::year(lubridate::as_datetime(data$x, "1970-01-01 00:00.00 UTC")) >= xmindate)
                                   }
                                   if (!is.null(xmaxdate) & !is.null(data)) {
                                     data <- data %>% dplyr::filter(lubridate::year(lubridate::as_datetime(data$x, "1970-01-01 00:00.00 UTC")) <= xmaxdate)
                                   }
                                 }
)

#' Geom for creating a timeline from earthquake data
#'
#' This function creates a new geom which will create a linear timeline for a
#' specified time range, and will display each earthquake as a point on the timeline.
#'
#' This geom makes use of the GeomEarthquake ggproto object, inherits from the basic Geom.
#' The GeomEarthquake object defines default values for the optional
#' aesthetics color, size, and alpha.  A null ggproto opject is created if there are no
#' earthquakes in the desired time range.
#'
#' This geom is based on (but does not inherit from) the point geom (geom_point).  Many of the input
#' parameters are the same as for the geom_point.  Parameter descriptions for identical parameters
#' are taken from the geom_point help file to minimize confusion.
#'
#'
#'
#' A new ggproto object (GeomEarthquake) is created for this geom.  The ggproto object inherits from the basic Geom.
#' Default values are provided for the aesthetics size, linetype and alpha.  A null ggproto opject is created if there are no
#' earthquakes in the desired time range.
#'
#' This code is based on input from the Extending ggplot2 vignette:
#' https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

#'
#' @param mapping A set of aesthetic mappings created by aes or aes_.  If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mappint at the top level of the plot.  You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.  There are three options:
#'
#' If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#'
#' A data.frame, or other object, will override the plot data.  All objects will be fortified to produce a data frame.
#' See fortify for which variables will be created.
#'
#' A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param ... Other arguments passed on layer.  Thes are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3.
#' They may also be parameters to the paired geom/stat.
#' @param na.rm If FALSE, the default, missing values are mremoved with a warning.  If TRUE, missing values are silently removed.
#' @param show.legend logical.  Should this layer be included in the legends?  NA, the default, incldes if any aesthetics are mapped.  False never includes,
#' and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.  This is most usefule for helper functions that
#' define both data and aesthetics and shouldn't inherit behavior from the default plot specification, e.t. borders.
#' @param x Date of the earthquake (required)
#' @param y Factor indicating some striatification in which case multiple time lines will be ploted for each level of the factor (e.g. country).
#' @param xmindate Minimum year to display on the timeline
#' @param xmaxdate Maximum year to display on the timeline
#'
#' @return This function has no return value
#'
#' @importFrom ggplot2 ggplot layer
#'
#' @examples
#' \dontrun{geom_timeline(data = earthquakes_clean,
#'                        aes(x = DATE, y = COUNTRY,
#'                        colour = TOTAL_DEATHS, size = EQ_PRIMARY), alpha = 0.6,
#'                        xmindate = 2000, xmaxdate = 2017)}
#'
#' @export
#'
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "identity",
                          #stat = "StatTimeline",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          x = NULL, y = NULL,
                          #size = NULL, alpha = NA, fill = NA,
                          xmindate = NULL, xmaxdate = NULL,
                          ...) {

  ggplot2::layer(
    geom = GeomEarthquake,
    mapping = mapping,
    data = data,
    stat = StatTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(xmindate = xmindate,
                  xmaxdate = xmaxdate,
                  na.rm = na.rm, ...)
  )
}

GeomEarthquake <- ggplot2::ggproto("GeomEarthquake", ggplot2::Geom,
                                   required_aes = c("x"),
                                   optional_aes = c("y", "xmindate","xmaxdate"),
                                   default_aes = ggplot2::aes(
                                     y = NULL,
                                     shape = 19, colour = "black",
                                     fill = NA, alpha = 0.5,
                                     stroke = 0.5, lwd = 1,
                                     xmindate = NULL, xmaxdate = NULL
                                   ),

                                   draw_key = ggplot2::draw_key_point,

                                   setup_data = function(data, params) {
                                     #browser() # debugging GeomEarthquake
                                     if (!is.numeric(data$size)) {
                                       data <- data %>% dplyr::mutate(size = as.double(size))
                                     }
                                     data
                                   },

                                   draw_group = function(data, panel_scales, coord) {
                                     #browser()
                                     # return a nullGrob for GeomEarthquake if no point info
                                     n <- nrow(data)
                                     if (n < 1) return(grid::nullGrob())

                                     if (!is.null(data$y)) {
                                       ## set the y value for the points on the timeline to match the
                                       ## y axis location of the major tics
                                       ## and hence to line up with the tick annotation on the y axis
                                       data$timeline_yval <- panel_scales$y.major[data$y]
                                     } else {
                                       ## set the y value for the points on the timeline to be 20% of the way up the y axis
                                       data$timeline_yval <- 0.2
                                     }

                                     ## Transform the data first
                                     coords <- coord$transform(data, panel_scales)

                                     x <- panel_scales$x.range
                                     x_limits <- coord$transform(as.data.frame(x), panel_scales)

                                     timeline_points_grob <- grid::pointsGrob(
                                       coords$x, coords$timeline_yval,
                                       default.units = "native",
                                       pch = coords$shape,
                                       gp = grid::gpar(
                                         col = alpha(coords$colour, coords$alpha),
                                         fill = alpha(coords$fill, coords$alpha),
                                         fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                         lwd = coords$stroke * .stroke / 2
                                       )
                                     )

                                     timeline_line_grob <- grid::segmentsGrob(
                                       x0 = grid::unit(x_limits$x[1], "native"),
                                       y0 = grid::unit(data$timeline_yval[1], "native"),
                                       x1 = grid::unit(x_limits$x[2], "native"),
                                       y1 = grid::unit(data$timeline_yval[1], "native"),
                                       default.units = "native",
                                       gp = grid::gpar(
                                         col = "black",
                                         alpha = 1
                                       )
                                     )

                                     timeline_xaxis_grob <- grid::xaxisGrob(
                                       gp = grid::gpar(
                                         col = "black",
                                         alpha = 1,
                                         lwd = 2
                                       )
                                     )

                                     grid::grobTree(timeline_points_grob, timeline_line_grob, timeline_xaxis_grob)

                                   }
)

#' Stat for creating a timeline for earthquake data labels
#'
#' This function creates a new stat which will create a linear timeline for a
#' specified time range, and will label each earthquake (up to n_max) as a label
#' connected with a line to a point on the timeline.
#'
#' This code is based on input from the Extending ggplot2 vignette:
#' https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
#'
#' @param mapping A set of aesthetic mappings created by aes or aes_.  If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mappint at the top level of the plot.  You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.  There are three options:
#'
#' If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#'
#' A data.frame, or other object, will override the plot data.  All objects will be fortified to produce a data frame.
#' See fortify for which variables will be created.
#'
#' A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data.
#' @param geom The geom to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param ... Other arguments passed on layer.  Thes are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3.
#' They may also be parameters to the paired geom/stat.
#' @param na.rm If FALSE, the default, missing values are mremoved with a warning.  If TRUE, missing values are silently removed.
#' @param show.legend logical.  Should this layer be included in the legends?  NA, the default, incldes if any aesthetics are mapped.  False never includes,
#' and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.  This is most usefule for helper functions that
#' define both data and aesthetics and shouldn't inherit behavior from the default plot specification, e.t. borders.
#' @param x Date of the earthquake (required)
#' @param label Name of column to use for labels (required)
#' #@param y Factor indicating some striatification in which case multiple time lines will be ploted for each level of the factor (e.g. country).
#' @param xmindate Minimum year to display on the timeline
#' @param xmaxdate Maximum year to display on the timeline
#' @param n_max The maximum number of (largest) earthquakes to include on the timeline
#'
#' @return This function has no return value
#'
#' @importFrom ggplot2 ggproto layer Stat
#' @importFrom dplyr filter top_n
#'
#' @examples
#' \dontrun{stat_timeline_label(data = earthquakes_clean,
#'                        aes(x = DATE, label = LOCATION_NAME),
#'                        xmindate = 2000, xmaxdate = 2017, n_max = 5)}
#'
#' @keywords internal
#'
stat_timeline_label <- function(mapping = NULL, data = NULL, geom = "point",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE,
                                x = NULL, label = NULL,
                                xmindate = NULL, xmaxdate = NULL, n_max = NA,
                                ...) {
  ggplot2::layer(
    stat = StatTimelineLabel,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      xmindate = xmindate,
      xmaxdate = xmaxdate,
      n_max = n_max,
      na.rm = na.rm, ...)
  )
}

StatTimelineLabel <- ggplot2::ggproto("StatTimelineLabel", ggplot2::Stat,
                                      required_aes = c("x","label"),
                                      optional_aes = c("n_max", "xmindata", "xmaxdate"),
                                      default_aes = ggplot2::aes(
                                        xmindate = NULL, xmaxdate = NULL,
                                        n_max = NA),

                                      compute_group = function(data, scales, xmindate, xmaxdate, n_max) {
                                        #browser() # in StatTimeline
                                        if (!is.null(xmindate) & !is.null(data)) {
                                          data <- data %>% dplyr::filter(lubridate::year(lubridate::as_datetime(data$x, "1970-01-01 00:00.00 UTC")) >= xmindate)
                                        }
                                        if (!is.null(xmaxdate) & !is.null(data)) {
                                          data <- data %>% dplyr::filter(lubridate::year(lubridate::as_datetime(data$x, "1970-01-01 00:00.00 UTC")) <= xmaxdate)
                                        }
                                        #browser()
                                        # subset the data to include the largest n_max points by magnitude (size) if applicable
                                        if (!is.na(n_max)) {
                                          data <- data %>% dplyr::top_n(n_max, data$size)
                                        }
                                      }
)

#'
#' Geom for adding annotations to earthquake data displyed with geom_timeline
#'
#' This function creates a new geom which will add annotations to the earthquake data displayed by
#' the geom_timeline geom.  This geom adds a vertical line to each data point with a text annotation
#' (e.g. the location of the earthquake) attached to each line.  There is an option to subset
#' the data to n_max number of earthquakes, where only the n_max number of largest earthquakes (by magnitude)
#' are included in the subset.  Aesthetics include x, which is the date of the earthquake, and label, which
#' takes the column name from which annotations are obtained.
#'
#' This code is based on input from the Extending ggplot2 vignette:
#' https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
#'
#' @param mapping A set of aesthetic mappings created by aes or aes_.  If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mappint at the top level of the plot.  You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.  There are three options:
#'
#' If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#'
#' A data.frame, or other object, will override the plot data.  All objects will be fortified to produce a data frame.
#' See fortify for which variables will be created.
#'
#' A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param ... Other arguments passed on layer.  Thes are often aesthetics, used to set an aesthetic to a fixed value,
#' like color = "red" or size = 3.  They may also be parameters to the paired geom/stat.
#' @param na.rm If FALSE, the default, missing values are mremoved with a warning.  If TRUE, missing values are silently removed.
#' @param show.legend logical.  Should this layer be included in the legends?  NA, the default, incldes if any aesthetics are mapped.
#' False never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#' This is most useful for helper functions that
#' define both data and aesthetics and shouldn't inherit behavior from the default plot specification, e.t. borders.
#' @param x Date of the earthquake (required)
#' @param label Name of the column to be used for the annotation text.
#' @param n_max Number of largest earthquakes (by magnitude) that
#' are included in the subset of earthquakes that are annotated.
#' #@param nudge_x Horizontal offset for text from point
#' #@param nudge_y Vertical offset for text from point
#' #@param parse If TRUE, the labels will be parsed into expressions and displayed
#'   as described in ?plotmath
#' #@param check_overlap If TRUE, text that overlaps previous text in the same layer will not be plotted
#' @param xmindate Minimum year to display on the timeline
#' @param xmaxdate Maximum year to display on the timeline
#' @param magnitude Optional parameter to define the column to use for the earthquake magnitude
#'
#' @return This function has no return value
#'
#' @importFrom ggplot2 ggplot layer
#'
#' @examples
#' \dontrun{geom_timeline_label(data = earthquakes_clean,
#'                        aes(x = DATE, label = LOCATION_NAME)}
#'
#' @export
#'
geom_timeline_label <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                x = NULL,
                                label = NULL,
                                n_max = NULL, magnitude = NULL,
                                xmindate = NULL, xmaxdate = NULL,
                                ...) {

  ggplot2::layer(
    geom = GeomEarthquakeLabel,
    mapping = mapping,
    data = data,
    stat = StatTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      xmindate = xmindate,
      xmaxdate = xmaxdate,
      n_max = n_max,
      na.rm = na.rm,
      ...
    )
  )
}

GeomEarthquakeLabel <- ggplot2::ggproto("GeomEarthquakeLabel", ggplot2::Geom,
                                        required_aes = c("x", "label"),
                                        optional_aes = c("y", "n_max","magnitude"),
                                        default_aes = ggplot2::aes(
                                          x = NULL, y = NULL, label = NULL,
                                          n_max = NA,
                                          colour = "black", size = 3.88, angle = 45,
                                          xmindate = NULL, xmaxdate = NULL, magnitude = NULL
                                        ),
                                        draw_key = ggplot2::draw_key_blank,

                                        draw_panel = function(data, params, coord) {

                                          #browser()
                                          coords <- coord$transform(data, params)

                                          if (!is.null(coords$y)) {
                                            y_val <- coords$y
                                          } else {
                                            y_val <- 0.2
                                          }

                                          # this grob will add the line segment from each earthquake circle to the label
                                          timeline_label_line_grob <- grid::segmentsGrob(
                                            x0 = grid::unit(coords$x,"native"),
                                            y0 = grid::unit(y_val,"native"),
                                            x1 = grid::unit(coords$x,"native"),
                                            y1 = grid::unit(y_val + 0.07,"native"),
                                            gp = grid::gpar(col = "black", alpha = 0.5)
                                          )
                                          # this grob will place the text on the timeline
                                          timeline_label_text_grob <- grid::textGrob(
                                            label = coords$label,
                                            x = grid::unit(coords$x,"native"),
                                            y = grid::unit(y_val + 0.09,"native"),
                                            rot = 30,
                                            just = "left",
                                            gp = grid::gpar(
                                              fontsize = 8)
                                          )
                                          # use grobTree to combine the two grobs and add to the layer
                                          grid::grobTree(timeline_label_line_grob, timeline_label_text_grob)
                                        }

)
