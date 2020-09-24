#' Area Chart Datr Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @param orientation Doesn't do anything for this type of chart.
#' @param order doesn't do anything
#' @inherit dsvizopts::dsviz_default_opts
#' @inheritDotParams dsvizopts::dsviz_default_opts
#' @section ctypes:
#' Dat-Num, Dat-Num
#' @examples
#' gg_area_DatNum(sample_data("Cat-Num", nrow = 10))
#' @export
gg_area_DatNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts)

  gg <- ggplot(l$d, aes(x = a, y = b, color = ..colors, fill = ..colors, group = 1)) +
    geom_line(alpha = l$extra$area_alpha) +
    scale_color_identity() +
    scale_fill_identity() +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y) +
    scale_y_continuous(labels = l$formats$f_nums) +
    scale_x_date(labels = l$formats$f_dats)
  if (l$dataLabels$show) {
    gg <- gg + geom_text(aes(y = labPos,
                             label = l$dataLabels$f_nums(b)),
                         check_overlap = TRUE,
                         size = l$dataLabels$size,
                         color = l$dataLabels$color)
  }
  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)

}

#' Area Chart Cat
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @examples
#' gg_area_Dat(sample_data("Dat", nrow = 10))
#' @export
gg_area_Dat <- gg_area_DatNum
