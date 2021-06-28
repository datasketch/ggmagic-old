#' Bar Chart Date Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @param orientation Doesn't do anything for this type of chart.
#' @inherit dsvizopts::dsviz_default_opts
#' @inheritDotParams dsvizopts::dsviz_default_opts
#' @section ctypes:
#' Dat-Num, Yea-Num
#' @examples
#' gg_bar_DatNum(sample_data("Dat-Num", nrow = 10))
#' @export
gg_bar_DatNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts, ftype = "Dat-Num", plot = "bar")
  print(l$d)
  gg <- ggplot(l$d, aes(x = a, y = b, fill = ..colors )) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y) +
    scale_y_continuous(labels = l$formats$f_nums) +
    scale_x_date(labels = l$formats$f_dats)
  if (l$dataLabels$show) {
    labpos <- d$..labpos
    gg <- gg + geom_text(aes(y = labpos,
                             label = l$dataLabels$f_nums(b)),
                         check_overlap = TRUE,
                         size = l$dataLabels$size,
                         color = l$dataLabels$color)
  }
  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)

}
