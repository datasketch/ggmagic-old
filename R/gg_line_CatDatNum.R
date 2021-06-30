#' Line Chart Categories Date Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @param orientation Doesn't do anything for this type of chart.
#' @param order doesn't do anything
#' @inherit dsvizopts::dsviz_default_opts
#' @inheritDotParams dsvizopts::dsviz_default_opts
#' @section ctypes:
#' Dat-Num, Yea-Num
#' @examples
#' gg_line_CatDatNum(sample_data("Cat-Dat-Num", nrow = 10))
#' @export
gg_line_CatDatNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  data[[1]] <- as_Cat(data[[1]])
  data[[2]] <- as_Dat(data[[2]])
  data[[3]] <- as_Num(data[[3]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- ggmagic_prep(data, opts, ftype = "Cat-Dat-Num", plot = "line")
  default_theme <- c(opts$theme, orientation = l$orientation)

  gg <- ggplot(l$d, aes(x = b, y = value, color = a, group = a)) +
    geom_line() +
    #scale_color_identity() +
    scale_color_manual(values = unique(l$d$..colors)) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y,
         colour = l$titles$legend) +
    scale_y_continuous(labels = l$formats$f_nums) +
    scale_x_date(labels = l$formats$f_dats)

  if (l$dataLabels$show) {
    gg <- gg +
      geom_text(
        aes(label = l$dataLabels$f_nums(l$d$value), y = l$d$value + 0.05),
        position = position_dodge(0.9),
        vjust = 0,
        check_overlap = TRUE,
        size = l$dataLabels$size,
        color = l$dataLabels$color
      )
  }
  gg <- gg + add_ggmagic_theme(default_theme)
  add_branding_bar(gg, opts$theme)

}
