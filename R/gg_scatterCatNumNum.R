#' Scatter Numeric Numeric
#'
#' This scatter
#'
#' @param data A data.frame
#' @param orientation Doesn't do anything for this type of chart.
#' @param order doesn't do anything
#' @inherit dsvizopts::dsviz_default_opts
#' @inheritDotParams dsvizopts::dsviz_default_opts
#' @section ctypes:
#' Dat-Num, Yea-Num
#' @examples
#' gg_scatter_CatNumNum(sample_data("Cat-Num-Num", nrow = 10))
#' @export
gg_scatter_CatNumNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  data[[1]] <- as_Cat(data[[1]])
  data[[2]] <- as_Num(data[[2]])
  data[[3]] <- as_Num(data[[3]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- ggmagic_prep(data, opts, plot = "scatter", ftype = "Cat-Num-Num")

  gg <- ggplot(l$d, aes(x = value_x, y = value_y, color = value_cat, group = value_cat)) +
    geom_point(size = 3) +
    scale_color_manual(values = l$d$..colors) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y,
         colour = l$titles$legend) +
    scale_y_continuous(labels = l$formats$f_nums$x) +
    scale_x_continuous(labels = l$formats$f_nums$y)

  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)

}
