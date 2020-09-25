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
#' gg_scatter_CatNumNumNum(sample_data("Cat-Num-Num-Num", nrow = 10))
#' @export
gg_scatter_CatNumNumNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts, family = "scatter")

  gg <- ggplot(l$d, aes(x = b, y = c, color = a, group = a, size = d)) +
    geom_point() +
    scale_color_manual(values = unique(l$d$..colors)) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y,
         colour = l$titles$legend,
         size = " ") +
    scale_y_continuous(labels = l$formats$f_CatNums) +
    scale_x_continuous(labels = l$formats$f_CatNums)

  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)

}
