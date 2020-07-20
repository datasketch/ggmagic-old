#' Area Chart Categories Yea Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @param orientation Doesn't do anything for this type of chart.
#' @param order doesn't do anything
#' @inherit dsvizopts::dsviz_default_opts
#' @inheritDotParams dsvizopts::dsviz_default_opts
#' @section ctypes:
#' Yea-Num, Yea-Num
#' @examples
#' gg_area_CatYea(sample_data("Cat-Num", nrow = 10))
#' @export
gg_area_CatDatNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts, family = "area")
  position <- "identity"
  if(opts$extra$graph_type == "stacked") position <- "stack"

  gg <- ggplot(l$d, aes(x = b, y = c, color = a, fill = a, group = a)) +
    geom_area(alpha = l$extra$area_alpha, position = position) +
    #scale_color_identity() +
    scale_color_manual(values = unique(l$d$..colors)) +
    scale_fill_manual(values = unique(l$d$..colors)) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y,
         fill = l$titles$color) +
    scale_y_continuous(labels = l$formats$f_nums) +
    scale_x_date(labels = l$formats$f_dats) +
    guides(color = FALSE)

  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)

}
