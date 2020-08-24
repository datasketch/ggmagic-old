#' Bar Chart Categories Yea Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @param orientation Doesn't do anything for this type of chart.
#' @param order doesn't do anything
#' @inherit dsvizopts::dsviz_default_opts
#' @inheritDotParams dsvizopts::dsviz_default_opts
#' @section ctypes:
#' Cat-Yea-Num, Cat-Yea
#' @examples
#' gg_bar_CatYeaNum(sample_data("Cat-Yea-Num", nrow = 10))
#' @export
gg_bar_CatYeaNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts, family = "bar")
  l$d$b <- as.character(l$d$b)

  ggpos <- "stack"
  if(l$extra$graph_type == "grouped")
    ggpos <- position_dodge2(width = 0.9, preserve = "single")


  gg <- ggplot(l$d, aes(x = b, y = c, fill = a)) +
    geom_bar(stat = "identity", position = ggpos) +
    scale_fill_identity()  +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y,
         fill = l$titles$legend) +
    scale_y_continuous(labels = l$formats$f_nums) +
    scale_fill_manual(values=l$d$..colors, labels = l$formats$f_cat)

  if (l$orientation == "hor")
    gg <- gg + coord_flip()

  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)
 gg
}
