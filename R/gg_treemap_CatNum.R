#' treemap Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @inherit dsvizopts::dsviz_default_opts
#' @inheritDotParams dsvizopts::dsviz_default_opts
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' gg_treemap_CatNum(sample_data("Cat-Num", nrow = 10))
#' @export
gg_treemap_CatNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts)

  gg <- ggplot(l$d, aes(area = b, fill = a, label = a)) +
    treemapify::geom_treemap() +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption) +
    scale_fill_manual(values=l$d$..colors, labels = l$formats$f_cat)

  if (l$dataLabels$show) {
   gg <- gg + geom_treemap_text(colour = l$dataLabels$color %||% "#2b2b2b",
                                size = l$dataLabels$size %||% 11)
  }

  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)
}
