#' treemap Chart Categories Yea Numeric
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
#' gg_treemap_CatYeaNum(sample_data("Cat-Yea-Num", nrow = 10))
#' @export
gg_treemap_CatYeaNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts, family = "treemap")
  l$d$b <- as.character(l$d$b)


  gg <- ggplot(l$d, aes(area = c, fill = a, label = b,
                  subgroup = a)) +
    geom_treemap() +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         fill = l$titles$legend) +
    scale_fill_manual(values=l$d$..colors, labels = l$formats$f_cat)

  if (l$dataLabels$show) {
    gg <- gg + treemapify::geom_treemap_text(colour = l$dataLabels$color %||% "#2b2b2b",
                                 size = l$dataLabels$size %||% 11)
  }

  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_treemap(gg, opts$theme)
}

#' treemap Chart Cat Yea
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @examples
#' gg_treemap_CatYea(sample_data("Cat-Yea", nrow = 10))
#' @export
gg_treemap_CatYea <- gg_treemap_CatYeaNum
