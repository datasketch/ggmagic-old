#' Treemap Chart Categories Categories Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @param orientation Doesn't do anything for this type of chart.
#' @param order doesn't do anything
#' @inherit dsvizopts::dsviz_default_opts
#' @inheritDotParams dsvizopts::dsviz_default_opts
#' @section ctypes:
#' Cat-Cat-Num, Cat-Cat
#' @examples
#' gg_treemap_CatCatNum(sample_data("Cat-Yea-Num", nrow = 10))
#' @export
gg_treemap_CatCatNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts, plot =  "treemap", ftype = "Cat-Cat-Num")

  gg <- ggplot(l$d, aes(area = c, fill = a, label = b,
                        subgroup = a)) +
    treemapify::geom_treemap() +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         fill = l$titles$legend) +
    scale_fill_manual(values=l$d$..colors, labels = l$formats$f_cat)

  if (l$dataLabels$show) {
    gg <- gg + treemapify::geom_treemap_text(
      label = paste0(l$d$b,  "\n", l$dataLabels$f_nums(l$d$c)),
                                 colour = l$dataLabels$color,
                                 size = l$dataLabels$size*5)
  }

  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)
}

#' treemap Chart Cat Cat
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @examples
#' gg_treemap_CatCat(sample_data("Cat-Cat", nrow = 10))
#' @export
gg_treemap_CatCat <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts, plot =  "treemap", ftype = "Cat-Cat")

  gg <- ggplot(l$d, aes(area = c, fill = a, label = b,
                        subgroup = a)) +
    treemapify::geom_treemap() +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         fill = l$titles$legend) +
    scale_fill_manual(values=l$d$..colors, labels = l$formats$f_cat)

  if (l$dataLabels$show) {
    gg <- gg + treemapify::geom_treemap_text(
      label = paste0(l$d$b,  "\n", l$dataLabels$f_nums(l$d$c)),
      colour = l$dataLabels$color,
      size = l$dataLabels$size*5)
  }

  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)
}
