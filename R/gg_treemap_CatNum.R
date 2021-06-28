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

  data[[1]] <- as_Cat(data[[1]])
  data[[2]] <- as_Num(data[[2]])

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts, extra_pattern = ".", plot =  "treemap", ftype = "Cat-Num")

  gg <- ggplot(l$d, aes(area = value, fill = a, label = a)) +
    treemapify::geom_treemap() +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption) +
    scale_fill_manual(values=l$d$..colors, labels = l$formats$f_cat)

  if (l$dataLabels$show) {
   gg <- gg + treemapify::geom_treemap_text(label=l$dataLabels$f_nums(l$d$value),
                                            size = l$dataLabels$size*3,
                                            colour = l$dataLabels$color)
  }

  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)
}

#' treemap Chart Cat
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @examples
#' gg_treemap_Cat(sample_data("Cat", nrow = 10))
#' @export
gg_treemap_Cat <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")

  data[[1]] <- as_Cat(data[[1]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- ggmagic_prep(data, opts, plot =  "treemap", ftype = "Cat")

  gg <- ggplot(l$d, aes(area = value, fill = a, label = a)) +
    treemapify::geom_treemap() +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption) +
    scale_fill_manual(values=l$d$..colors, labels = l$formats$f_cat)

  if (l$dataLabels$show) {
    gg <- gg + treemapify::geom_treemap_text(label=l$dataLabels$f_nums(l$d$value),
                                             size = l$dataLabels$size*3,
                                             colour = l$dataLabels$color)
  }

  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)
}

