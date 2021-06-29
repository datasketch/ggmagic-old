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
gg_area_CatYeaNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  data[[1]] <- as_Cat(data[[1]])
  data[[2]] <- as_Cat(data[[2]])
  data[[3]] <- as_Num(data[[3]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- ggmagic_prep(data, opts, ftype = "Cat-Yea-Num", plot = "area")

  position <- "identity"
  if(opts$extra$graph_type == "stacked") position <- "stack"

  gg <- ggplot(l$d, aes(x = b, y = value, color = a, fill = a, group = a)) +
    geom_area(alpha = l$extra$area_alpha, position = position) +
    #scale_color_identity() +
    scale_color_manual(values = unique(l$d$..colors)) +
    scale_fill_manual(values = unique(l$d$..colors)) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y,
         fill = l$titles$legend) +
    scale_y_continuous(labels = l$formats$f_nums) +
    guides(color = FALSE)

  if (l$dataLabels$show) {
    if (position == "identity") {
      gg <- gg +
        geom_text(
          aes(label = l$dataLabels$f_nums(l$d$value), y = l$d$value + 0.05),
          position = position_dodge(0.9),
          vjust = 0,
          check_overlap = TRUE,
          size = l$dataLabels$size,
          color = l$dataLabels$color
        )
    } else {
      gg <- gg + geom_text(label = l$dataLabels$f_nums(l$d$value),
                           position = position_stack(vjust = 0.5),
                           check_overlap = TRUE,
                           size = l$dataLabels$size,
                           color = l$dataLabels$color)
    }
  }
  gg <- gg + add_ggmagic_theme(opts$theme)
  add_branding_bar(gg, opts$theme)

}
