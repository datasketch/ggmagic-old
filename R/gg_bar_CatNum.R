#' Bar Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @inherit dsvizopts::dsviz_default_opts
#' @inheritDotParams dsvizopts::dsviz_default_opts
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' gg_bar_CatNum(sample_data("Cat-Num", nrow = 10))
#' @export
gg_bar_CatNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  data[[1]] <- homodatum::as_Cat(data[[1]])
  data[[2]] <- homodatum::as_Num(data[[2]])
  #check_fonts(opts$theme)
  opts <- dsvizopts::merge_dsviz_options(...)
  l <- ggmagic_prep(data, opts, extra_pattern = ".", plot =  "bar", ftype = "Cat-Num")
  default_theme <- c(opts$theme, orientation = l$orientation)
  d <- l$d

  gg <- ggplot(l$d, aes(x = a, y = value, fill = ..colors )) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y) +
    scale_y_continuous(labels = l$formats$f_nums, n.breaks = nrow(d)) +
    scale_x_discrete(labels = l$formats$f_cats, limits = d$a)

  if (l$dataLabels$show) {
    gg <- gg + geom_text(
      aes(label = l$dataLabels$f_nums(d$value), y = d$value + 0.05),
      position = position_dodge(0.9),
      vjust = 0,
      check_overlap = TRUE,
      size = l$dataLabels$size,
      color = l$dataLabels$color
    )

  }

    #scale_x_discrete(labels = l$formats$f_cats)

  if (l$orientation == "hor")
    gg <- gg + coord_flip()

  gg <- gg + add_ggmagic_theme(default_theme)
  add_branding_bar(gg, opts$theme)

}

#' Bar Chart Cat
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @examples
#' gg_bar_Cat(sample_data("Cat", nrow = 10))
#' @export
gg_bar_Cat <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")

  data[[1]] <- homodatum::as_Cat(data[[1]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- ggmagic_prep(data, opts, extra_pattern = ".", plot =  "bar", ftype = "Cat")
  default_theme <- c(opts$theme, orientation = l$orientation)

  d <- l$d

  gg <- ggplot(l$d, aes(x = a, y = value, fill = ..colors )) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y) +
    scale_y_continuous(labels = l$formats$f_nums) +
    scale_x_discrete(labels = l$formats$f_cats, limits = d$a)

  if (l$dataLabels$show) {
    gg <- gg + geom_text(
      aes(label = l$dataLabels$f_nums(d$value), y = d$value + 0.05),
      position = position_dodge(0.9),
      vjust = 0,
      check_overlap = TRUE,
      size = l$dataLabels$size,
      color = l$dataLabels$color
    )

  }

  #scale_x_discrete(labels = l$formats$f_cats)

  if (l$orientation == "hor")
    gg <- gg + coord_flip()

  gg <- gg + add_ggmagic_theme(default_theme)
  add_branding_bar(gg, opts$theme)

}

