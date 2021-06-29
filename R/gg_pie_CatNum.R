#' Pie (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' Color by, does not work, defaults to Cat var
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_pie_CatNum(sample_data("Cat-Num", nrow = 10))
#' @export gg_pie_CatNum
gg_pie_CatNum <- function(data = NULL,
                          ...) {

  if (is.null(data)) stop("need dataset to visualize")
  data[[1]] <- homodatum::as_Cat(data[[1]])
  data[[2]] <- homodatum::as_Num(data[[2]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- ggmagic_prep(data, opts, extra_pattern = "pie", ftype = "Cat-Num")

  gg <- ggplot(l$d, aes(x = 1, y = value, fill = a)) +
    geom_bar(width = 1, stat = "identity", color = l$theme$background_color) +
    scale_fill_manual(l$titles$x,values = l$d$..colors) +
    coord_polar(theta = "y") +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption)
  if(l$dataLabels$show){
    gg <- gg +  geom_text(aes(#y =..ylabpos,
                              label = l$dataLabels$f_nums(value)),
                          position = position_stack(vjust = 0.5),
                          check_overlap = TRUE,
                          size = l$dataLabels$size,
                          color = l$dataLabels$color)
  }

  gg <- gg + add_ggmagic_theme_clean(opts$theme)
  add_branding_bar(gg, opts$theme)

}


#' Pie (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' gg_pie_Cat(sample_data("Cat", nrow = 10))
#' @export gg_pie_Cat
gg_pie_Cat <- gg_pie_CatNum


#' Donut (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_donut_CatNum(sample_data("Cat-Num", nrow = 10))
#' @export gg_donut_CatNum

gg_donut_CatNum <- function(data = NULL,
                            ...) {

  if (is.null(data)) stop("need dataset to visualize")
  data[[1]] <- homodatum::as_Cat(data[[1]])
  data[[2]] <- homodatum::as_Num(data[[2]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- ggmagic_prep(data, opts, extra_pattern = "donut", ftype = "Cat-Num")

  # Calculate width
  #dwidth <- -1/(d$x) + 1.5
  l$extra$donut_width <- ifelse(l$extra$donut_width < 0.01, 0.01, l$extra$donut_width)
  x_donut_min <- -1/(l$extra$donut_width) + 1.5

  d <- l$d
  gg <- ggplot(d, aes(x = 1, y = value, fill = a)) +
    geom_bar(width = 1, stat = "identity", color = l$theme$background_color) +
    scale_fill_manual(l$titles$x, values = d$..colors) +
    xlim(x_donut_min, 1.5) +
    coord_polar(theta = "y") +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption)
  if(l$dataLabels$show){
    gg <- gg +  geom_text(aes(#y =..ylabpos,
      label = l$dataLabels$f_nums(value)),
      position = position_stack(vjust = 0.5),
      check_overlap = TRUE,
      size = l$dataLabels$size,
      color = l$dataLabels$color)
  }

  gg <- gg + add_ggmagic_theme_clean(opts$theme)
  add_branding_bar(gg, opts$theme)

}


#' Donut (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' gg_donut_Cat(sample_data("Cat", nrow = 10))
#' @export gg_pie_Cat
gg_donut_Cat <- gg_donut_CatNum
