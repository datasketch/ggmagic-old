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
#' gg_pie_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_pie_CatNum
gg_pie_CatNum <- function(data = NULL,
                          ...) {

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts, extra_pattern = "pie")

  gg <- ggplot(l$d, aes(x = 1, y = b, fill = a)) +
    geom_bar(width = 1, stat = "identity", color = l$theme$background_color) +
    scale_fill_manual(l$titles$x,values = l$d$..colors) +
    # xlim(c(0.5, 1.5)) +
    coord_polar(theta = "y") +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption)
  if(l$dataLabels$show){
    gg <- gg + geom_text(aes(y = ..ylabpos,
                             label = l$dataLabels$f_nums(l$d$b)),
                         x = l$extra$pie_dataLabels_pos + 0.5,
                         size = l$dataLabels$size %||% 0.3 * l$theme$text_size,
                         color = l$dataLabels$color %||% l$theme$text_color,
                         check_overlap = TRUE)
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
#' gg_pie_Cat(sampleData("Cat", nrow = 10))
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
#' gg_donut_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_donut_CatNum

gg_donut_CatNum <- function(data = NULL,
                            ...) {

  if (is.null(data)) stop("need dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  l <- ggmagic_prep(data, opts, extra_pattern = "donut")

  # Calculate width
  #dwidth <- -1/(d$x) + 1.5
  l$extra$donut_width <- ifelse(l$extra$donut_width < 0.01, 0.01, l$extra$donut_width)
  x_donut_min <- -1/(l$extra$donut_width) + 1.5

  d <- l$d
  gg <- ggplot(d, aes(x = 1, y = b, fill = a)) +
    geom_bar(width = 1, stat = "identity", color = l$theme$background_color) +
    scale_fill_manual(l$titles$x, values = d$..colors) +
    xlim(x_donut_min, 1.5) +
    coord_polar(theta = "y") +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption)
  if(l$dataLabels$show){
    gg <- gg + geom_text(aes(y = ..ylabpos,
                             label = l$dataLabels$f_nums(l$d$b)),
                         x = l$extra$donut_dataLabels_pos + 0.5,
                         size = l$dataLabels$size %||% 0.3 * l$theme$text_size,
                         color = l$dataLabels$color %||% l$theme$text_color,
                         check_overlap = TRUE)
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
#' gg_donut_Cat(sampleData("Cat", nrow = 10))
#' @export gg_donut_Cat
gg_donut_Cat <- function(data = NULL,
                         agg_text = NULL,
                         caption = NULL,
                         colors = NULL,
                         color_scale ="discrete",
                         drop_na = FALSE,
                         highlight_value = NULL,
                         highlight_value_color = '#F9B233',
                         hor_label = NULL,
                         hor_line = NULL,
                         label_ratio = 1,
                         label_wrap = 12,
                         legend_position = "bottom",
                         legend_show = TRUE,
                         legend_title = NULL,
                         marks = c(".", ","),
                         n_digits = NULL,
                         order = NULL,
                         orientation = "ver",
                         percentage = FALSE,
                         prefix = NULL,
                         slice_n = NULL,
                         sort = "no",
                         subtitle = NULL,
                         suffix = NULL,
                         text_color = "#5A6B72",
                         text_show = TRUE,
                         text_size = 3,
                         theme = NULL,
                         title = NULL,
                         opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  defaultOptions <- list(
    agg_text = agg_text,
    caption = caption,
    colors = colors,
    color_scale = color_scale,
    drop_na = drop_na,
    highlight_value = highlight_value,
    highlight_value_color = highlight_value_color,
    label_ratio = label_ratio,
    label_wrap = label_wrap,
    legend_position = legend_position,
    legend_show = legend_show,
    legend_title = legend_title,
    marks = marks,
    n_digits = n_digits,
    order = order,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    slice_n = slice_n,
    sort = sort,
    subtitle = subtitle,
    suffix = suffix,
    text_color = text_color,
    text_show = text_show,
    text_size = text_size,
    theme = theme,
    title = title
  )
  opts <- modifyList(defaultOptions, opts %||% list())

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label))

  gg <- gg_donut_CatNum(data = d, opts = opts, ...)
  gg
}
