#' Pie (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_pie_CatNum(sample_data("Cat-Num", nrow = 10))
#' @export gg_pie_CatNum
gg_pie_CatNum <- function(data = NULL,
                          agg = "sum",
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
    agg = agg,
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

  opts$title <-  opts$title %||% ""
  opts$subtitle <- opts$subtitle %||% ""
  opts$caption <- opts$caption %||% ""

  if (opts$drop_na)
    d <- d %>%
    tidyr::drop_na()

  opts$n_digits <- ifelse(!is.null(opts$n_digits), opts$n_digits, 0)

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = ggmagic::agg(opts$agg, b)) %>%
    dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), opts$n_digits))

  d <- ggmagic::sortSlice(d, "b", "a", "ver", opts$sort, opts$sliceN)
  d <- ggmagic::orderCategory(d, "a", "ver", opts$order, opts$label_wrap)
  colores_plot <- opts$colors
  if (!is.null(opts$theme$colors)) colores_plot <- opts$theme$colors
  fillCol <- ggmagic::fillColors(d, "a", colores_plot, opts$color_scale, NULL, NULL, opts$label_wrap)

  d$ct <- cumsum(d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)]) -
    d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)] / 2
  d <- d %>%
    dplyr::mutate(b = ifelse(b == 0, NA, b),
                  percent = ifelse(percent == 0, NA, percent))

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  label_size <- opts$text_size
  if (!is.null(opts$theme$labsData_sizeLabel)) label_size <- as.numeric(gsub("px", "",opts$theme$labsData_sizeLabel))/3

  label_color <- opts$text_color
  if (!is.null(opts$theme$labsData_colLabel)) label_color <-  opts$theme$labsData_colLabel

  gg <- ggplot(d, aes(x = 1, y = b, fill = a)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "y") +
    geom_text(aes(y = ct,
                  x = opts$label_ratio,
                  label = paste0(opts$prefix,
                                 format(d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)],
                                        big.mark = opts$marks[1],
                                        decimal.mark = opts$marks[2],
                                        digits = opts$n_digits,
                                        nsmall = opts$n_digits),
                                 opts$suffix)),
              check_overlap = TRUE,
              size = label_size,
              color = ifelse(opts$text_show, label_color, "transparent")) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, x = "", y = "") +
    scale_fill_manual(values = fillCol, name = opts$legend_title)


  theme_user <- opts$theme
  optsTheme <- list( colors = opts$colors, background = opts$background)
  themeCustom <- modifyList(optsTheme, theme_user %||% list())
  gg <- gg + ggmagic::tma(custom = themeCustom)+ theme_ds_clean()


  gg +
    theme_leg() +
    theme(legend.position = opts$legend_position,
          plot.caption = element_text(hjust = 1),
          plot.background = element_rect(fill = opts$background, colour = opts$background),
          panel.background = element_rect(fill = opts$background, colour =opts$background)) #+
    #guides(fill = guide_legend(nrow = 1))
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
gg_pie_Cat <- function(data = NULL,
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

  gg <- gg_pie_CatNum(data = d, opts = opts, ...)
  gg
}



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
                            agg = "sum",
                            background = "transparent",
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
    agg = agg,
    agg_text = agg_text,
    background = background,
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

  opts$title <-  opts$title %||% ""
  opts$subtitle <- opts$subtitle %||% ""
  opts$caption <- opts$caption %||% ""

  if (opts$drop_na)
    d <- d %>%
    tidyr::drop_na()

  opts$n_digits <- ifelse(!is.null(opts$n_digits), opts$n_digits, 0)

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = ggmagic::agg(opts$agg, b)) %>%
    dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), opts$n_digits))

  d <- ggmagic::sortSlice(d, "b", "a", "ver", opts$sort, opts$sliceN)
  d <- ggmagic::orderCategory(d, "a", "ver", opts$order, opts$label_wrap)
  colores_plot <- opts$colors
  if (!is.null(opts$theme$colors)) colores_plot <- opts$theme$colors
  fillCol <- ggmagic::fillColors(d, "a", colores_plot, opts$color_scale, NULL, NULL, opts$label_wrap)

  d$ct <- cumsum(d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)]) -
    d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)] / 2
  d <- d %>%
    dplyr::mutate(b = ifelse(b == 0, NA, b),
                  percent = ifelse(percent == 0, NA, percent))

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  label_size <- opts$text_size
  if (!is.null(opts$theme$labsData_sizeLabel)) label_size <- as.numeric(gsub("px", "",opts$theme$labsData_sizeLabel))/3

  label_color <- opts$text_color
  if (!is.null(opts$theme$labsData_colLabel)) label_color <-  opts$theme$labsData_colLabel

  gg <- ggplot(d, aes(x = 1, y = b, fill = a)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "y") +
    xlim(c(-0.5, 1.5)) +
    geom_text(aes(y = ct,
                  x = opts$label_ratio,
                  label = paste0(opts$prefix,
                                 format(d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)],
                                        big.mark = opts$marks[1],
                                        decimal.mark = opts$marks[2],
                                        digits = opts$n_digits,
                                        nsmall = opts$n_digits),
                                 opts$suffix)),
              check_overlap = TRUE,
              size = label_size,
              color = ifelse(opts$text_show, label_color, "transparent")) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, x = "", y = "") +
    scale_fill_manual(values = fillCol, name = opts$legend_title)

  theme_user <- opts$theme
  optsTheme <- list( colors = opts$colors, background = opts$background)
  themeCustom <- modifyList(optsTheme, theme_user %||% list())
  gg <- gg + ggmagic::tma(custom = themeCustom) + theme_ds_clean()


  gg +
    theme_leg() +
    theme(legend.position = opts$legend_position,
          plot.caption = element_text(hjust = 1),
          plot.background = element_rect(fill = opts$background, colour = opts$background),
          panel.background = element_rect(fill = opts$background, colour = opts$background)) #+
    #guides(fill = guide_legend(nrow = 1))
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
