#' Treemap (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num, Yea-Num, Dat-Num,
#' @examples
#' gg_treemap_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_treemap_CatNum
gg_treemap_CatNum <- function(data = NULL,
                              agg = "sum",
                              agg_text = NULL,
                              caption = NULL,
                              colors = NULL,
                              color_opacity = "0.7",
                              color_scale ="discrete",
                              drop_na = FALSE,
                              drop_na_v = c(FALSE, FALSE),
                              graph_type = 'grouped',
                              group_color = 'transparent',
                              highlight_value = NULL,
                              highlight_valueColor = '#F9B233',
                              hor_label = NULL,
                              hor_line = NULL,
                              hor_line_label = NULL,
                              label_ratio = 1,
                              label_wrap = 12,
                              label_wrapV = c(12, 12),
                              lang = "es",
                              legend_position = "bottom",
                              legend_show = TRUE,
                              legend_title = NULL,
                              marks = c(".", ","),
                              n_digits = NULL,
                              n_digits_size = NULL,
                              n_digits_y = NULL,
                              n_digits_x = NULL,
                              order = NULL,
                              order1 = NULL,
                              order2 = NULL,
                              orientation = "ver",
                              percentage = FALSE,
                              prefix = NULL,
                              prefix_size = NULL,
                              prefix_x = NULL,
                              prefix_y = NULL,
                              regression = FALSE,
                              regression_color = '#d35400',
                              shape_type = 19,
                              sliceN = NULL,
                              sort = "no",
                              spline = FALSE,
                              start_zero = TRUE,
                              subtitle = NULL,
                              suffix = NULL,
                              suffix_X = NULL,
                              suffix_Y = NULL,
                              suffix_size = NULL,
                              text_color = "#5A6B72",
                              text_colorV = c("#FFFFFF", "#212428"),
                              text_show = TRUE,
                              text_size = 3,
                              text_size_v = c(15, 17),
                              theme = NULL,
                              title = NULL,
                              ver_label = NULL,
                              ver_line = NULL,
                              ver_line_label = NULL,
                              opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  defaultOptions <- list(
    agg = agg,
    agg_text = agg_text,
    caption = caption,
    colors = colors,
    color_opacity = color_opacity,
    color_scale = color_scale,
    drop_na = drop_na,
    drop_na_v = drop_na_v,
    graph_type = graph_type,
    group_color = group_color,
    highlight_value = highlight_value,
    highlight_valueColor = highlight_valueColor,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    label_ratio = label_ratio,
    label_wrap = label_wrap,
    label_wrap_v = label_wrap_v,
    lang = lang,
    legend_position = legend_position,
    legend_show = legend_show,
    legend_title = legend_title,
    marks = marks,
    n_digits = n_digits,
    n_digits_size = n_digits_size,
    n_digits_y = n_digits_y,
    n_digits_x = n_digits_x,
    order = order,
    order1 = order1,
    order2 = order2,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    prefix_size = prefix_size,
    prefix_x = prefix_x,
    prefix_y = prefix_y,
    regression = regression,
    regression_color = regression_color,
    shape_type = shape_type,
    slice_n = slice_n,
    sort = sort,
    spline = spline,
    start_zero = start_zero,
    subtitle = subtitle,
    suffix = suffix,
    suffix_x = suffix_x,
    suffix_y = suffix_y,
    suffix_size = suffix_size,
    text_color = text_color,
    text_color_v = text_color_v,
    text_show = text_show,
    text_size = text_size,
    text_size_v = text_size_v,
    theme = theme,
    title = title,
    ver_label = ver_label,
    ver_line = ver_line,
    ver_line_label = ver_line_label
  )
  opts <- modifyList(defaultOptions, opts %||% list())

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  opts$title <-  opts$title %||% ""
  opts$subtitle <- opts$subtitle %||% ""
  opts$caption <- opts$caption %||% ""

  if (opts$dropNa)
    d <- d %>%
    tidyr::drop_na()

  opts$nDigits <- ifelse(!is.null(opts$nDigits), opts$nDigits, 0)

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = ggmagic::agg(opts$agg, b))  %>%
    dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), opts$nDigits))

  d <- ggmagic::sortSlice(d, "b", "a", "ver", "desc", opts$sliceN)
  d <- ggmagic::orderCategory(d, "a", "ver", unique(d$a), opts$label_wrap)
  fillCol <- fillColors(data = d, "a", colors = opts$colors, opts$color_scale, opts$highlight_value, opts$highlight_valueColor, opts$label_wrap)

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  varP <- ifelse(opts$percentage, "percent", "b")

  gg <- ggplot(d, aes(area = c,
                     fill = a,
                     label =  paste0(d$a, "\n", paste0(opts$prefix,
                                                       format(d[[varP]],
                                                              big.mark = opts$marks[1],
                                                              decimal.mark = opts$marks[2],
                                                              digits = opts$nDigits),
                                                       opts$suffix)))) +
    treemapify::geom_treemap() +
    geom_treemap_text(min.size = 0, colour = ifelse(opts$text_show, opts$text_colorV[1], "transparent"), size = opts$text_sizeV[1]) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption) +
    scale_fill_manual(values = fillCol, name = opts$legend_title)

  if (is.null(opts$theme)) {
    gg <- gg + ggmagic::tma()
  } else {
    gg <- gg + opts$theme
  }

  gg +
    theme(legend.position = ifelse(opts$legend_show, opts$legend_position, "none"),
          plot.caption = element_text(hjust = 1)) +
    guides(fill=guide_legend(nrow = 1, byrow = TRUE)) +
    theme_leg()
}

#' Treemap (categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' gg_treemap_Cat(sampleData("Cat", nrow = 10))
#' @export gg_treemap_Cat
gg_treemap_Cat <-  function(data = NULL,
                            agg = "sum",
                            agg_text = NULL,
                            caption = NULL,
                            colors = NULL,
                            color_opacity = "0.7",
                            color_scale ="discrete",
                            drop_na = FALSE,
                            drop_na_v = c(FALSE, FALSE),
                            graph_type = 'grouped',
                            group_color = 'transparent',
                            highlight_value = NULL,
                            highlight_valueColor = '#F9B233',
                            hor_label = NULL,
                            hor_line = NULL,
                            hor_line_label = NULL,
                            label_ratio = 1,
                            label_wrap = 12,
                            label_wrapV = c(12, 12),
                            lang = "es",
                            legend_position = "bottom",
                            legend_show = TRUE,
                            legend_title = NULL,
                            marks = c(".", ","),
                            n_digits = NULL,
                            n_digits_size = NULL,
                            n_digits_y = NULL,
                            n_digits_x = NULL,
                            order = NULL,
                            order1 = NULL,
                            order2 = NULL,
                            orientation = "ver",
                            percentage = FALSE,
                            prefix = NULL,
                            prefix_size = NULL,
                            prefix_x = NULL,
                            prefix_y = NULL,
                            regression = FALSE,
                            regression_color = '#d35400',
                            shape_type = 19,
                            sliceN = NULL,
                            sort = "no",
                            spline = FALSE,
                            start_zero = TRUE,
                            subtitle = NULL,
                            suffix = NULL,
                            suffix_X = NULL,
                            suffix_Y = NULL,
                            suffix_size = NULL,
                            text_color = "#5A6B72",
                            text_colorV = c("#FFFFFF", "#212428"),
                            text_show = TRUE,
                            text_size = 3,
                            text_size_v = c(15, 17),
                            theme = NULL,
                            title = NULL,
                            ver_label = NULL,
                            ver_line = NULL,
                            ver_line_label = NULL,
                            opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  defaultOptions <- list(
    agg = agg,
    agg_text = agg_text,
    caption = caption,
    colors = colors,
    color_opacity = color_opacity,
    color_scale = color_scale,
    drop_na = drop_na,
    drop_na_v = drop_na_v,
    graph_type = graph_type,
    group_color = group_color,
    highlight_value = highlight_value,
    highlight_valueColor = highlight_valueColor,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    label_ratio = label_ratio,
    label_wrap = label_wrap,
    label_wrap_v = label_wrap_v,
    lang = lang,
    legend_position = legend_position,
    legend_show = legend_show,
    legend_title = legend_title,
    marks = marks,
    n_digits = n_digits,
    n_digits_size = n_digits_size,
    n_digits_y = n_digits_y,
    n_digits_x = n_digits_x,
    order = order,
    order1 = order1,
    order2 = order2,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    prefix_size = prefix_size,
    prefix_x = prefix_x,
    prefix_y = prefix_y,
    regression = regression,
    regression_color = regression_color,
    shape_type = shape_type,
    slice_n = slice_n,
    sort = sort,
    spline = spline,
    start_zero = start_zero,
    subtitle = subtitle,
    suffix = suffix,
    suffix_x = suffix_x,
    suffix_y = suffix_y,
    suffix_size = suffix_size,
    text_color = text_color,
    text_color_v = text_color_v,
    text_show = text_show,
    text_size = text_size,
    text_size_v = text_size_v,
    theme = theme,
    title = title,
    ver_label = ver_label,
    ver_line = ver_line,
    ver_line_label = ver_line_label
  )
  opts <- modifyList(defaultOptions, opts %||% list())

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label[1]))

  gg <- gg_treemap_CatNum(data = d, opts = opts)
  gg
}



#' Treemap (categories, categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num, Cat-Dat-Num,
#' @examples
#' gg_treemap_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export gg_treemap_CatCatNum
gg_treemap_CatCatNum <- function(data = NULL,
                                 agg = "sum",
                                 agg_text = NULL,
                                 caption = NULL,
                                 colors = NULL,
                                 color_opacity = "0.7",
                                 color_scale ="discrete",
                                 drop_na = FALSE,
                                 drop_na_v = c(FALSE, FALSE),
                                 graph_type = 'grouped',
                                 group_color = 'transparent',
                                 highlight_value = NULL,
                                 highlight_valueColor = '#F9B233',
                                 hor_label = NULL,
                                 hor_line = NULL,
                                 hor_line_label = NULL,
                                 label_ratio = 1,
                                 label_wrap = 12,
                                 label_wrapV = c(12, 12),
                                 lang = "es",
                                 legend_position = "bottom",
                                 legend_show = TRUE,
                                 legend_title = NULL,
                                 marks = c(".", ","),
                                 n_digits = NULL,
                                 n_digits_size = NULL,
                                 n_digits_y = NULL,
                                 n_digits_x = NULL,
                                 order = NULL,
                                 order1 = NULL,
                                 order2 = NULL,
                                 orientation = "ver",
                                 percentage = FALSE,
                                 prefix = NULL,
                                 prefix_size = NULL,
                                 prefix_x = NULL,
                                 prefix_y = NULL,
                                 regression = FALSE,
                                 regression_color = '#d35400',
                                 shape_type = 19,
                                 sliceN = NULL,
                                 sort = "no",
                                 spline = FALSE,
                                 start_zero = TRUE,
                                 subtitle = NULL,
                                 suffix = NULL,
                                 suffix_X = NULL,
                                 suffix_Y = NULL,
                                 suffix_size = NULL,
                                 text_color = "#5A6B72",
                                 text_colorV = c("#FFFFFF", "#212428"),
                                 text_show = TRUE,
                                 text_size = 3,
                                 text_size_v = c(15, 17),
                                 theme = NULL,
                                 title = NULL,
                                 ver_label = NULL,
                                 ver_line = NULL,
                                 ver_line_label = NULL,
                                 opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  defaultOptions <- list(
    agg = agg,
    agg_text = agg_text,
    caption = caption,
    colors = colors,
    color_opacity = color_opacity,
    color_scale = color_scale,
    drop_na = drop_na,
    drop_na_v = drop_na_v,
    graph_type = graph_type,
    group_color = group_color,
    highlight_value = highlight_value,
    highlight_valueColor = highlight_valueColor,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    label_ratio = label_ratio,
    label_wrap = label_wrap,
    label_wrap_v = label_wrap_v,
    lang = lang,
    legend_position = legend_position,
    legend_show = legend_show,
    legend_title = legend_title,
    marks = marks,
    n_digits = n_digits,
    n_digits_size = n_digits_size,
    n_digits_y = n_digits_y,
    n_digits_x = n_digits_x,
    order = order,
    order1 = order1,
    order2 = order2,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    prefix_size = prefix_size,
    prefix_x = prefix_x,
    prefix_y = prefix_y,
    regression = regression,
    regression_color = regression_color,
    shape_type = shape_type,
    slice_n = slice_n,
    sort = sort,
    spline = spline,
    start_zero = start_zero,
    subtitle = subtitle,
    suffix = suffix,
    suffix_x = suffix_x,
    suffix_y = suffix_y,
    suffix_size = suffix_size,
    text_color = text_color,
    text_color_v = text_color_v,
    text_show = text_show,
    text_size = text_size,
    text_size_v = text_size_v,
    theme = theme,
    title = title,
    ver_label = ver_label,
    ver_line = ver_line,
    ver_line_label = ver_line_label
  )
  opts <- modifyList(defaultOptions, opts %||% list())

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  opts$title <-  opts$title %||% ""
  opts$subtitle <- opts$subtitle %||% ""
  opts$caption <- opts$caption %||% ""

  if (any(opts$drop_naV))
    d <- d %>%
    tidyr::drop_na(which(opts$drop_naV))

  opts$nDigits <- ifelse(!is.null(opts$nDigits), opts$nDigits, 0)

  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = ggmagic::agg(opts$agg, c))  %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(percent = round(c * 100 / sum(c, na.rm = TRUE), opts$nDigits)) %>%
    drop_na(c)

  d <- ggmagic::orderCategory(d, "a", "ver", unique(d$a), opts$label_wrapV[1])
  d <- ggmagic::orderCategory(d, "b", "ver", unique(d$b), opts$label_wrapV[2])
  fillCol <- ggmagic::fillColors(d, "a", opts$colors, opts$color_scale, NULL, NULL, opts$label_wrap)

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  varP <- ifelse(opts$percentage, "percent", "c")

  gg <- ggplot(d, aes(area = c, fill = a, subgroup = b, label =  paste0(d$a, "\n", paste0(opts$prefix,
                                                                                          format(d[[varP]],
                                                                                                 big.mark = opts$marks[1],
                                                                                                 decimal.mark = opts$marks[2],
                                                                                                 digits = opts$nDigits),
                                                                                          opts$suffix)))) +
    treemapify::geom_treemap() +
    geom_treemap_subgroup_border(color = opts$group_color) +
    geom_treemap_subgroup_text(place = "topleft",  colour = opts$text_colorV[2], min.size = 0, reflow = T, size = opts$text_sizeV[2]) +
    geom_treemap_text(colour = opts$text_colorV[1], place = "bottomleft", min.size = 0, size = opts$text_sizeV[2]) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption) +
    scale_fill_manual(values = fillCol, name = opts$legend_title)

  if (is.null(opts$theme)) {
    gg <- gg + ggmagic::tma()
  } else {
    gg <- gg + opts$theme
  }

  gg +
    theme(legend.position = ifelse(opts$legend_show, opts$legend_position, "none"),
          plot.caption = element_text(hjust = 1)) +
    guides(fill=guide_legend(nrow = 1, byrow = TRUE)) +
    theme_leg()
}



#' Treemap (categories, categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Cat-Cat, Cat-Yea, Cat-Dat,
#' @examples
#' gg_treemap_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export gg_treemap_CatCat
gg_treemap_CatCat <- function(data = NULL,
                              agg = "sum",
                              agg_text = NULL,
                              caption = NULL,
                              colors = NULL,
                              color_opacity = "0.7",
                              color_scale ="discrete",
                              drop_na = FALSE,
                              drop_na_v = c(FALSE, FALSE),
                              graph_type = 'grouped',
                              group_color = 'transparent',
                              highlight_value = NULL,
                              highlight_valueColor = '#F9B233',
                              hor_label = NULL,
                              hor_line = NULL,
                              hor_line_label = NULL,
                              label_ratio = 1,
                              label_wrap = 12,
                              label_wrapV = c(12, 12),
                              lang = "es",
                              legend_position = "bottom",
                              legend_show = TRUE,
                              legend_title = NULL,
                              marks = c(".", ","),
                              n_digits = NULL,
                              n_digits_size = NULL,
                              n_digits_y = NULL,
                              n_digits_x = NULL,
                              order = NULL,
                              order1 = NULL,
                              order2 = NULL,
                              orientation = "ver",
                              percentage = FALSE,
                              prefix = NULL,
                              prefix_size = NULL,
                              prefix_x = NULL,
                              prefix_y = NULL,
                              regression = FALSE,
                              regression_color = '#d35400',
                              shape_type = 19,
                              sliceN = NULL,
                              sort = "no",
                              spline = FALSE,
                              start_zero = TRUE,
                              subtitle = NULL,
                              suffix = NULL,
                              suffix_X = NULL,
                              suffix_Y = NULL,
                              suffix_size = NULL,
                              text_color = "#5A6B72",
                              text_colorV = c("#FFFFFF", "#212428"),
                              text_show = TRUE,
                              text_size = 3,
                              text_size_v = c(15, 17),
                              theme = NULL,
                              title = NULL,
                              ver_label = NULL,
                              ver_line = NULL,
                              ver_line_label = NULL,
                              opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  defaultOptions <- list(
    agg = agg,
    agg_text = agg_text,
    caption = caption,
    colors = colors,
    color_opacity = color_opacity,
    color_scale = color_scale,
    drop_na = drop_na,
    drop_na_v = drop_na_v,
    graph_type = graph_type,
    group_color = group_color,
    highlight_value = highlight_value,
    highlight_valueColor = highlight_valueColor,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    label_ratio = label_ratio,
    label_wrap = label_wrap,
    label_wrap_v = label_wrap_v,
    lang = lang,
    legend_position = legend_position,
    legend_show = legend_show,
    legend_title = legend_title,
    marks = marks,
    n_digits = n_digits,
    n_digits_size = n_digits_size,
    n_digits_y = n_digits_y,
    n_digits_x = n_digits_x,
    order = order,
    order1 = order1,
    order2 = order2,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    prefix_size = prefix_size,
    prefix_x = prefix_x,
    prefix_y = prefix_y,
    regression = regression,
    regression_color = regression_color,
    shape_type = shape_type,
    slice_n = slice_n,
    sort = sort,
    spline = spline,
    start_zero = start_zero,
    subtitle = subtitle,
    suffix = suffix,
    suffix_x = suffix_x,
    suffix_y = suffix_y,
    suffix_size = suffix_size,
    text_color = text_color,
    text_color_v = text_color_v,
    text_show = text_show,
    text_size = text_size,
    text_size_v = text_size_v,
    theme = theme,
    title = title,
    ver_label = ver_label,
    ver_line = ver_line,
    ver_line_label = ver_line_label
  )
  opts <- modifyList(defaultOptions, opts %||% list())

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label[1]))

  gg <- gg_treemap_CatCatNum(data = d, opts = opts)
  gg
}



