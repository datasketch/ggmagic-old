#' #' Treemap (categories, numbers)
#' #'
#' #' Compare aggregations among category's levels
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Cat-Num, Yea-Num, Dat-Num,
#' #' @examples
#' #' gg_treemap_CatNum(sampleData("Cat-Num", nrow = 10))
#' #' @export gg_treemap_CatNum
#' gg_treemap_CatNum <- function(data = NULL,
#'                               agg = "sum",
#'                               agg_text = NULL,
#'                               caption = NULL,
#'                               colors = NULL,
#'                               color_scale ="discrete",
#'                               drop_na = FALSE,
#'                               highlight_value = NULL,
#'                               highlight_value_color = '#F9B233',
#'                               label_wrap = 12,
#'                               marks = c(".", ","),
#'                               n_digits = NULL,
#'                               order = NULL,
#'                               percentage = FALSE,
#'                               prefix = NULL,
#'                               slice_n = NULL,
#'                               sort = "no",
#'                               subtitle = NULL,
#'                               suffix = NULL,
#'                               text_color = "#5A6B72",
#'                               text_show = TRUE,
#'                               text_size = 3,
#'                               theme = NULL,
#'                               title = NULL,
#'                               opts = NULL, ...) {
#'
#'   if (is.null(data)) {
#'     stop("Load an available dataset")
#'   }
#'
#'   defaultOptions <- list(
#'     agg = agg,
#'     agg_text = agg_text,
#'     caption = caption,
#'     colors = colors,
#'     color_scale = color_scale,
#'     drop_na = drop_na,
#'     highlight_value = highlight_value,
#'     highlight_value_color = highlight_value_color,
#'     label_wrap = label_wrap,
#'     marks = marks,
#'     n_digits = n_digits,
#'     order = order,
#'     percentage = percentage,
#'     prefix = prefix,
#'     slice_n = slice_n,
#'     sort = sort,
#'     subtitle = subtitle,
#'     suffix = suffix,
#'     text_color = text_color,
#'     text_show = text_show,
#'     text_size = text_size,
#'     theme = theme,
#'     title = title
#'   )
#'   opts <- modifyList(defaultOptions, opts %||% list())
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   opts$title <-  opts$title %||% ""
#'   opts$subtitle <- opts$subtitle %||% ""
#'   opts$caption <- opts$caption %||% ""
#'
#'   options(scipen = 9999)
#'
#'   if (opts$drop_na)
#'     d <- d %>%
#'     tidyr::drop_na()
#'
#'   opts$n_digits <- ifelse(!is.null(opts$n_digits), opts$n_digits, 0)
#'
#'   d <- d  %>%
#'     tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
#'                            b = NA)) %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(b = ggmagic::agg(opts$agg, b))  %>%
#'     dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), opts$n_digits))
#'
#'   d <- ggmagic::sortSlice(d, "b", "a", "ver", "desc", opts$slice_n)
#'   d <- ggmagic::orderCategory(d, "a", "ver", unique(d$a), opts$label_wrap)
#'
#'   colores_plot <- opts$colors
#'   if (!is.null(opts$theme$colors)) colores_plot <- opts$theme$colors
#'   fillCol <- fillColors(data = d, "a", colors = colores_plot, opts$color_scale, opts$highlight_value, opts$highlight_value_color, opts$label_wrap)
#'
#'   if (opts$percentage & is.null(opts$suffix)) {
#'     opts$suffix <- "%"
#'   }
#'
#'   label_size <- opts$text_size
#'   if (!is.null(opts$theme$labsData_sizeLabel)) label_size <- as.numeric(gsub("px", "",opts$theme$labsData_sizeLabel))/3
#'
#'   label_color <- opts$text_color
#'   if (!is.null(opts$theme$labsData_colLabel)) label_color <-  opts$theme$labsData_colLabel
#'
#'   varP <- ifelse(opts$percentage, "percent", "b")
#'
#'   gg <- ggplot(d, aes(area = b,
#'                      fill = a,
#'                      label =  paste0(d$a, "\n", paste0(opts$prefix,
#'                                                        format(d[[varP]],
#'                                                               big.mark = opts$marks[1],
#'                                                               decimal.mark = opts$marks[2],
#'                                                               digits = opts$n_digits),
#'                                                        opts$suffix)))) +
#'     treemapify::geom_treemap() +
#'     scale_fill_manual(values = fillCol) +
#'      geom_treemap_text(size = label_size * 5, colour = ifelse(opts$text_show, label_color, "transparent")) +
#'      labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption)
#'
#'   theme_user <- opts$theme
#'   optsTheme <- list( colors = opts$colors, background = opts$background)
#'   themeCustom <- modifyList(optsTheme, theme_user %||% list())
#'   gg <- gg + ggmagic::tma(custom = themeCustom, orientation = opts$orientation)
#'
#'   gg +
#'     theme(legend.position = "none",
#'           plot.caption = element_text(hjust = 1)) +
#'     theme_leg()
#' }
#'
#' #' Treemap (categories)
#' #'
#' #' Compare aggregations among category's levels
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Cat, Yea, Dat
#' #' @examples
#' #' gg_treemap_Cat(sampleData("Cat", nrow = 10))
#' #' @export gg_treemap_Cat
#' gg_treemap_Cat <-  function(data = NULL,
#'                             agg_text = NULL,
#'                             caption = NULL,
#'                             colors = NULL,
#'                             color_scale ="discrete",
#'                             drop_na = FALSE,
#'                             highlight_value = NULL,
#'                             highlight_value_color = '#F9B233',
#'                             label_wrap = 12,
#'                             marks = c(".", ","),
#'                             n_digits = NULL,
#'                             order = NULL,
#'                             percentage = FALSE,
#'                             prefix = NULL,
#'                             slice_n = NULL,
#'                             sort = "no",
#'                             subtitle = NULL,
#'                             suffix = NULL,
#'                             text_color = "#5A6B72",
#'                             text_show = TRUE,
#'                             text_size = 3,
#'                             theme = NULL,
#'                             title = NULL,
#'                             opts = NULL, ...) {
#'
#'   if (is.null(data)) {
#'     stop("Load an available dataset")
#'   }
#'
#'   defaultOptions <- list(
#'     agg_text = agg_text,
#'     caption = caption,
#'     colors = colors,
#'     color_scale = color_scale,
#'     drop_na = drop_na,
#'     highlight_value = highlight_value,
#'     highlight_value_color = highlight_value_color,
#'     label_wrap = label_wrap,
#'     marks = marks,
#'     n_digits = n_digits,
#'     order = order,
#'     percentage = percentage,
#'     prefix = prefix,
#'     slice_n = slice_n,
#'     sort = sort,
#'     subtitle = subtitle,
#'     suffix = suffix,
#'     text_color = text_color,
#'     text_show = text_show,
#'     text_size = text_size,
#'     theme = theme,
#'     title = title
#'   )
#'   opts <- modifyList(defaultOptions, opts %||% list())
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   d <- d %>%
#'     dplyr::group_by_all() %>%
#'     dplyr::summarise(b = n())
#'
#'   prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
#'   names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label[1]))
#'
#'   gg <- gg_treemap_CatNum(data = d, opts = opts)
#'   gg
#' }
#'
#'
#'
#' #' Treemap (categories, categories, numbers)
#' #'
#' #' Compare aggregations among category's levels
#' #'
#' #' @param data A data.frame
#' #' @return Ggplot visualization
#' #' @section ctypes:
#' #' Cat-Cat-Num, Cat-Yea-Num, Cat-Dat-Num,
#' #' @examples
#' #' gg_treemap_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' #' @export gg_treemap_CatCatNum
#' gg_treemap_CatCatNum <- function(data = NULL,
#'                                  agg = "sum",
#'                                  agg_text = NULL,
#'                                  caption = NULL,
#'                                  colors = NULL,
#'                                  color_scale ="discrete",
#'                                  drop_na = FALSE,
#'                                  drop_na_legend = FALSE,
#'                                  group_color = 'transparent',
#'                                  highlight_value = NULL,
#'                                  highlight_value_color = '#F9B233',
#'                                  label_wrap = 12,
#'                                  label_wrap_legend = 12,
#'                                  legend_position = "bottom",
#'                                  legend_show = TRUE,
#'                                  legend_title = NULL,
#'                                  marks = c(".", ","),
#'                                  n_digits = NULL,
#'                                  order1 = NULL,
#'                                  order2 = NULL,
#'                                  percentage = FALSE,
#'                                  prefix = NULL,
#'                                  slice_n = NULL,
#'                                  sort = "no",
#'                                  subtitle = NULL,
#'                                  suffix = NULL,
#'                                  text_color_v = c("#FFFFFF", "#212428"),
#'                                  text_position_v = c( "bottomleft", "topleft"),
#'                                  text_show_v = c(TRUE, TRUE),
#'                                  text_size_v = c(15, 17),
#'                                  theme = NULL,
#'                                  title = NULL,
#'                                  opts = NULL, ...) {
#'
#'   if (is.null(data)) {
#'     stop("Load an available dataset")
#'   }
#'
#'   defaultOptions <- list(
#'     agg = agg,
#'     agg_text = agg_text,
#'     caption = caption,
#'     colors = colors,
#'     color_scale = color_scale,
#'     drop_na = drop_na,
#'     drop_na_legend = drop_na_legend,
#'     group_color = group_color,
#'     highlight_value = highlight_value,
#'     highlight_value_color = highlight_value_color,
#'     label_wrap = label_wrap,
#'     label_wrap_legend = label_wrap_legend,
#'     legend_position = legend_position,
#'     legend_show = legend_show,
#'     legend_title = legend_title,
#'     marks = marks,
#'     n_digits = n_digits,
#'     order1 = order1,
#'     order2 = order2,
#'     percentage = percentage,
#'     prefix = prefix,
#'     slice_n = slice_n,
#'     sort = sort,
#'     subtitle = subtitle,
#'     suffix = suffix,
#'     text_color_v = text_color_v,
#'     text_position_v = text_position_v,
#'     text_show_v = text_show_v,
#'     text_size_v = text_size_v,
#'     theme = theme,
#'     title = title
#'   )
#'   opts <- modifyList(defaultOptions, opts %||% list())
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   opts$title <-  opts$title %||% ""
#'   opts$subtitle <- opts$subtitle %||% ""
#'   opts$caption <- opts$caption %||% ""
#'
#'   options(scipen = 9999)
#'
#'   if (opts$drop_na)
#'     d <- d %>%
#'     tidyr::drop_na(b)
#'
#'   if(opts$drop_na_legend)
#'     d <- d %>%
#'     tidyr::drop_na(a)
#'
#'   opts$n_digits <- ifelse(!is.null(opts$n_digits), opts$n_digits, 0)
#'
#'   d <- d %>%
#'     tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
#'                            b = ifelse(is.character(d$b), "NA", NA),
#'                            c = NA)) %>%
#'     dplyr::group_by(a, b) %>%
#'     dplyr::summarise(c = ggmagic::agg(opts$agg, c))  %>%
#'     dplyr::group_by(b) %>%
#'     dplyr::mutate(percent = round(c * 100 / sum(c, na.rm = TRUE), opts$n_digits)) %>%
#'     drop_na(c)
#'
#'   d <- ggmagic::orderCategory(d, "a", opts$orientation, opts$order1, opts$label_wrap_legend)
#'   d <- ggmagic::orderCategory(d, "b", opts$orientation, opts$order2, opts$label_wrap)
#'   colores_plot <- opts$colors
#'   if (!is.null(opts$theme$colors)) colores_plot <- opts$theme$colors
#'   fillCol <- ggmagic::fillColors(d, "b", colores_plot, opts$color_scale, NULL, NULL, opts$label_wrap)
#'
#'   if (opts$percentage & is.null(opts$suffix)) {
#'     opts$suffix <- "%"
#'   }
#'
#'   varP <- ifelse(opts$percentage, "percent", "c")
#'
#'   gg <- ggplot(d, aes(area = c, fill = b, subgroup = b, label =  paste0(d$a, "\n", paste0(opts$prefix,
#'                                                                                           format(d[[varP]],
#'                                                                                                  big.mark = opts$marks[1],
#'                                                                                                  decimal.mark = opts$marks[2],
#'                                                                                                  digits = opts$n_digits),
#'                                                                                           opts$suffix)))) +
#'     treemapify::geom_treemap() +
#'     geom_treemap_subgroup_border(color = opts$group_color) +
#'     labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption) +
#'     scale_x_continuous(expand = c(0, 0)) +
#'     scale_y_continuous(expand = c(0, 0))  +
#'     scale_fill_manual(values = fillCol, name = opts$legend_title)
#'
#'   if (text_show_v[1]) {
#'     gg <- gg +  geom_treemap_text(colour = opts$text_color_v[1], place = text_position_v[1], min.size = 0, size = opts$text_size_v[2])
#'   }
#'
#'
#'   if (text_show_v[2]) {
#'    gg <- gg + geom_treemap_subgroup_text(place = text_position_v[2],  colour = opts$text_color_v[2], min.size = 0, reflow = T, size = opts$text_size_v[2])
#'   }
#'
#'   theme_user <- opts$theme
#'   optsTheme <- list( colors = opts$colors, background = opts$background)
#'   themeCustom <- modifyList(optsTheme, theme_user %||% list())
#'   gg <- gg + ggmagic::tma(custom = themeCustom)
#'
#'   gg +
#'     theme(legend.position = ifelse(opts$legend_show, opts$legend_position, "none"),
#'           plot.caption = element_text(hjust = 1)) +
#'     guides(fill=guide_legend(nrow = 1, byrow = TRUE)) +
#'     theme_leg()
#' }
#'
#'
#'
#' #' Treemap (categories, categories)
#' #'
#' #' Compare aggregations among category's levels
#' #'
#' #' @param data A data.frame
#' #' @return Ggplot visualization
#' #' @section ctypes:
#' #' Cat-Cat, Cat-Yea, Cat-Dat,
#' #' @examples
#' #' gg_treemap_CatCat(sampleData("Cat-Cat", nrow = 10))
#' #' @export gg_treemap_CatCat
#' gg_treemap_CatCat <- function(data = NULL,
#'                               agg_text = NULL,
#'                               caption = NULL,
#'                               colors = NULL,
#'                               color_scale ="discrete",
#'                               drop_na = FALSE,
#'                               drop_na_legend = FALSE,
#'                               group_color = 'transparent',
#'                               highlight_value = NULL,
#'                               highlight_value_color = '#F9B233',
#'                               label_wrap = 12,
#'                               label_wrap_legend = 12,
#'                               legend_position = "bottom",
#'                               legend_show = TRUE,
#'                               legend_title = NULL,
#'                               marks = c(".", ","),
#'                               n_digits = NULL,
#'                               order1 = NULL,
#'                               order2 = NULL,
#'                               percentage = FALSE,
#'                               prefix = NULL,
#'                               slice_n = NULL,
#'                               sort = "no",
#'                               subtitle = NULL,
#'                               suffix = NULL,
#'                               text_color_v = c("#FFFFFF", "#212428"),
#'                               text_position_v = c( "bottomleft", "topleft"),
#'                               text_show_v = c(TRUE, TRUE),
#'                               text_size_v = c(15, 17),
#'                               theme = NULL,
#'                               title = NULL,
#'                               opts = NULL, ...) {
#'
#'   if (is.null(data)) {
#'     stop("Load an available dataset")
#'   }
#'   defaultOptions <- list(
#'     agg_text = agg_text,
#'     caption = caption,
#'     colors = colors,
#'     color_scale = color_scale,
#'     drop_na = drop_na,
#'     drop_na_legend = drop_na_legend,
#'     group_color = group_color,
#'     highlight_value = highlight_value,
#'     highlight_value_color = highlight_value_color,
#'     label_wrap = label_wrap,
#'     label_wrap_legend = label_wrap_legend,
#'     legend_position = legend_position,
#'     legend_show = legend_show,
#'     legend_title = legend_title,
#'     marks = marks,
#'     n_digits = n_digits,
#'     order1 = order1,
#'     order2 = order2,
#'     percentage = percentage,
#'     prefix = prefix,
#'     slice_n = slice_n,
#'     sort = sort,
#'     subtitle = subtitle,
#'     suffix = suffix,
#'     text_color_v = text_color_v,
#'     text_position_v = text_position_v,
#'     text_show_v = text_show_v,
#'     text_size_v = text_size_v,
#'     theme = theme,
#'     title = title
#'   )
#'   opts <- modifyList(defaultOptions, opts %||% list())
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   d <- d %>%
#'     dplyr::group_by_all() %>%
#'     dplyr::summarise(c = n())
#'
#'   prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
#'   names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label[1]))
#'
#'   gg <- gg_treemap_CatCatNum(data = d, opts = opts)
#'   gg
#' }
#'
#'
#'
