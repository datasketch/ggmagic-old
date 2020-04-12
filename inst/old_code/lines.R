#' #' Lines (categories, numbers)
#' #'
#' #' Compare aggregations among category's levels
#' #'
#' #' @param data A data.frame
#' #' @return Ggplot2 visualization
#' #' @section ctypes:
#' #' Cat-Num, Dat-Num, Yea-Num
#' #' @examples
#' #' gg_line_CatNum(sampleData("Cat-Num", nrow = 10))
#' #' @export gg_line_CatNum
#' gg_line_CatNum <- function(data = NULL,
#'                            agg = "sum",
#'                            agg_text = NULL,
#'                            caption = NULL,
#'                            colors = NULL,
#'                            drop_na = FALSE,
#'                            hor_label = NULL,
#'                            hor_line = NULL,
#'                            label_ratio = 1,
#'                            label_wrap = 12,
#'                            marks = c(".", ","),
#'                            n_digits = NULL,
#'                            order = NULL,
#'                            orientation = "ver",
#'                            percentage = FALSE,
#'                            prefix = NULL,
#'                            shape_size = 3,
#'                            shape_type = 19,
#'                            slice_n = NULL,
#'                            sort = "no",
#'                            start_zero = TRUE,
#'                            subtitle = NULL,
#'                            suffix = NULL,
#'                            text_color = "#5A6B72",
#'                            text_show = TRUE,
#'                            text_size = 3,
#'                            theme = NULL,
#'                            title = NULL,
#'                            ver_label = NULL,
#'                            ver_line = NULL,
#'                            opts = NULL, ...) {
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
#'     drop_na = drop_na,
#'     hor_label = hor_label,
#'     hor_line = hor_line,
#'     label_ratio = label_ratio,
#'     label_wrap = label_wrap,
#'     marks = marks,
#'     n_digits = n_digits,
#'     order = order,
#'     orientation = orientation,
#'     percentage = percentage,
#'     prefix = prefix,
#'     shape_size = shape_size,
#'     shape_type = shape_type,
#'     slice_n = slice_n,
#'     sort = sort,
#'     start_zero = start_zero,
#'     subtitle = subtitle,
#'     suffix = suffix,
#'     text_color = text_color,
#'     text_show = text_show,
#'     text_size = text_size,
#'     theme = theme,
#'     title = title,
#'     ver_label = ver_label,
#'     ver_line = ver_line
#'   )
#'   opts <- modifyList(defaultOptions, opts %||% list())
#'   options(scipen = 9999)
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   opts$title <-  opts$title %||% ""
#'   opts$subtitle <- opts$subtitle %||% ""
#'   opts$caption <- opts$caption %||% ""
#'
#'   Lc <- length(unique(d$a))
#'   angleText <- ifelse( Lc >= 10 & Lc < 15,
#'                        45,
#'                        ifelse(Lc >= 15, 90, 0))
#'
#'   prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)
#'
#'   labelsXY <- ggmagic::orientationXY(opts$orientation,
#'                                      x = nms[1],
#'                                      y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(prefix_agg, nms[2])),
#'                                      hor = opts$horLabel,
#'                                      ver = opts$verLabel)
#'   lineXY <- ggmagic::orientationXY(opts$orientation,
#'                                    0,
#'                                    0,
#'                                    hor = opts$horLine,
#'                                    ver = opts$verLine)
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
#'     dplyr::summarise(b = ggmagic::agg(opts$agg, b)) %>%
#'     dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), opts$n_digits))
#'
#'   # d$a <- as.character(d$a)
#'   # d$a[is.na(d$a)] <- "NA"
#'
#'   d <- ggmagic::sortSlice(d, "b", "a", opts$orientation, opts$sort, opts$slice_n)
#'   d <- ggmagic::orderCategory(d, "a", opts$orientation, opts$order, opts$label_wrap)
#'   d <- ggmagic::labelPosition(d, "b", opts$label_ratio, opts$percentage)
#'
#'   colores_plot <- opts$colors
#'   if (!is.null(opts$theme$colors)) colores_plot <- opts$theme$colors
#'   fillCol <- ggmagic::fillColors(d, "a", colores_plot, "no", opts$highlight_value, opts$highlight_value_color, opts$label_wrap)
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
#'   minLim <- ifelse(min(d[[varP]], na.rm = T) < 0, min(d[[varP]], na.rm = T), 0)
#'   maxLim <- ceiling(max(d[[varP]], na.rm = T) + 0.1 * max(d[[varP]], na.rm = T))
#'
#'   gg <- ggplot(d, aes(x = a, y = d[[varP]], colour = a, group = 1)) +
#'     geom_line() +
#'     geom_point(shape = as.integer(opts$shape_type)) +
#'     geom_vline(xintercept = lineXY[2],
#'                color = ifelse((opts$orientation == "hor" & !is.null(opts$horLine)) | (opts$orientation == "ver" & !is.null(opts$verLine)),
#'                               "black",
#'                               "transparent"),
#'                linetype = "dashed") +
#'     geom_hline(yintercept = lineXY[1],
#'                color = ifelse((opts$orientation == "hor" & !is.null(opts$verLine)) | (opts$orientation == "ver" & !is.null(opts$horLine)),
#'                               "black",
#'                               "transparent"),
#'                linetype = "dashed") +
#'     geom_text(aes(y = labPos,
#'                   label = paste0(opts$prefix,
#'                                  format(d[[varP]],
#'                                         big.mark = opts$marks[1],
#'                                         decimal.mark = opts$marks[2],
#'                                         digits = opts$n_digits,
#'                                         nsmall = opts$n_digits),
#'                                  opts$suffix)),
#'               check_overlap = TRUE,
#'               size = label_size,
#'               color = ifelse(opts$text_show, label_color, "transparent")) +
#'     labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, x = labelsXY[1], y = labelsXY[2]) +
#'     scale_color_manual(values = fillCol) +
#'     scale_y_continuous(labels =  function(x) paste0(opts$prefix,
#'                                                     format(x,
#'                                                            big.mark = opts$marks[1],
#'                                                            decimal.mark = opts$marks[2],
#'                                                            digits = opts$n_digits,
#'                                                            nsmall = opts$n_digits),
#'                                                     opts$suffix),
#'                        #breaks = seq(ifelse(opts$start_zero, 0, minLim), maxLim, round(maxLim/Lc, 2)),
#'                        limits = c(ifelse(opts$start_zero, 0, minLim), maxLim))#c(ifelse(start_zero, 0, NA), NA))
#'
#'   if (opts$orientation == "hor") {
#'     gg <- gg +
#'       coord_flip()
#'   }
#'
#'   theme_user <- opts$theme
#'   optsTheme <- list( colors = opts$colors, background = opts$background)
#'   themeCustom <- modifyList(optsTheme, theme_user %||% list())
#'   gg <- gg + ggmagic::tma(custom = themeCustom, orientation = opts$orientation)
#'
#'   gg <- gg + theme(legend.position = "none",
#'                    plot.caption = element_text(hjust = 1),
#'                    axis.text.x = element_text(angle = angleText))
#'   gg
#' }
#'
#'
#' #' Lines (categories)
#' #'
#' #' Compare category's levels
#' #'
#' #' @param data A data.frame
#' #' @return Ggplot2 visualization
#' #' @section ctypes:
#' #' Cat
#' #' @examples
#' #' gg_line_Cat(sampleData("Cat", nrow = 10))
#' #' @export gg_line_Cat
#' gg_line_Cat <- function(data = NULL,
#'                         agg_text = NULL,
#'                         caption = NULL,
#'                         colors = NULL,
#'                         drop_na = FALSE,
#'                         hor_label = NULL,
#'                         hor_line = NULL,
#'                         label_ratio = 1,
#'                         label_wrap = 12,
#'                         marks = c(".", ","),
#'                         n_digits = NULL,
#'                         order = NULL,
#'                         orientation = "ver",
#'                         percentage = FALSE,
#'                         prefix = NULL,
#'                         shape_size = 3,
#'                         shape_type = 19,
#'                         slice_n = NULL,
#'                         sort = "no",
#'                         start_zero = TRUE,
#'                         subtitle = NULL,
#'                         suffix = NULL,
#'                         text_color = "#5A6B72",
#'                         text_show = TRUE,
#'                         text_size = 3,
#'                         theme = NULL,
#'                         title = NULL,
#'                         ver_label = NULL,
#'                         ver_line = NULL,
#'                         opts = NULL, ...) {
#'
#'   if (is.null(data)) {
#'     stop("Load an available dataset")
#'   }
#'
#'   defaultOptions <- list(
#'     agg_text = agg_text,
#'     caption = caption,
#'     colors = colors,
#'     drop_na = drop_na,
#'     hor_label = hor_label,
#'     hor_line = hor_line,
#'     label_ratio = label_ratio,
#'     label_wrap = label_wrap,
#'     marks = marks,
#'     n_digits = n_digits,
#'     order = order,
#'     orientation = orientation,
#'     percentage = percentage,
#'     prefix = prefix,
#'     shape_size = shape_size,
#'     shape_type = shape_type,
#'     slice_n = slice_n,
#'     sort = sort,
#'     start_zero = start_zero,
#'     subtitle = subtitle,
#'     suffix = suffix,
#'     text_color = text_color,
#'     text_show = text_show,
#'     text_size = text_size,
#'     theme = theme,
#'     title = title,
#'     ver_label = ver_label,
#'     ver_line = ver_line
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
#'   prefix_agg <- ifelse(is.null(opts$agg_text), "Count ", opts$agg_text)
#'   names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label))
#'
#'   gg <- gg_line_CatNum(data = d, opts = opts)
#'   gg
#' }
#'
#'
#'
#' #' Lines (categories, ordered categories, numbers)
#' #'
#' #' Compare quantities among two categories
#' #'
#' #' @param data A data.frame
#' #' @return Ggplot2 visualization
#' #' @section ctypes:
#' #' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' #' @examples
#' #' gg_line_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' #' @export gg_line_CatCatNum
#' gg_line_CatCatNum <- function(data = NULL,
#'                               agg = "sum",
#'                               agg_text = NULL,
#'                               caption = NULL,
#'                               colors = NULL,
#'                               color_opacity = 0.7,
#'                               color_scale ="discrete",
#'                               drop_na = FALSE,
#'                               drop_na_legend = FALSE,
#'                               graph_type = 'grouped',
#'                               group_color = 'transparent',
#'                               highlight_value = NULL,
#'                               highlight_value_color = '#F9B233',
#'                               hor_label = NULL,
#'                               hor_line = NULL,
#'                               hor_line_label = NULL,
#'                               label_ratio = 1,
#'                               label_wrap = 12,
#'                               label_wrap_legend = 12,
#'                               legend_position = "bottom",
#'                               legend_show = TRUE,
#'                               legend_title = NULL,
#'                               marks = c(".", ","),
#'                               n_digits = NULL,
#'                               order1 = NULL,
#'                               order2 = NULL,
#'                               orientation = "ver",
#'                               percentage = FALSE,
#'                               prefix = NULL,
#'                               shape_type = 19,
#'                               slice_n = NULL,
#'                               sort = "no",
#'                               spline = FALSE,
#'                               start_zero = TRUE,
#'                               subtitle = NULL,
#'                               suffix = NULL,
#'                               text_color = "#212428",
#'                               text_show = TRUE,
#'                               text_size = 3,
#'                               theme = NULL,
#'                               title = NULL,
#'                               ver_label = NULL,
#'                               ver_line = NULL,
#'                               ver_line_label = NULL,
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
#'     color_opacity = color_opacity,
#'     color_scale = color_scale,
#'     drop_na = drop_na,
#'     drop_na_legend = drop_na_legend,
#'     graph_type = graph_type,
#'     group_color = group_color,
#'     highlight_value = highlight_value,
#'     highlight_value_color = highlight_value_color,
#'     hor_label = hor_label,
#'     hor_line = hor_line,
#'     hor_line_label = hor_line_label,
#'     label_ratio = label_ratio,
#'     label_wrap = label_wrap,
#'     label_wrap_legend = label_wrap_legend,
#'     legend_position = legend_position,
#'     legend_show = legend_show,
#'     legend_title = legend_title,
#'     marks = marks,
#'     n_digits = n_digits,
#'     order = order,
#'     orientation = orientation,
#'     percentage = percentage,
#'     prefix = prefix,
#'     shape_type = shape_type,
#'     slice_n = slice_n,
#'     sort = sort,
#'     spline = spline,
#'     start_zero = start_zero,
#'     subtitle = subtitle,
#'     suffix = suffix,
#'     text_color = text_color,
#'     text_show = text_show,
#'     text_size = text_size,
#'     theme = theme,
#'     title = title,
#'     ver_label = ver_label,
#'     ver_line = ver_line,
#'     ver_line_label = ver_line_label
#'   )
#'   opts <- modifyList(defaultOptions, opts %||% list())
#'   options(scipen = 9999)
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   Lc <- length(unique(d$a))
#'   angleText <- ifelse( Lc >= 10 & Lc < 15,
#'                        45,
#'                        ifelse(Lc >= 15, 90, 0))
#'
#'   opts$title <-  opts$title %||% ""
#'   opts$subtitle <- opts$subtitle %||% ""
#'   opts$caption <- opts$caption %||% ""
#'   opts$legend_title <- opts$legend_title %||% nms[1]
#'
#'   prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, as.character(opts$agg_text))
#'
#'   labelsXY <- ggmagic::orientationXY(opts$orientation,
#'                                      x = nms[2],
#'                                      y = ifelse(nrow(d) == dplyr::n_distinct(d$a) & nrow(d) == dplyr::n_distinct(d$b),
#'                                                 nms[3],
#'                                                 paste(prefix_agg, nms[3])),
#'                                      hor = opts$horLabel,
#'                                      ver = opts$verLabel)
#'   lineXY <- ggmagic::orientationXY(opts$orientation,
#'                                    0,
#'                                    0,
#'                                    hor = opts$horLine,
#'                                    ver = opts$verLine)
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
#'   d <- d  %>%
#'     tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
#'                            b = ifelse(is.character(d$b), "NA", NA),
#'                            c = NA)) %>%
#'     dplyr::group_by(a, b) %>%
#'     dplyr::summarise(c = ggmagic::agg(opts$agg, c)) %>%
#'     tidyr::spread(b, c, fill = 0) %>%
#'     tidyr::gather(b, c, -a) %>%
#'     dplyr::mutate(percent = round(c * 100 / sum(c, na.rm = TRUE), opts$n_digits))
#'
#'   d <- ggmagic::orderCategory(d, "a", opts$orientation, opts$order1, opts$label_wrap_legend)
#'   d <- ggmagic::orderCategory(d, "b", opts$orientation, opts$order2, opts$label_wrap)
#'   d <- ggmagic::labelPosition(d, "c", opts$label_ratio, opts$percentage, zeroToNa = TRUE)
#'
#'   colores_plot <- opts$colors
#'   if (!is.null(opts$theme$colors)) colores_plot <- opts$theme$colors
#'   fillCol <- ggmagic::fillColors(d, "a", colores_plot, opts$color_scale, NULL, NULL, opts$label_wrap)
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
#'   varP <- ifelse(opts$percentage, "percent", "c")
#'   minLim <- min(d[[varP]], na.rm = T)
#'   maxLim <- ceiling(max(d[[varP]], na.rm = T) + 0.1 * max(d[[varP]], na.rm = T))
#'
#'   gg <- ggplot(d, aes(x = b, y = d[[varP]], colour = a, group = a)) +
#'     geom_line() +
#'     geom_point(shape = as.integer(opts$shape_type)) +
#'     geom_vline(xintercept = lineXY[2],
#'                color = ifelse((opts$orientation == "hor" & !is.null(opts$horLine)) | (opts$orientation == "ver" & !is.null(opts$verLine)),
#'                               "#5A6B72",
#'                               "transparent"),
#'                linetype = "dashed") +
#'     geom_hline(yintercept = lineXY[1],
#'                color = ifelse((opts$orientation == "hor" & !is.null(opts$verLine)) | (opts$orientation == "ver" & !is.null(opts$horLine)),
#'                               "#5A6B72",
#'                               "transparent"),
#'                linetype = "dashed") +
#'     geom_text(aes(y = labPos,
#'                   label = paste0(opts$prefix,
#'                                  format(d[[varP]],
#'                                         big.mark = opts$marks[1],
#'                                         decimal.mark = opts$marks[2],
#'                                         digits = opts$n_digits,
#'                                         nsmall = opts$n_digits),
#'                                  opts$suffix)),
#'               check_overlap = TRUE,
#'               size = label_size,
#'               color = ifelse(opts$text_show, label_color, "transparent"),
#'               position = position_dodge(width = 1)) +
#'     labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, x = labelsXY[1], y = labelsXY[2]) +
#'     scale_colour_manual(values = fillCol, name = opts$legend_title) +
#'     scale_y_continuous(labels = function(x) paste0(opts$prefix,
#'                                                    format(x,
#'                                                           big.mark = opts$marks[1],
#'                                                           decimal.mark = opts$marks[2],
#'                                                           digits = opts$n_digits,
#'                                                           nsmall = opts$n_digits),
#'                                                    opts$suffix),
#'                        #breaks = seq(ifelse(opts$start_zero, 0, minLim), maxLim, round(maxLim/Lc, 2)),
#'                        limits = c(ifelse(opts$start_zero, 0, minLim), maxLim))
#'
#'   if (opts$orientation == "hor")
#'     gg <- gg +
#'     coord_flip()
#'
#'   theme_user <- opts$theme
#'   optsTheme <- list( colors = opts$colors, background = opts$background)
#'   themeCustom <- modifyList(optsTheme, theme_user %||% list())
#'   gg <- gg + ggmagic::tma(custom = themeCustom, orientation = opts$orientation)
#'
#'   gg <- gg +
#'     theme(axis.text.x = element_text(angle = angleText),
#'           plot.caption = element_text(hjust = 1),
#'           legend.position= opts$legend_position) +
#'     theme_leg() +
#'     guides(fill = guide_legend(nrow = 1))
#'
#'   if (!opts$legend_show) {
#'     gg <- gg + theme(legend.position = "none")
#'   }
#'
#'   gg
#' }
#'
#'
#' #' Lines (categories, ordered categories)
#' #'
#' #' Compare quantities among two categories
#' #'
#' #' @param data A data.frame
#' #' @return Ggplot2 visualization
#' #' @section ctypes:
#' #' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' #' @examples
#' #' gg_line_CatCat(sampleData("Cat-Cat", nrow = 10))
#' #' @export gg_line_CatCat
#' gg_line_CatCat <- function(data = NULL,
#'                            agg_text = NULL,
#'                            caption = NULL,
#'                            colors = NULL,
#'                            color_scale ="discrete",
#'                            drop_na = FALSE,
#'                            drop_na_legend = FALSE,
#'                            graph_type = 'grouped',
#'                            group_color = 'transparent',
#'                            highlight_value = NULL,
#'                            highlight_value_color = '#F9B233',
#'                            hor_label = NULL,
#'                            hor_line = NULL,
#'                            hor_line_label = NULL,
#'                            label_ratio = 1,
#'                            label_wrap = 12,
#'                            label_wrap_legend = 12,
#'                            legend_position = "bottom",
#'                            legend_show = TRUE,
#'                            legend_title = NULL,
#'                            marks = c(".", ","),
#'                            n_digits = NULL,
#'                            order1 = NULL,
#'                            order2 = NULL,
#'                            orientation = "ver",
#'                            percentage = FALSE,
#'                            prefix = NULL,
#'                            shape_type = 19,
#'                            slice_n = NULL,
#'                            sort = "no",
#'                            spline = FALSE,
#'                            start_zero = TRUE,
#'                            subtitle = NULL,
#'                            suffix = NULL,
#'                            text_color =  "#212428",
#'                            text_show = TRUE,
#'                            text_size = 3,
#'                            theme = NULL,
#'                            title = NULL,
#'                            ver_label = NULL,
#'                            ver_line = NULL,
#'                            ver_line_label = NULL,
#'                            opts = NULL, ...) {
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
#'     drop_na_legend = drop_na_legend,
#'     graph_type = graph_type,
#'     group_color = group_color,
#'     highlight_value = highlight_value,
#'     highlight_value_color = highlight_value_color,
#'     hor_label = hor_label,
#'     hor_line = hor_line,
#'     hor_line_label = hor_line_label,
#'     label_ratio = label_ratio,
#'     label_wrap = label_wrap,
#'     label_wrap_legend = label_wrap_legend,
#'     legend_position = legend_position,
#'     legend_show = legend_show,
#'     legend_title = legend_title,
#'     marks = marks,
#'     n_digits = n_digits,
#'     order = order,
#'     orientation = orientation,
#'     percentage = percentage,
#'     prefix = prefix,
#'     shape_type = shape_type,
#'     slice_n = slice_n,
#'     sort = sort,
#'     spline = spline,
#'     start_zero = start_zero,
#'     subtitle = subtitle,
#'     suffix = suffix,
#'     text_color = text_color,
#'     text_show = text_show,
#'     text_size = text_size,
#'     theme = theme,
#'     title = title,
#'     ver_label = ver_label,
#'     ver_line = ver_line,
#'     ver_line_label = ver_line_label
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
#'   prefix_agg <- ifelse(is.null(opts$agg_text), "Count ", opts$agg_text)
#'   names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label[1]))
#'   opts$agg_text <- " "
#'   gg <- gg_line_CatCatNum(data = d, opts = opts, ...)
#'   gg
#' }
#'
#'
#'
#'
#'
#'
