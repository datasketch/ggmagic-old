#' default_options <- list(
#'   agg = "sum",
#'   agg_text = NULL,
#'   caption = NULL,
#'   colors = NULL,
#'   color_opacity = "0.7",
#'   color_scale ="discrete",
#'   drop_na = FALSE,
#'   drop_na_v = c(FALSE, FALSE),
#'   graph_type = 'grouped',
#'   group_color = 'transparent',
#'   highlight_value = NULL,
#'   highlight_valueColor = '#F9B233',
#'  hor_label = NULL,
#'   hor_line = NULL,
#'   hor_line_label = NULL,
#'   label_ratio = 1,
#'   label_wrap = 12,
#'   label_wrapV = c(12, 12),
#'   lang = "es",
#'   legend_position = "bottom",
#'   legend_show = TRUE,
#'   legend_title = NULL,
#'   marks = c(".", ","),
#'   n_digits = NULL,
#'   n_digits_size = NULL,
#'   n_digits_y = NULL,
#'   n_digits_x = NULL,
#'   order = NULL,
#'   order1 = NULL,
#'   order2 = NULL,
#'   orientation = "ver",
#'   percentage = FALSE,
#'   prefix = NULL,
#'   prefix_size = NULL,
#'   prefix_x = NULL,
#'   prefix_y = NULL,
#'   regression = FALSE,
#'   regression_color = '#d35400',
#'   shape_type = 19,
#'   sliceN = NULL,
#'   sort = "no",
#'   spline = FALSE,
#'   start_zero = TRUE,
#'   subtitle = NULL,
#'   suffix = NULL,
#'   suffix_X = NULL,
#'   suffix_Y = NULL,
#'   suffix_size = NULL,
#'   text_color = "#5A6B72",
#'   text_colorV = c("#FFFFFF", "#212428"),
#'   text_show = TRUE,
#'   text_size = 3,
#'   text_size_v = c(15, 17),
#'   theme = NULL,
#'   title = NULL,
#'   ver_label = NULL,
#'   ver_line = NULL,
#'   ver_line_label = NULL
#' )
#'
#' #' @export
#' getOptions <- function(opts = NULL) {
#'   if (is.null(opts)){
#'     opts <- default_options
#'   } else {
#'     opts <- modifyList(default_options, opts)
#'   }
#'   opts
#' }

# cambiaron de nombre
# hor_line_label = " ",
# ver_line_label = " ",
# color_opacity = "0.7",
# color_scale ="discrete",
# graph_type = "grouped",
# highlight_value = NULL,
# highlight_valueColor = '#F9B233',
# label_ratio = 1,
# label_wrap = 12,
# label_wrapV = c(12, 12)
# legend_position = "bottom",
# legend_title = NULL,
# shape_type = 19,
# text_color = "#5A6B72",
# text_show = TRUE,
# text_size = 3,










