default_options <- list(
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  horLabel = NULL,
  verLabel = NULL,
  horLine = NULL,
  horLine_label = NULL,
  verLine = NULL,
  verLine_label = NULL,
  agg = "sum",
  agg_text = NULL,
  colors = NULL,
  color_opacity = "0.7",
  color_scale ="discrete",
  dropNa = FALSE,
  dropNaV = c(FALSE, FALSE),
  graph_type = "grouped",
  group_color = "transparent",
  highlight_value = NULL,
  highlight_valueColor = '#F9B233',
  label_ratio = 1,
  label_wrap = 12,
  label_wrapV = c(12, 12),
  lang = "es",
  legend_position = "bottom",
  legend_show = TRUE,
  legend_title = NULL,
  marks = c(".", ","),
  nDigits = NULL,
  order = NULL,
  order1 = NULL,
  order2 = NULL,
  orientation = "ver",
  percentage = FALSE,
  prefix = NULL,
  shape_type = 19,
  sliceN = NULL,
  sort = "no",
  spline = FALSE,
  startAtZero = TRUE,
  suffix = NULL,
  text_color = "#5A6B72",
  text_colorV = c("#FFFFFF", "#212428"),
  text_show = TRUE,
  text_size = 3,
  text_sizeV = c(15, 17),
  theme = NULL
)

# ¿no toca exportar
#' @export
getOptions <- function(opts = NULL) {
  if (is.null(opts)){
    opts <- default_options
  } else {
    opts <- modifyList(default_options, opts)
  }
  opts
}

# cambiaron de nombre
# horLine_label = " ",
# verLine_label = " ",
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










