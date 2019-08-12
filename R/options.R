default_options <- list(
  agg = "sum",
  agg_text = NULL,
  caption = NULL,
  colors = NULL,
  color_opacity = "0.7",
  color_scale ="discrete",
  dropNa = FALSE,
  dropNaV = c(FALSE, FALSE),
  highlight_value = NULL,
  highlight_valueColor = '#F9B233',
  horLabel = NULL,
  horLine = NULL,
  horLine_label = NULL,
  label_ratio = 1,
  label_wrap = 12,
  label_wrapV = c(12, 12),
  lang = "es",
  legend_position = "bottom",
  legend_show = TRUE,
  legend_title = NULL,
  marks = c(".", ","),
  nDigits = NULL,
  nDigitsSize = NULL,
  nDigitsY = NULL,
  nDigitsX = NULL,
  order = NULL,
  order1 = NULL,
  order2 = NULL,
  orientation = "ver",
  percentage = FALSE,
  prefix = NULL,
  prefixSize = NULL,
  prefixX = NULL,
  prefixY = NULL,
  regression = FALSE,
  regression_color = '#d35400',
  shape_type = 19,
  sliceN = NULL,
  sort = "no",
  spline = FALSE,
  startAtZero = TRUE,
  subtitle = NULL,
  suffix = NULL,
  suffixX = NULL,
  suffixY = NULL,
  suffixSize = NULL,
  text_color = "#5A6B72",
  text_colorV = c("#FFFFFF", "#212428"),
  text_show = TRUE,
  text_size = 3,
  text_sizeV = c(15, 17),
  theme = NULL,
  title = NULL,
  verLabel = NULL,
  verLine = NULL,
  verLine_label = NULL
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










