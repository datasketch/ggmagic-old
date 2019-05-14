default_options <- list(
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  horLabel = NULL,
  verLabel = NULL,
  horLine = NULL,
  horLine_label = " ",
  verLine = NULL,
  verLine_label = " ",
  agg = "sum",
  agg_text = NULL,
  colors = NULL,
  color_opacity = "0.7",
  color_scale ="discrete",
  dropNa = FALSE,
  dropNaV = c(FALSE, FALSE),
  graph_type = "grouped",
  highlight_value = NULL, #highlightValue
  highlight_valueColor = '#F9B233', #highlight_value_color
  label_ratio = 1, # no debería ser label_ratio
  label_wrap = 12,
  label_wrapV = c(12, 12),
  lang = "es",
  legend_position = "bottom",
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
  text_color = "#5A6B72", # text_color
  text_show = TRUE, # text_show
  text_size = 3, # text_size
  theme = NULL
)

# ¿no toca exportar
getOptions <- function(opts = NULL) {
  if (is.null(opts)){
    opts <- default_options
  } else {
    opts <- modifyList(default_options, opts)
  }
  opts
}

opts$
function(data = NULL,
         opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)
