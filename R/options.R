
#' Chart Options
#'
#' @name chart_options
#' @param data data.frame with the proper frtype
#' @param agg defaults to "sum"
#' @param agg_text defaults to NULL
#' @param caption defaults to NULL
#' @param colors defaults to NULL
#' @param color_scale defaults to discrete"
#' @param drop_na defaults to FALSE
#' @param format_num defaults to "1,500.00"
#' @param highlight_value defaults to NULL
#' @param highlight_value_color defaults to '#F9B233'
#' @param hor_label defaults to NULL
#' @param hor_line defaults to NULL
#' @param label_ratio defaults to 1
#' @param label_wrap defaults to 12
#' @param marks defaults to c("." "")
#' @param n_digits defaults to NULL
#' @param order defaults to NULL
#' @param orientation defaults to "ver"
#' @param percentage defaults to FALSE
#' @param prefix defaults to NULL
#' @param slice_n defaults to NULL
#' @param sort defaults to "no"
#' @param subtitle defaults to NULL
#' @param suffix defaults to NULL
#' @param text_color defaults to "#5A6B72"
#' @param text_show defaults to TRUE
#' @param text_size defaults to 3
#' @param theme defaults to NULL
#' @param title defaults to NULL
#' @param ver_label defaults to NULL
#' @param ver_line defaults to NULL
#' @param opts defaults to NULL
#' @return ggplot2 object
NULL
#> NULL

mergeOptions <- function(..., defaults = NULL){
  args <- list(...)
  # str(args)
  opts_list <- args$opts
  # str(opts_list)
  args$opts <- NULL
  args_opts <- modifyList(args, opts_list %||% list())
  # str(args_opts)
  modifyList(defaults, args_opts)
}

ggmagic_defaults <- function(x){
  opts <- list(
      agg = "sum",
      agg_text = NULL,
      axis_text_angle = NULL,
      background_color = "#FAFAF0",
      caption = NULL,
      color_by = NULL,# which variable?
      palette_colors = NULL,
      #color_scale ="discrete",
      drop_na = FALSE,
      format_num_sample = "1,500.00",
      format_dat_sample = "Jun 24 2010",
      # highlight_value = NULL,
      # highlight_value_color = '#F9B233',
      hor_label = NULL,
      hor_line = NULL,
      # label_ratio = 1,
      # label_wrap = 12,
      locale = "en-US",
      orientation = "ver",
      slice_n = NULL,
      sort = "no",
      subtitle = NULL,
      text_color = "#5A6B72",
      # text_show = TRUE, # show values?
      text_size = 11,
      theme = default_theme_opts(),
      title = NULL,
      ver_label = NULL,
      ver_line = NULL
  )

  # order = NULL,
  # ORDEN DE LAS CATEGORiAS
  # order_columns Cat-Cat-Num, se puede intercambiar?
  # label_wrap
  # label_wrap_legend
  # limits_x limits_y
  # para el eje numerico
  # text_color (title, labelX, )
  # zoom?

  opts$agg_text<- opts$agg_text %||% opts$agg
  opts$palette_colors <- opts$palette_colors %||% opts$theme$palette_colors
  opts$background_color <- opts$background_color %||% opts$theme$background_color

  opts
}
