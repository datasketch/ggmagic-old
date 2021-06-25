

# allow orientation?
# bar_DatNum no orientation
# bar_CatNum f_cats f_nums
# dataLabels_location extra for pie

ggmagic_prep <- function(data, opts = NULL, extra_pattern = ".", plot =  "bar", ftype = "Cat-Num") {

  if (is.null(data)) return()

  # color -------------------------------------------------------------------

  palette_type <- opts$theme$palette_type %||% "categorical"
  if(is.null(opts$theme$palette_colors)){
    opts$theme$palette_colors <- opts$theme[[paste0("palette_colors_", palette_type)]]
  }
  palette_colors <- opts$theme$palette_colors

  # data preparation by type
  list_d <- dsvizprep::data_charts_prep(data = data,
                                        ftype = ftype,
                                        agg =  opts$summarize$agg,
                                        color_by = opts$style$color_by,
                                        ptage_col = opts$postprocess$percentage_col,
                                        drop_na = opts$preprocess$drop_na,
                                        na_label = opts$preprocess$na_label,
                                        drop_na_legend = opts$preprocess$drop_na_legend,
                                        sort_opts = opts$postprocess$sort,
                                        slice_n = opts$postprocess$slice_n,
                                        palette = palette_colors,
                                        highlight_value = opts$chart$highlight_value,
                                        highlight_value_color = opts$chart$highlight_value_color,
                                        order_legend = opts$postprocess$order_legend,
                                        order = opts$postprocess$order,
                                        label_wrap_legend = opts$style$label_wrap_legend,
                                        label_wrap = opts$style$label_wrap,
                                        group_extra_num = TRUE)
  # format setting of data being displayed
  data_format <- dsvizprep::format_prep(data = list_d$data,
                                        dic = list_d$dic,
                                        formats = list(sample_num = opts$style$format_sample_num,
                                                       sample_cat = opts$style$format_sample_cat,
                                                       prefix = opts$style$prefix,
                                                       suffix = opts$style$suffix))

  # axis labels -------------------------------------------------------------

  nms_dic <- list_d$nms[names(list_d$nms) %in% c("a", "b", "c", "d")]

  labelsXY <- dsvizprep::labelsXY(hor_title = opts$title$hor_title %||% nms_dic[[(1 + (length(nms_dic) - 2))]], #
                                  ver_title = opts$title$ver_title %||% nms_dic[[length(nms_dic)]],
                                  nms = nms_dic, orientation = opts$chart$orientation)

  hor_title <- as.character(labelsXY[1])
  ver_title <- as.character(labelsXY[2])

  list(
    d = data_format,
    titles = list(
      title = opts$title$title,
      subtitle = opts$title$subtitle,
      caption = opts$title$caption %||% "",
      x = hor_title,
      y = ver_title
    ),
    dataLabels = list(
      show = opts$dataLabels$dataLabels_show,
      type = opts$dataLabels$dataLabels_type,
      color = opts$dataLabels$dataLabels_color %||% opts$theme$text_color,
      size = (opts$dataLabels$dataLabels_size  %||% opts$theme$text_size) /3,
      dataLabels_text_outline = opts$dataLabels$dataLabels_text_outline,
      format_dataLabels = format_dataLabels
    ),
    theme = c(opts$theme,
              isNullCaption = is.null(opts$title$caption),
              bar_pointWidth = opts$theme$bar_pointWidth,
              credits = show_caption,
              y_credits = y_caption,
              suffix = suffix_enter,
              prefix = opts$style$prefix),
    spline = opts$style$spline,
    graph_type = opts$chart$graph_type,
    extra = dsvizopts::get_extra_opts(opts, extra_pattern)
  )
}

