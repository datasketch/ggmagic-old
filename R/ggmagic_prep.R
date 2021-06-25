

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
                                        ptage = opts$postprocess$percentage,
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
  data_format <- list_d$data
  # Handle number/strings/dates formats
  f_cats <-  makeup::makeup_format(sample = opts$style$format_sample_cat,
                                   type = "chr" )
  f_nums <- makeup::makeup_format(sample = opts$style$format_sample_num,
                                  #locale = opts$style$locale,
                                  prefix = opts$style$prefix,
                                  suffix = opts$style$suffix)

  f_dats <- makeup::makeup_format(sample = opts$style$format_sample_dat,
                                  locale = opts$style$locale)


  # axis labels -------------------------------------------------------------

  nms_dic <- list_d$nms_lab

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
    orientation = opts$chart$orientation %||% "ver",
    formats = list(
      f_cats = f_cats,
      f_nums = f_nums,
      f_dats = f_dats
    ),
    dataLabels = list(
      show = opts$dataLabels$dataLabels_show,
      type = opts$dataLabels$dataLabels_type,
      color = opts$dataLabels$dataLabels_color %||% opts$theme$text_color,
      size = (opts$dataLabels$dataLabels_size  %||% opts$theme$text_size) /3,
      dataLabels_text_outline = opts$dataLabels$dataLabels_text_outline,
      f_nums = makeup::makeup_format(sample = opts$dataLabels$dataLabels_format_sample %||% opts$style$format_num_sample,
                                     locale = opts$style$locale,
                                     prefix = opts$style$prefix,
                                     suffix = opts$style$suffix)
    ),
    theme = opts$theme,
    spline = opts$style$spline,
    graph_type = opts$chart$graph_type,
    extra = dsvizopts::get_extra_opts(opts, extra_pattern)
  )
}

