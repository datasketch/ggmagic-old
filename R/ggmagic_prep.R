

# allow orientation?
# bar_DatNum no orientation
# bar_CatNum f_cats f_nums
# dataLabels_location extra for pie

ggmagic_prep <- function(data, opts = NULL, extra_pattern = "."){

  # Handle homodatum
  f <- homodatum::fringe(data)

  needs_CatNum_agg <- f$frtype == "Cat"

  nms <- fringe_labels(f)
  d <- fringe_data(f)

  if(needs_CatNum_agg){
    d <- d %>%
      dplyr::group_by_all() %>%
      dplyr::summarise(b = n())
  }


  #axis_text_angle
  labelsXY <- opts$title$hor_title %||% nms[1]
  labelsXY[2] <- opts$title$ver_title %||% nms[2]
  if(opts$chart$orientation == "hor") labelsXY <- rev(labelsXY)
  hor_title <- labelsXY[1]
  ver_title <- labelsXY[2]

  # Drop NAs
  # TODO: Add NAs as categories or dates when it makes sense
  d <- preprocessData(d, drop_na = opts$preprocess$drop_na,
                      na_label = opts$preprocess$na_label, na_label_cols = "a")
  # Summarize
  d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)

  # Postprocess
  d <- postprocess(d, "b", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)

  if(f$dic$hdType[1] == "Cat"){
    d <- dsvizopts::order_category(d, col = "a", order = opts$postprocess$order,
                                   label_wrap = opts$style$label_wrap)
  }


  # Styles
  # Handle colors
  color_by <- names(nms[match(opts$style$color_by, nms)])
  # color_by <- "a" pie
  palette <- opts$theme$palette_colors
  d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)

  # Handle number/strings/dates formats
  f_cats <-  makeup::makeup_format(sample = opts$style$format_cat_sample,
                                   type = "chr" )
  f_nums <- makeup::makeup_format(sample = opts$style$format_num_sample)
  fmt_dataLabel <- opts$dataLabels$dataLabels_format_sample %||% opts$style$format_num_sample
  f_nums_dataLabel <- makeup::makeup_format(sample = fmt_dataLabel)

  f_dats <- makeup_format(sample = opts$style$format_dat_sample,
                          locale = opts$style$locale)

  # Calculate extra opts for pie or donut
  if(grepl("pie|donut", extra_pattern)){
    d$..ylabpos <- sum(d$b) - cumsum(d$b) + 0.5 *d$b

  }


  list(
    d = d,
    titles = list(
      title = opts$title$title,
      subtitle = opts$title$subtitle,
      caption = opts$title$caption,
      x = hor_title,
      y = ver_title
    ),
    orientation = opts$chart$orientation,
    formats = list(
      f_cats = f_cats,
      f_nums = f_nums,
      f_dats = f_dats
    ),
    dataLabels = list(
      show = opts$dataLabels$dataLabels_show,
      color = opts$dataLabels$dataLabels_color,
      size = opts$dataLabels$dataLabels_size,
      f_nums = f_nums_dataLabel
    ),
    extra = get_extra_opts(opts, extra_pattern),
    theme = opts$theme
  )

}

