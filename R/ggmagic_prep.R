

# allow orientation?
# bar_DatNum no orientation
# bar_CatNum f_cats f_nums
# dataLabels_location extra for pie

ggmagic_prep <- function(data, opts = NULL,
                         extra_pattern = ".", family = ""){

  # Handle homodatum
  f <- homodatum::fringe(data)

  needs_CatNum_agg <- f$frtype %in% c("Cat", "Dat", "Yea")
  needs_CatCat_agg <- f$frtype %in% c("Cat-Cat", "Cat-Dat" ,"Cat-Yea")
  is_CatCat <- grepl("Cat-Cat", f$frtype)

  nms <- fringe_labels(f)
  d <- fringe_d(f)

  if(needs_CatNum_agg){
    d <- d %>%
      dplyr::group_by_all() %>%
      dplyr::summarise(b = n())
    f$frtype <- "Cat-Num"
  }

  if(needs_CatCat_agg){
    d <- d %>%
      dplyr::group_by_all() %>%
      dplyr::summarise(c = n())
    f$frtype <- "Cat-Cat-Num"
  }


  if(f$frtype %in% c("Cat-Dat-Num","Cat-Yea-Num")){
    labelsXY <- opts$title$hor_title %||% nms[2]
    labelsXY[2] <- opts$title$ver_title %||% nms[3]
    color_title <- opts$title$color_title %||% nms[1]
  }else{
    labelsXY <- opts$title$hor_title %||% nms[1]
    labelsXY[2] <- opts$title$ver_title %||% nms[2]
    color_title <- NULL
  }
  if(opts$chart$orientation == "hor") labelsXY <- rev(labelsXY)
  hor_title <- labelsXY[1]
  ver_title <- labelsXY[2]

  # Drop NAs
  # TODO: Add NAs as categories or dates when it makes sense
  d <- dsvizopts::preprocessData(d, drop_na = opts$preprocess$drop_na,
                      na_label = opts$preprocess$na_label, na_label_cols = "a")

  # Summarize
  if(f$frtype %in% c("Cat-Num", "Dat-Num")){
    d <- dsvizopts::summarizeData(d, opts$summarize$agg, to_agg = b, a)
    if (opts$postprocess$percentage) {
      d$b <- (d$b/sum(d$b))*100
      opts$style$suffix <- "%"
    }
    d <- ggmagic::labelPosition(d, "b", opts$style$label_ratio)
    label_position <- NULL
  }
  if(f$frtype %in% c("Cat-Cat-Num", "Cat-Yea-Num", "Cat-Dat-Num")){
    d <- summarizeData(d, opts$summarize$agg, to_agg = c, a, b)
    if (opts$postprocess$percentage) {
      by_col <- opts$postprocess$percentage_col

      if (is.null(by_col)) {
        by_col <- "a"
      } else {
        by_col <- names(nms[match(by_col, nms)])
      }

      d <- d %>% group_by_(by_col) %>%
        dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
      opts$style$suffix <- "%"
    }

    if (opts$chart$graph_type == "grouped") {
      d <- ggmagic::labelPosition(d, "c", opts$style$label_ratio, zeroToNa = TRUE)
      label_position <- position_dodge(width = 0.55)
    } else {
      d$labPos <- d$c
      label_position <-  position_stack(vjust = 0.5)
    }
  }

  # Postprocess
  if(f$frtype == c("Cat-Num")){
    d <- dsvizopts::postprocess(d, "b", sort = opts$postprocess$sort,
                                slice_n = opts$postprocess$slice_n)
  }

  # Complete missing with 0s
  if(family == "area" && opts$extra$graph_type == "stacked"){
    d <- d %>%
      ungroup() %>%
      #filter(!is.na(b)) %>% # Remove missing years
      tidyr::complete(a,nesting(b), fill = list(c = 0))

  }

  if(f$dic$hdType[1] == "Cat"){
    d <- dsvizopts::order_category(d, col = "a", order = opts$postprocess$order,
                                   label_wrap = opts$style$label_wrap, new_line = "\n")
  }


  # Styles
  # Handle colors
  color_by <- names(nms[match(opts$style$color_by, nms)])
  # color_by <- "a" pie

  palette <- opts$theme$palette_colors

  d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)
  if(f$frtype %in% c("Cat-Dat-Num", "Cat-Yea-Num") && family %in% c("line","area", "bar", "treemap")){
    d$..colors <- paletero::map_colors(d, color_by = "a", palette, colors_df = NULL)
  }
  if(grepl("Cat-Cat|Cat-Yea",f$frtype)){
    d$..colors <- paletero::map_colors(d, color_by = "b", palette, colors_df = NULL)
  }




  # Handle number/strings/dates formats
  f_cats <-  makeup::makeup_format(sample = opts$style$format_cat_sample,
                                   type = "chr" )
  f_nums <- makeup::makeup_format(sample = opts$style$format_num_sample,
                                  locale = opts$style$locale,
                                  prefix = opts$style$prefix,
                                  suffix = opts$style$suffix)
  fmt_dataLabel <- opts$dataLabels$dataLabels_format_sample %||% opts$style$format_num_sample
  f_nums_dataLabel <- makeup::makeup_format(sample = fmt_dataLabel,
                                            locale = opts$style$locale,
                                            prefix = opts$style$prefix,
                                            suffix = opts$style$suffix)

  f_dats <- makeup::makeup_format(sample = opts$style$format_dat_sample,
                          locale = opts$style$locale)

  # Calculate extra opts for pie or donut
  if(grepl("pie|donut", extra_pattern)){
    d$..ylabpos <- sum(d$b) - cumsum(d$b) + 0.5 *d$b
  }

  extra <- dsvizopts::get_extra_opts(opts, extra_pattern)

  list(
    d = d,
    colors = list(
      c_cats = NULL # user for custom color values
    ),
    titles = list(
      title = opts$title$title,
      subtitle = opts$title$subtitle,
      caption = opts$title$caption,
      x = hor_title,
      y = ver_title,
      color = color_title,
      legend = opts$title$legend_title %||% " "
    ),
    orientation = opts$chart$orientation %||% "ver",
    formats = list(
      f_cats = f_cats,
      f_nums = f_nums,
      f_dats = f_dats
    ),
    dataLabels = list(
      show = opts$dataLabels$dataLabels_show,
      color = opts$dataLabels$dataLabels_color %||% opts$theme$text_color,
      size = (opts$dataLabels$dataLabels_size %||% opts$theme$text_size)/3,
      f_nums = f_nums_dataLabel,
      f_label_position =  label_position
    ),
    extra = extra,
    theme = opts$theme
  )

}

