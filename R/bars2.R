#' Bar Chart Date Numeric
#'
#' @param data A data.frame
#' @rdname chart_options
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_bar_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_bar_CatNum
gg_bar_DatNum <- function(data, ...){
  opts <- mergeOptions(..., defaults = ggmagic_defaults())
  # str(opts)
  #opts <- ggmagic_defaults()
  if (is.null(data)) {
    stop("need a dataset to visualize")
  }
  f <- homodatum::fringe(data)
  nms <- getFringeLabels(f)
  d <- getFringeDataFrame(f)
  #axis_text_angle
  labelsXY <- c(nms[1], nms[2])

  # Drop NAs
  d <- processDataOptions(d, opts)

  # Add NAs as categories or dates when it makes sense
  # d <- na_as_Cat(d)

  # Summarize
  d <- summarizeData(d, opts$agg, to_agg = b, a)

  # Handle colors
  color_by <- opts$color_by
  palette <- opts$palette_colors
  d$..colors <- paletero::map_colors(f, color_by, palette, colors_df)

  # Handle number/strings/dates formats
  f_date <- makeup_format(sample = opts$format_dat_sample, locale = opts$locale)
  f_nums <- makeup_format(sample = opts$format_num_sample)

  gg <- ggplot(d, aes(x = a, y = b, fill = ..colors )) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption,
         x = labelsXY[1], y = labelsXY[2]) +
    scale_y_continuous(labels = f_nums) +
    scale_x_date(labels = f_date)

  opts_theme <- merge_theme_options(opts)
  str(opts_theme)
  gg <- gg +
    labs(caption = opts_theme$caption) +
    theme_datasketch(opts_theme)
  ggg <- add_branding_bar(gg, opts_theme)
  # grid.draw(ggg)
  ggg

}
