#' Bar Chart Date Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @param orientation Doesn't do anything for this type of chart.
#' @inherit ggmagic_default_opts
#' @inheritDotParams ggmagic_default_opts
#' @section ctypes:
#' Dat-Num, Yea-Num
#' @examples
#' gg_bar_DatNum(sampleData("Cat-Num", nrow = 10))
#' @export
gg_bar_DatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  f <- homodatum::fringe(data)
  nms <- getFringeLabels(f)
  d <- getFringeDataFrame(f)

  #axis_text_angle
  labsXY <- dsvizopts::labelsXY(opts$title$hor_title, opts$title$ver_title,
                       nms, opts$chart$orientation)
  hor_title <- labsXY[1]
  ver_title <- labsXY[2]

  # labelsXY <- opts$title$hor_title %||% nms[1]
  # labelsXY[2] <- opts$title$ver_title %||% nms[2]
  # if(opts$chart$orientation == "hor") labelsXY <- rev(labelsXY)

  # Drop NAs
  # Add NAs as categories or dates when it makes sense
  d <- preprocessData(d, opts$preprocess)

  # Summarize
  d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)

  # Styles
  # Handle colors
  # color_by <- opts$style$color_by
  color_by <- names(nms[match(opts$style$color_by, nms)])
  palette <- opts$theme$palette_colors
  d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)

  # Handle number/strings/dates formats
  f_date <- makeup_format(sample = opts$style$format_dat_sample,
                          locale = opts$style$locale)
  f_nums <- makeup_format(sample = opts$style$format_num_sample)

  gg <- ggplot(d, aes(x = a, y = b, fill = ..colors )) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    labs(title = opts$title$title,
         subtitle = opts$title$subtitle,
         caption = opts$title$caption,
         x = labsXY[1], y = labsXY[2]) +
    scale_y_continuous(labels = f_nums) +
    scale_x_date(labels = f_date)

  # if (opts$orientation == "hor")
  #   gg <- gg +
  #   coord_flip()

  # # opts_theme <- merge_theme_options(opts)
  # message("opts$theme")
  # # str(opts$theme)
  gg <- gg + theme_datasketch(opts$theme)
  add_branding_bar(gg, opts$theme)

}
