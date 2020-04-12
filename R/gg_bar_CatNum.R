#' Bar Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @inherit chart_defaults
#' @inheritDotParams chart_defaults
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' gg_bar_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export
gg_bar_CatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")
  opts <- merge_ggmagic_options(...)

  f <- homodatum::fringe(data)
  nms <- getFringeLabels(f)
  d <- getFringeDataFrame(f)

  #axis_text_angle
  labelsXY <- c(nms[1], nms[2])

  # Drop NAs
  # TODO: Add NAs as categories or dates when it makes sense
  d <- preprocessData(d, opts$preprocess)

  # Summarize
  d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)

  # Styles
  # Handle colors
  color_by <- opts$style$color_by
  palette <- opts$theme$palette_colors
  d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)

  # Handle number/strings/dates formats
  f_cats <-  makeup::makeup_format(sample = opts$style$format_cat_sample,
                                   type = "chr" )
  f_nums <- makeup::makeup_format(sample = opts$style$format_num_sample)

  gg <- ggplot(d, aes(x = a, y = b, fill = ..colors )) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    labs(title = opts$title$title,
         subtitle = opts$title$subtitle,
         caption = opts$title$caption,
         x = labelsXY[1], y = labelsXY[2]) +
    scale_y_continuous(labels = f_nums) +
    scale_x_discrete(labels = f_cats)

  if (opts$chart$orientation == "ver")
    gg <- gg + coord_flip()

  # opts_theme <- merge_theme_options(opts)
  # str(opts_theme)
  # ??? gg + labs(caption = opts_theme$caption)
  gg <- gg + theme_datasketch(opts$theme)
  add_branding_bar(gg, opts$theme)

}
