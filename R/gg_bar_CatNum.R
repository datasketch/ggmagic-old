#' Bar Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @inherit ggmagic_default_opts
#' @inheritDotParams ggmagic_default_opts
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' gg_bar_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export
gg_bar_CatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  f <- homodatum::fringe(data)
  nms <- getFringeLabels(f)
  d <- getFringeDataFrame(f)

  #axis_text_angle
  labelsXY <- opts$title$hor_title %||% nms[1]
  labelsXY[2] <- opts$title$ver_title %||% nms[2]
  if(opts$chart$orientation == "hor") labelsXY <- rev(labelsXY)
  hor_title <- labelsXY[1]
  ver_title <- labelsXY[2]

  # Drop NAs
  # TODO: Add NAs as categories or dates when it makes sense
  d <- preprocessData(d, opts$preprocess)

  # Summarize
  d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)

  # Styles
  # Handle colors
  color_by <- names(nms[match(opts$style$color_by, nms)])
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
         x = hor_title, y = ver_title) +
    scale_y_continuous(labels = f_nums) +
    scale_x_discrete(labels = f_cats)

  if (opts$chart$orientation == "hor")
    gg <- gg + coord_flip()

  # opts_theme <- merge_theme_options(opts)
  # str(opts_theme)
  # ??? gg + labs(caption = opts_theme$caption)
  gg <- gg + theme_datasketch(opts$theme)
  add_branding_bar(gg, opts$theme)

}
