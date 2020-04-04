


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
  opts <- mergeOptions(...)
  #opts <- getDefaultOptions()
  if (is.null(data)) {
    stop("need a dataset to visualize")
  }
  f <- fringe(data)
  nms <- getFringeLabels(f)
  d <- getFringeDataFrame(f)

  #axis_text_angle
  #axis titles
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
  d$..colors <- mapColors(f, color_by, palette, colors_df)


  gg <- ggplot(d, aes(x = a, y = b, fill = colors )) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    labs(title = opts$title, subtitle = opts$subtitle,
         caption = opts$caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_gradientn(colours = opts$palette_colors) +
    scale_y_continuous(labels = d3.format::d3_format("$,.3~s")) +
    scale_x_date(date_labels =  "%b %Y")

  theme_user <- opts$theme
  optsTheme <- list( colors = opts$colors, background = opts$background)
  themeCustom <- modifyList(optsTheme, theme_user %||% list())
  gg <- gg + ggmagic::tma(custom = themeCustom, orientation = opts$orientation)


  gg + theme(legend.position = "none",
             plot.caption = element_text(hjust = 1),
             axis.text.x = element_text(angle = angleText))


}
