#' Treemap (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num, Yea-Num, Dat-Num,
#' @examples
#' gg_treemap_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_treemap_CatNum

gg_treemap_CatNum <-  function(data,
                               title = NULL,
                               subtitle = NULL,
                               caption = NULL,
                               labelWrap = 12,
                               colors = NULL,
                               colorScale = 'continuous',
                               agg = "sum",
                               marks = c(".", ","),
                               nDigits = NULL,
                               dropNa = FALSE,
                               highlightValueColor = '#F9B233',
                               percentage = FALSE,
                               format = c('', ''),
                               highlightValue = NULL,
                               sliceN = NULL,
                               showText = TRUE,
                               showLegend = TRUE,
                               legendPosition = c("right", "bottom"),
                               tooltip = list(headerFormat = NULL, pointFormat = NULL),
                               export = FALSE,
                               theme = NULL,
                               ...) {

  data <- sampleData('Cat-Num')
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d


  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()


  if (is.null(nDigits)) {
    nDig <- 0
  } else {
    nDig <- nDigits
  }

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b))



  if (percentage) {
    d <- d %>%
      dplyr::mutate(b = b * 100 / sum(b, na.rm = TRUE))
  }


  d <- sortSlice(d, "b", "a", "ver", "desc", sliceN)
  d <- orderCategory(d, "a", "ver", unique(d$a), labelWrap)
  #d <- labelPosition(d, "b", labelRatio, percentage)
  fillCol <- fillColors(d, "a", colors, colorScale, highlightValue, highlightValueColor, labelWrap)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

   d$b <- round(d$b, nDig)

   if (showText) {
     d$label <- paste0(d$a, "\n", format[1] ,d$b, format[2])
   } else {
    d$label <- ""
     }

  g <- ggplot(d, aes(area = b, fill = a, label =  label)) +
         geom_treemap() +
           geom_treemap_text(min.size = 0,
                             colour = "white"
                             )

  if (!showLegend) {
   g <- g + theme(legend.position = "none")
  }

  g <- g +
        scale_fill_manual(values = fillCol) +
         labs(title = title, subtitle = subtitle, caption = caption)
  g
}
