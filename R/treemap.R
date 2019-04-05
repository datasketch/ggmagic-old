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
                               colorScale = 'discrete',
                               agg = "sum",
                               marks = c(".", ","),
                               nDigits = NULL,
                               dropNa = FALSE,
                               prefix = NULL,
                               suffix = NULL,
                               highlightValueColor = '#F9B233',
                               percentage = FALSE,
                               format = c('', ''),
                               highlightValue = NULL,
                               sliceN = NULL,
                               showText = TRUE,
                               showLegend = TRUE,
                               legendPosition = "bottom",
                               theme = NULL,
                               ...) {

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
  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- 'NA'



  if (percentage) {
    d <- d %>%
      dplyr::mutate(b = b * 100 / sum(b, na.rm = TRUE))
  }


  d <- sortSlice(d, "b", "a", "ver", "desc", sliceN)
  d <- orderCategory(d, "a", "ver", unique(d$a), labelWrap)
  #d <- labelPosition(d, "b", labelRatio, percentage)
  fillCol <- fillColors(d, "a", colors, colorScale, highlightValue, highlightValueColor, labelWrap)

  if (percentage & is.null(suffix)) {
    suffix <- "%"
  }

  d$b <- round(d$b, nDig)

  if (showText) {
    d$label <- paste0(d$a, "\n", prefix ,format(d$b,  big.mark = marks[1], decimal.mark = marks[2]), suffix)
  } else {
    d$label <- ""
  }

  g <- ggplot(d, aes(area = b, fill = a, label =  label)) +
    treemapify::geom_treemap() +
    geom_treemap_text(min.size = 0,
                      colour = "white"
    )

  if (!showLegend) {
    g <- g + theme(legend.position = "none")
  } else {
    g <- g + theme(legend.position = legendPosition) +
      guides(fill=guide_legend(nrow=1,byrow=TRUE))
  }

  g <- g +
    scale_fill_manual(values = fillCol) +
    labs(title = title, subtitle = subtitle, caption = caption, fill = "")

  if (is.null(theme)) {
    g <- g + tma()
  } else {
    g <- g + theme
  }
  g +
    theme_leg()
}

#' Treemap (categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' gg_treemap_Cat(sampleData("Cat", nrow = 10))
#' @export gg_treemap_Cat

gg_treemap_Cat <-  function(data,
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
                            prefix = NULL,
                            suffix = NULL,
                            highlightValue = NULL,
                            sliceN = NULL,
                            showText = TRUE,
                            showLegend = TRUE,
                            legendPosition = "bottom",
                            theme = NULL,
                            ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  names(d) <- c(f$dic_$d$label, paste0("count ", f$dic_$d$label))

  g <- gg_treemap_CatNum(data = d, title = title, subtitle = subtitle, caption = caption,labelWrap = labelWrap, colors = colors, colorScale = colorScale, agg = agg,marks = marks,nDigits = nDigits,dropNa = dropNa, highlightValueColor = highlightValueColor,percentage = percentage, prefix = prefix, suffix = suffix, highlightValue = highlightValue,sliceN = sliceN, showText = showText, showLegend = showLegend, legendPosition = legendPosition, theme = theme,...)
  g
}



#' Treemap (categories, categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num, Cat-Dat-Num,
#' @examples
#' gg_treemap_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export gg_treemap_CatCatNum

gg_treemap_CatCatNum <- function(data,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 agg = "sum",
                                 colors = NULL,
                                 colorScale = 'discrete',
                                 colorGroup = 'transparent',
                                 colorText = c('#212428', '#FFFFFF'),
                                 dropNaV = c(FALSE, FALSE),
                                 prefix = NULL,
                                 suffix = NULL,
                                 labelWrapV = c(12, 12),
                                 marks = c(".", ","),
                                 nDigits = NULL,
                                 percentage = FALSE,
                                 showText = TRUE,
                                 showLegend = TRUE,
                                 legendPosition = "bottom",
                                 theme = NULL, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (any(dropNaV))
    d <- d %>%
    tidyr::drop_na(which(dropNaV))

  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c))

  d <- d %>% drop_na(c)


  if (percentage) {
    d <- d %>% group_by(b) %>%
      dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
  }

  d <- orderCategory(d, "a", "ver", unique(d$a), labelWrapV[1])
  d <- orderCategory(d, "b", "ver", unique(d$b), labelWrapV[2])

  fillCol <- fillColors(d, "b", colors, colorScale, NULL, NULL, labelWrapV[1])

  if (showText) {
    d$label <- paste0(d$a, "\n", prefix ,format(d$c,  big.mark = marks[1], decimal.mark = marks[2]), suffix)
  } else {
    d$label <- ""
  }

  g <- ggplot(d, aes(area = c, fill = b, subgroup = b, label = label)) +
    treemapify::geom_treemap() +
    geom_treemap_subgroup_border(color = colorGroup) +
    geom_treemap_subgroup_text(place = "topleft",  colour = colorText[1], min.size = 0, reflow = T, size = 17) +
    geom_treemap_text(colour = colorText[2], place = "bottomleft", min.size = 0, size = 15)

  if (!showLegend) {
    g <- g + theme(legend.position = "none")
  } else {
    g <- g + theme(legend.position = legendPosition) +
      guides(fill=guide_legend(nrow=1,byrow=TRUE))
  }

  g <- g +
    scale_fill_manual(values = fillCol) +
    labs(title = title, subtitle = subtitle, caption = caption, fill = "")

  if (is.null(theme)) {
    g <- g + tma()
  } else {
    g <- g + theme
  }

  g +
    theme_leg()

}




#' Treemap (categories, categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Cat-Cat, Cat-Yea, Cat-Dat,
#' @examples
#' gg_treemap_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export gg_treemap_CatCat

gg_treemap_CatCat <- function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              agg = "sum",
                              colors = NULL,
                              colorScale = 'discrete',
                              colorGroup = 'transparent',
                              colorText = c('#212428', '#FFFFFF'),
                              dropNaV = c(FALSE, FALSE),
                              prefix = NULL,
                              suffix = NULL,
                              labelWrapV = c(12, 12),
                              marks = c(".", ","),
                              nDigits = NULL,
                              percentage = FALSE,
                              showText = TRUE,
                              showLegend = TRUE,
                              legendPosition = "bottom",
                              theme = NULL, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  names(d) <- c(f$dic_$d$label, paste0("count", f$dic_$d$label[1]))
  gg_treemap_CatCatNum(data = d, title = title,subtitle = subtitle, caption = caption, agg = agg,colors = colors, colorScale = colorScale, colorGroup = colorGroup, colorText = colorText, dropNaV = dropNaV,  prefix = prefix, suffix = suffix, labelWrapV = labelWrapV, marks = marks, nDigits = nDigits, percentage = percentage, showText = showText, showLegend = showLegend, legendPosition = legendPosition,theme = theme, ...)
}




#' Treemap (categories, categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-NumP, Yea-NumP, Dat-NumP
#' @examples
#' gg_treemap_CatNumP(sampleData("Cat-NumP", nrow = 10))
#' @export gg_treemap_CatNumP

gg_treemap_CatNumP <- function(data,
                               title = NULL,
                               subtitle = NULL,
                               caption = NULL,
                               agg = "sum",
                               colors = NULL,
                               colorScale = 'discrete',
                               colorGroup = 'transparent',
                               colorText = c('#212428', '#FFFFFF'),
                               dropNaV = c(FALSE, FALSE),
                               prefix = NULL,
                               suffix = NULL,
                               labelWrapV = c(12, 12),
                               marks = c(".", ","),
                               nDigits = NULL,
                               percentage = FALSE,
                               showText = TRUE,
                               showLegend = TRUE,
                               legendPosition = "bottom",
                               theme = NULL, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  names(d) <- f$dic_$d$label

  data <- d %>%
    gather("categories", "count", names(d)[-1])
  gg_treemap_CatCatNum(data = data, title = title, subtitle = subtitle, caption = caption, agg = agg,colors = colors, colorScale = colorScale, colorGroup = colorGroup, colorText = colorText, dropNaV = dropNaV, prefix = prefix ,suffix = suffix, labelWrapV = labelWrapV, marks = marks, nDigits = nDigits, percentage = percentage, showText = showText, showLegend = showLegend, legendPosition = legendPosition,theme = theme, ...)

}
