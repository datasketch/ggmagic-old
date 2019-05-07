#' Bar (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_bar_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_bar_CatNum
gg_bar_CatNum <- function(data,
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          horLabel = NULL,
                          verLabel = NULL,
                          horLine = NULL,
                          #horLineLabel = NULL,
                          verLine = NULL,
                          #verLineLabel = NULL,
                          agg = "sum",
                          agg_text = NULL,
                          colors = NULL,
                          colorText = "#5A6B72",
                          colorScale = "no",
                          dropNa = FALSE,
                          prefix = NULL,
                          suffix = NULL,
                          highlightValue = NULL,
                          highlightValueColor = NULL,
                          labelRatio = 1,
                          labelWrap = 12,
                          marks = c(".", ","),
                          nDigits = NULL,
                          order = NULL,
                          orientation = "ver",
                          percentage = FALSE,
                          sort = "no",
                          sliceN = NULL,
                          showText = TRUE,
                          sizeText = 3,
                          theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  Lc <- length(unique(d$a))
  angleText <- ifelse( Lc >= 10 & Lc < 15, 45,
                       ifelse(Lc >= 15, 90, 0))

  prefix_agg <- ifelse(is.null(agg_text), agg, agg_text)

  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(prefix_agg, nms[2])),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          0,
                          0,
                          hor = horLine,
                          ver = verLine)

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
      dplyr::summarise(b = round(agg(agg, b)), nDig) %>%
      dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), nDig))

  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d <- labelPosition(d, "b", labelRatio, percentage)
  fillCol <- fillColors(d, "a", colors, colorScale, highlightValue, highlightValueColor, labelWrap)

  if (percentage & is.null(suffix)) {
    suffix <- "%"
  }

  varP <- ifelse(percentage, "percent", "b")
  minLim <- ifelse(min(d[[varP]], na.rm = T) < 0, min(d[[varP]], na.rm = T), 0)
  maxLim <- max(d[[varP]], na.rm = T) + 0.3 * max(d[[varP]], na.rm = T)
  sq <- nchar(round(maxLim - minLim, 0)) - 1
  minLim <- round(minLim *  10^(-sq), 0) * 10^sq
  maxLim <- round(maxLim *  10^(-sq), 0) * 10^sq
  sq <- seq(minLim, maxLim, 10^sq)
  sq <- unique(c(0, minLim, sq))

  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], fill = a)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = lineXY[2],
               color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)),
                              "#5A6B72",
                              "transparent"),
               linetype = "dashed") +
    geom_hline(yintercept = lineXY[1],
               color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)),
                              "#5A6B72",
                              "transparent"),
               linetype = "dashed") +
    geom_text(aes(y = labPos,
                  label = paste0(prefix,
                                 format(d[[ifelse(percentage, "percent", "b")]],
                                        big.mark = marks[1],
                                        decimal.mark = marks[2],
                                        nsmall = nDig),
                                 suffix)),
              check_overlap = TRUE,
              size = sizeText,
              color = ifelse(showText, colorText, "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = fillCol) +
    scale_y_continuous(labels =  function(x) paste0(prefix,
                                                    format(x,
                                                           big.mark = marks[1],
                                                           decimal.mark = marks[2],
                                                           nsmall = nDig),
                                                    suffix),
                       breaks = sq,
                       limits = c(minLim, maxLim))

  if (orientation == "hor")
    gg <- gg +
    coord_flip()

  if (is.null(theme)) {
    gg <- gg + tma()
  } else {
    gg <- gg + theme
  }

  gg + theme(legend.position = "none",
             plot.caption = element_text(hjust = 1),
             axis.text.x = element_text(angle = angleText))
}


#' Bar (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' gg_bar_Cat(sampleData("Cat", nrow = 10))
#' @export gg_bar_Cat
gg_bar_Cat <- function(data,
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       horLabel = NULL,
                       verLabel = NULL,
                       horLine = NULL,
                       #horLineLabel = NULL,
                       verLine = NULL,
                       #verLineLabel = NULL,
                       colors = NULL,
                       colorText = "#5A6B72",
                       colorScale = "no",
                       dropNa = FALSE,
                       prefix = NULL,
                       suffix = NULL,
                       highlightValue = NULL,
                       highlightValueColor = NULL,
                       labelRatio = 1,
                       labelWrap = 12,
                       marks = c(".", ","),
                       nDigits = NULL,
                       order = NULL,
                       orientation = "ver",
                       percentage = FALSE,
                       sort = "no",
                       sliceN = NULL,
                       showText = TRUE,
                       sizeText = 3,
                       theme = NULL, ...) {


  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  names(d) <- c(f$dic_$d$label, paste0("count ", f$dic_$d$label))
  gg <- gg_bar_CatNum(data = d,
                      title = title,
                      subtitle = subtitle,
                      caption = caption,
                      horLabel = horLabel,
                      verLabel = verLabel,
                      horLine = horLine,
                      #horLineLabel = NULL,
                      verLine = verLine,
                      #verLineLabel = NULL,
                      agg = "sum",
                      agg_text = " ",
                      colors = colors,
                      colorText = colorText,
                      colorScale = colorScale,
                      dropNa = dropNa,
                      prefix = prefix,
                      suffix = suffix,
                      highlightValue = highlightValue,
                      highlightValueColor = highlightValueColor,
                      labelRatio = labelRatio,
                      labelWrap = labelWrap,
                      marks = marks,
                      nDigits = nDigits,
                      order = order,
                      orientation = orientation,
                      percentage = percentage,
                      sort = sort,
                      sliceN = sliceN,
                      showText = showText,
                      sizeText = sizeText,
                      theme = theme, ...)
  gg
}



#' Bar (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' @examples
#' gg_bar_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export gg_bar_CatCatNum
gg_bar_CatCatNum <- function(data,
                             title = NULL,
                             subtitle = NULL,
                             caption = NULL,
                             horLabel = NULL,
                             verLabel = NULL,
                             horLine = NULL,
                             #horLineLabel = NULL,
                             verLine = NULL,
                             #verLineLabel = NULL,
                             agg = "sum",
                             agg_text = NULL,
                             colors = NULL,
                             colorText = "#5A6B72",
                             colorScale = "discrete",
                             dropNaV = c(FALSE, FALSE),
                             prefix = NULL,
                             suffix = NULL,
                             graphType = "grouped",
                             labelRatio = 1,
                             labelWrapV = c(12, 12),
                             legendPosition = "bottom",
                             legendTitle = NULL,
                             marks = c(".", ","),
                             nDigits = 0,
                             order1 = NULL,
                             order2 = NULL,
                             orientation = "ver",
                             percentage = FALSE,
                             showText = TRUE,
                             sizeText = 3,
                             theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  Lc <- length(unique(d$a))
  angleText <- ifelse( Lc >= 10 & Lc < 15, 45,
                       ifelse(Lc >= 15, 90, 0))

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  legendTitle <- legendTitle %||% nms[1]

  prefix_agg <- ifelse(is.null(agg_text), agg, as.character(agg_text))

  labelsXY <- orientationXY(orientation,
                            x = nms[2],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a) & nrow(d) == dplyr::n_distinct(d$b),
                                       nms[3],
                                       paste(prefix_agg, nms[3])),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          0,
                          0,
                          hor = horLine,
                          ver = verLine)

  if (any(dropNaV))
    d <- d %>%
    tidyr::drop_na(which(dropNaV))

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(percent = c * 100 / sum(c, na.rm = TRUE))

  pd <- position_dodge(width = 0.6)
  if (graphType == "stacked") {
    pd <- "stack"
    labelRatio <- 0.5
    d <- d %>%
      dplyr::mutate(c = ifelse(c == 0, NA, c),
                    percent = ifelse(percent == 0, NA, percent))
  }

  d <- orderCategory(d, "a", orientation, order1, labelWrapV[1])
  d <- orderCategory(d, "b", orientation, order2, labelWrapV[2])

  if (graphType == "grouped") {
    d <- labelPosition(d, "c", labelRatio, percentage, zeroToNa = TRUE)
  }

  fillCol <- fillColors(d, "a", colors, colorScale, NULL, NULL, labelWrapV[1])

  if (percentage & is.null(suffix)) {
    suffix <- "%"
  }

  if (is.null(nDigits)) {
    nDig <- 0
  } else {
    nDig <- nDigits
  }

  varP <- ifelse(percentage, "percent", "c")
  minLim <- ifelse(min(d[[varP]], na.rm = T) < 0, min(d[[varP]], na.rm = T), 0)
  maxLim <- max(d[[varP]], na.rm = T) + 0.3 * max(d[[varP]], na.rm = T)
  # sq <- nchar(round(maxLim - minLim, 0)) - 2
  # minLim <- round(minLim *  10^(-sq), 0) * 10^sq
  # maxLim <- round(maxLim *  10^(-sq), 0) * 10^sq
  # sq <- seq(minLim, maxLim, 10^sq)
  # sq <- unique(c(0, minLim, sq))

  gg <- ggplot(d, aes(x = b, y = d[[ifelse(percentage, "percent", "c")]], fill = a)) +
    # geom_bar(width = 0.5, stat = "identity", position = ifelse(graphType == "stacked", "stack", "dodge")) +
    geom_bar(width = 0.5, stat = "identity", position = pd) +
    # geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = lineXY[2],
               color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)),
                              "#5A6B72",
                              "transparent"),
               linetype = "dashed") +
    geom_hline(yintercept = lineXY[1],
               color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)),
                              "#5A6B72",
                              "transparent"),
               linetype = "dashed") +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = fillCol,
                      name = legendTitle) +
    scale_y_continuous(labels = function(x) paste0(prefix,
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2],
                                                          digits = nDig,
                                                          nsmall = nDig),
                                                   suffix)#,
                       # breaks = sq,
                       # limits = c(minLim, maxLim)) +
                       # breaks = seq(minLim, maxLim, round(maxLim/Lc, nDig)),
                       # limits = c(minLim, maxLim)
                       ) +
    theme(legend.position = legendPosition,
          plot.caption = element_text(hjust = 1))

  if (graphType == "stacked") {
    gg <- gg +
      geom_text(aes(y = d[[ifelse(percentage, "percent", "c")]],
                    label = paste0(prefix,
                                   format(d[[ifelse(percentage, "percent", "c")]],
                                          big.mark = marks[1],
                                          decimal.mark = marks[2],
                                          digits = nDig,
                                          nsmall = nDig),
                                   suffix)),
                check_overlap = TRUE,
                sizeText = sizeText,
                color = ifelse(showText, colorText, "transparent"),
                position = position_stack(vjust = labelRatio))
  } else {
    gg <- gg +
      geom_text(aes(y = labPos,
                    label = paste0(prefix,
                                   format(d[[ifelse(percentage, "percent", "c")]],
                                          big.mark = marks[1],
                                          decimal.mark = marks[2],
                                          digits = nDig,
                                          nsmall = nDig),
                                   suffix)),
                check_overlap = TRUE,
                sizeText = sizeText,
                color = ifelse(showText, colorText, "transparent"),
                position = position_dodge(width = 0.65))
  }
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()


  if (is.null(theme)) {
    gg <- gg + tma()
  } else {
    gg <- gg + theme
  }

  gg + theme(axis.text.x = element_text(angle = angleText),
             legend.position= legendPosition) +
    theme_leg() +
    guides(fill = guide_legend(nrow = 1))
}


#' Bar (categories, ordered categories)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' gg_bar_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export gg_bar_CatCat
gg_bar_CatCat <- function(data,
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          horLabel = NULL,
                          verLabel = NULL,
                          horLine = NULL,
                          #horLineLabel = NULL,
                          verLine = NULL,
                          #verLineLabel = NULL,
                          colors = NULL,
                          colorText = "#5A6B72",
                          colorScale = "discrete",
                          dropNaV = c(FALSE, FALSE),
                          prefix = NULL,
                          suffix = NULL,
                          graphType = "grouped",
                          labelRatio = 1,
                          labelWrapV = c(12, 12),
                          legendPosition = "bottom",
                          legendTitle = NULL,
                          marks = c(".", ","),
                          nDigits = 0,
                          order1 = NULL,
                          order2 = NULL,
                          orientation = "ver",
                          percentage = FALSE,
                          showText = TRUE,
                          sizeText = 3,
                          theme = NULL, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  names(d) <- c(f$dic_$d$label, paste0("count", f$dic_$d$label[1]))
  gg <- gg_bar_CatCatNum(data = d,
                         title = title,
                         subtitle = subtitle,
                         caption = caption,
                         horLabel = horLabel,
                         verLabel = verLabel,
                         horLine = horLine,
                         #horLineLabel = NULL,
                         verLine = verLine,
                         #verLineLabel = NULL,
                         agg = "sum",
                         agg_text = " ",
                         colors = colors,
                         colorText = colorText,
                         colorScale = colorScale,
                         dropNaV = dropNaV,
                         prefix = prefix,
                         suffix = suffix,
                         graphType = graphType,
                         labelRatio = labelRatio,
                         labelWrapV = labelWrapV,
                         legendPosition = legendPosition,
                         legendTitle = legendTitle,
                         marks = marks,
                         nDigits = nDigits,
                         order1 = order1,
                         order2 = order2,
                         orientation = orientation,
                         percentage = percentage,
                         showText = showText,
                         sizeText = sizeText,
                         theme = theme, ...)
  gg
}




#' Bar (ordered category, n numbers)
#'
#' Compare n quantities among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-NumP
#' @examples
#' gg_bar_CatNumP(sampleData("Cat-NumP", nrow = 10))
#' @export gg_bar_CatNumP
gg_bar_CatNumP <- function(data,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           horLabel = NULL,
                           verLabel = NULL,
                           horLine = NULL,
                           #horLineLabel = NULL,
                           verLine = NULL,
                           #verLineLabel = NULL,
                           agg = "sum",
                           agg_text = NULL,
                           colors = NULL,
                           colorText = "#5A6B72",
                           colorScale = "discrete",
                           dropNaV = c(FALSE, FALSE),
                           prefix = NULL,
                           suffix = NULL,
                           graphType = "grouped",
                           labelRatio = 1,
                           labelWrapV = c(12, 12),
                           legendPosition = "bottom",
                           legendTitle = NULL,
                           marks = c(".", ","),
                           nDigits = 0,
                           order1 = NULL,
                           order2 = NULL,
                           orientation = "ver",
                           percentage = FALSE,
                           showText = TRUE,
                           sizeText = 3,
                           theme = NULL, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  names(d) <- f$dic_$d$label

  data <- d %>%
    gather("categories", "count", names(d)[-1])

  gg <- gg_bar_CatCatNum(data,
                         title = title,
                         subtitle = subtitle,
                         caption = caption,
                         horLabel = horLabel,
                         verLabel = verLabel,
                         horLine = horLine,
                         #horLineLabel = NULL,
                         verLine = verLine,
                         #verLineLabel = NULL,
                         agg = agg,
                         agg_text = agg_text,
                         colors = colors,
                         colorText = colorText,
                         colorScale = colorScale,
                         dropNaV = dropNaV,
                         prefix = prefix,
                         suffix = suffix,
                         graphType = graphType,
                         labelRatio = labelRatio,
                         labelWrapV = labelWrapV,
                         legendPosition = legendPosition,
                         legendTitle = legendTitle,
                         marks = marks,
                         nDigits = nDigits,
                         order1 = order1,
                         order2 = order2,
                         orientation = orientation,
                         percentage = percentage,
                         showText = showText,
                         sizeText = sizeText,
                         theme = theme, ...)
  gg
}
