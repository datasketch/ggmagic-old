#' Lines (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_line_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_line_CatNum
gg_line_CatNum <- function(data,
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
                           nDigits = 0,
                           order = NULL,
                           orientation = "ver",
                           percentage = FALSE,
                           shapeType = 19,
                           sort = "no",
                           sliceN = NULL,
                           showText = TRUE,
                           spline = FALSE,
                           startAtZero = TRUE,
                           theme = NULL, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if(!is.null(colors)) {
    colors <- colors[1]
  }

  Lc <- length(unique(d$a))
  angleText <- ifelse( Lc >= 7 & Lc < 15, 45,
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

  d <-  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b)) %>%
    dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))

  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- 'NA'

  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d <- labelPosition(d, "b", labelRatio, percentage)
  fillCol <- fillColors(d, "a", colors, colorScale, highlightValue, highlightValueColor, labelWrap)
  if (percentage & is.null(suffix)) {
    suffix <- "%"
  }

  # if (spline) {
  #   d <- as.data.frame(spline(d))
  # }
  varP <- ifelse(percentage, "percent", "b")
  minLim <- min(d[[varP]], na.rm = T)
  maxLim <- max(d[[varP]], na.rm = T) + 0.3 * max(d[[varP]], na.rm = T)

  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], colour = a, group = 1)) +
    geom_line() +
    geom_point(shape = as.integer(shapeType)) +
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
                                        digits = nDigits,
                                        nsmall = nDigits),
                                 suffix)),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_color_manual(values = fillCol) +
    scale_y_continuous(labels =  function(x) paste0(prefix,
                                                    format(x,
                                                           big.mark = marks[1],
                                                           decimal.mark = marks[2],
                                                           digits = nDigits,
                                                           nsmall = nDigits),
                                                    suffix),
                       breaks = seq(ifelse(startAtZero, 0, minLim), maxLim, round(maxLim/Lc, 2)),
                       limits = c(ifelse(startAtZero, 0, minLim), maxLim))

  if (is.null(theme)) {
    gg <- gg + tma()
  } else {
    gg <- gg + theme
  }
  if (orientation == "hor")
    gg <- gg +
    coord_flip()

  gg <- gg + theme(legend.position = "none",
                   plot.caption = element_text(hjust = 1),
                   axis.text.x = element_text(angle = angleText))
  gg
}


#' Lines (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat
#' @examples
#' gg_line_Cat(sampleData("Cat", nrow = 10))
#' @export gg_line_Cat
gg_line_Cat <- function(data,
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
                        nDigits = 0,
                        order = NULL,
                        orientation = "ver",
                        percentage = FALSE,
                        shapeType = 19,
                        sort = "no",
                        sliceN = NULL,
                        showText = TRUE,
                        spline = FALSE,
                        startAtZero = TRUE,
                        theme = NULL, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  names(d) <- c(f$dic_$d$label, paste0("count ", f$dic_$d$label))

  gg <- gg_line_CatNum(data = d,
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
                       shapeType = shapeType,
                       sort = sort,
                       sliceN = sliceN,
                       showText = showText,
                       spline = spline,
                       startAtZero = startAtZero,
                       theme = theme, ...)
  gg
}



#' Lines (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' @examples
#' gg_line_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export gg_line_CatCatNum
gg_line_CatCatNum <- function(data,
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
                              labelRatio = 1,
                              labelWrapV = c(12, 12),
                              legendPosition = "bottom",
                              legendTitle = NULL,
                              marks = c(".", ","),
                              nDigits = 0,
                              order = NULL,
                              orientation = "ver",
                              percentage = FALSE,
                              shapeType = 19,
                              showText = TRUE,
                              spline = FALSE,
                              startAtZero = TRUE,
                              theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  Lc <- length(unique(d$a))
  angleText <- ifelse( Lc >= 7 & Lc < 15, 45,
                       ifelse(Lc >= 15, 90, 0))

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  legendTitle <- legendTitle %||% nms[1]
  labelsXY <- orientationXY(orientation,
                            x = nms[2],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a) & nrow(d) == dplyr::n_distinct(d$b),
                                       nms[3],
                                       paste(agg, nms[3])),
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
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::mutate(percent = c * 100 / sum(c, na.rm = TRUE))

  d$c[is.na(d$c)] <- NA
  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- NA
  d$b <- as.character(d$b)
  d$b[is.na(d$b)] <- NA
  d <- orderCategory(d, "a", orientation, order, labelWrapV[1])
  d <- orderCategory(d, "b", orientation, NULL, labelWrapV[2])
  d <- labelPosition(d, "c", labelRatio, percentage, zeroToNa = TRUE)
  fillCol <- fillColors(d, "a", colors, colorScale, NULL, NULL, labelWrapV[1])

  if (percentage & is.null(suffix)) {
    suffix <- "%"
  }

  varP <- ifelse(percentage, "percent", "c")
  minLim <- min(d[[varP]], na.rm = T)
  maxLim <- max(d[[varP]], na.rm = T) + 0.3 * max(d[[varP]], na.rm = T)

  gg <- ggplot(d, aes(x = b, y = d[[ifelse(percentage, "percent", "c")]], colour = a, group = a)) +
    geom_line() +
    geom_point(shape = as.integer(shapeType)) +
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
                                 format(d[[ifelse(percentage, "percent", "c")]],
                                        big.mark = marks[1],
                                        decimal.mark = marks[2],
                                        digits = nDigits,
                                        nsmall = nDigits),
                                 suffix)),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent"),
              position = position_dodge(width = 1)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_colour_manual(values = fillCol,
                        name = legendTitle) +
    scale_y_continuous(labels = function(x) paste0(prefix,
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2],
                                                          digits = nDigits,
                                                          nsmall = nDigits),
                                                   suffix),
                       breaks = seq(ifelse(startAtZero, 0, minLim), maxLim, round(maxLim/Lc, 2)),
                       limits = c(ifelse(startAtZero, 0, minLim), maxLim))

  if (is.null(theme)) {
    gg <- gg + tma()
  } else {
    gg <- gg + theme
  }
  if (orientation == "hor")
    gg <- gg +
    coord_flip()

  gg + theme(axis.text.x = element_text(angle = angleText),
             legend.position= legendPosition) +
    theme_leg() +
    guides(colour = guide_legend(nrow = 1))
}


#' Lines (categories, ordered categories)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' gg_line_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export gg_line_CatCat
gg_line_CatCat <- function(data,
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
                           labelRatio = 1,
                           labelWrapV = c(12, 12),
                           legendPosition = "bottom",
                           legendTitle = NULL,
                           marks = c(".", ","),
                           nDigits = 0,
                           order = NULL,
                           orientation = "ver",
                           percentage = FALSE,
                           shapeType = 19,
                           showText = TRUE,
                           spline = FALSE,
                           startAtZero = TRUE,
                           theme = NULL, ...) {


  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  names(d) <- c(f$dic_$d$label, paste0("count", f$dic_$d$label[1]))


  gg <- gg_line_CatCatNum(data = d,
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
                          highlightValue = highlightValue,
                          highlightValueColor = highlightValueColor,
                          labelRatio = labelRatio,
                          labelWrapV = labelWrapV,
                          marks = marks,
                          nDigits = nDigits,
                          order = order,
                          orientation = orientation,
                          percentage = percentage,
                          shapeType = shapeType,
                          showText = showText,
                          spline = spline,
                          startAtZero = startAtZero,
                          theme = theme, ...)
  gg
}


#' Lines (ordered category, n numbers)
#'
#' Compare n quantities among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-NumP
#' @examples
#' gg_line_CatNumP(sampleData("Cat-NumP", nrow = 10))
#' @export gg_line_CatNumP
gg_line_CatNumP <- function(data,
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
                            labelRatio = 1,
                            labelWrapV = c(12, 12),
                            legendPosition = "bottom",
                            legendTitle = NULL,
                            marks = c(".", ","),
                            nDigits = 0,
                            order = NULL,
                            orientation = "ver",
                            percentage = FALSE,
                            shapeType = 19,
                            showText = TRUE,
                            spline = FALSE,
                            startAtZero = TRUE,
                            theme = NULL, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  names(d) <- f$dic_$d$label

  data <- d %>%
    gather("categories", "count", names(d)[-1])
  gg <- gg_line_CatCatNum(data,
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
                         labelRatio = labelRatio,
                         labelWrapV = labelWrapV,
                         legendPosition = legendPosition,
                         legendTitle = legendTitle,
                         marks = marks,
                         nDigits = nDigits,
                         order = order,
                         orientation = orientation,
                         percentage = percentage,
                         shapeType = shapeType,
                         sort = sort,
                         sliceN = sliceN,
                         showText = showText,
                         spline = spline,
                         startAtZero = startAtZero,
                         theme = theme, ...)
  gg
}



