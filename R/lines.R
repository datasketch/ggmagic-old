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
                           colors = NULL,
                           colorText = "black",
                           colorScale = "no",
                           dropNa = FALSE,
                           format = c("", ""),
                           highlightValue = NULL,
                           highlightValueColor = NULL,
                           labelRatio = 1,
                           labelWrap = 12,
                           marks = c(".", ","),
                           nDigits = 2,
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
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2])),
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

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b)) %>%
    dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))

  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d <- labelPosition(d, "b", labelRatio, percentage)
  fillCol <- fillColors(d, "a", colors, colorScale, highlightValue, highlightValueColor, labelWrap)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  # if (spline) {
  #   d <- as.data.frame(spline(d))
  # }

  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], colour = "", group = 1)) +
    geom_line() +
    geom_point(type = shapeType) +
    geom_vline(xintercept = lineXY[2],
               color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_hline(yintercept = lineXY[1],
               color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_text(aes(y = labPos,
                  label = paste0(format[1],
                                 format(d[[ifelse(percentage, "percent", "b")]],
                                        big.mark = marks[1],
                                        decimal.mark = marks[2],
                                        digits = nDigits),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_color_manual(values = ifelse(is.null(colors), dsColorsHex()[1], colors)) +
    scale_y_continuous(labels =  function(x) paste0(format[1],
                                                    format(x,
                                                           big.mark = marks[1],
                                                           decimal.mark = marks[2],
                                                           digits = nDigits),
                                                    format[2]),
                       limits = c(ifelse(startAtZero, 0, NA), NA)) +
    theme_ds() +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 1))
  ### FECHAAA
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
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
                        colorText = "black",
                        colorScale = "no",
                        dropNa = FALSE,
                        format = c("", ""),
                        highlightValue = NULL,
                        highlightValueColor = NULL,
                        labelRatio = 1,
                        labelWrap = 12,
                        marks = c(".", ","),
                        nDigits = 2,
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


  data <- data %>%
    dplyr::group_by_(names(data)) %>%
    dplyr::summarise_(b = n())

  names(data)[2] <- paste0("count", names(data[1]))
  gg <- gg_line_CatNum(data,
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
                       colors = colors,
                       colorText = colorText,
                       colorScale = colorScale,
                       dropNa = dropNa,
                       format = format,
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

#' Lines (years, numbers)
#'
#' Compare quantities over years
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_line_YeaNum(sampleData("Yea-Num", nrow = 10))
#' @export gg_line_YeaNum
gg_line_YeaNum <- gg_line_CatNum


#' Lines (dates, numbers)
#'
#' Compare quantities over dates
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_line_DatNum(sampleData("Dat-Num", nrow = 10))
#' @export gg_line_DatNum
gg_line_DatNum <- gg_line_CatNum


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
                              colors = NULL,
                              colorText = "black",
                              colorScale = "discrete",
                              dropNa = c(FALSE, FALSE),
                              format = c("", ""),
                              graphType = "grouped",
                              labelRatio = 1,
                              labelWrap = c(12, 12),
                              legendPosition = "right",
                              legendTitle = NULL,
                              marks = c(".", ","),
                              nDigits = 2,
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

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  legendTitle <- legendTitle %||% nms[2]
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
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

  if (any(dropNa))
    d <- d %>%
    tidyr::drop_na(which(dropNa))

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::mutate(percent = c * 100 / sum(c, na.rm = TRUE))
  View(d)

  if (graphType == "stacked") {
    d <- d %>%
      dplyr::mutate(c = ifelse(c == 0, NA, c))
  }

  d <- orderCategory(d, "a", orientation, order, labelWrap[1])
  d <- labelPosition(d, "c", labelRatio, percentage, zeroToNa = TRUE)
  fillCol <- fillColors(d, "b", colors, colorScale, NULL, NULL, labelWrap[2])

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "c")]], colour = b, group = b)) +
    geom_line() +
    geom_point(type = shapeType) +
    geom_vline(xintercept = lineXY[2],
               color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_hline(yintercept = lineXY[1],
               color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_colour_manual(values = fillCol,
                        name = legendTitle) +
    scale_y_continuous(labels = function(x) paste0(format[1],
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2],
                                                          digits = nDigits),
                                                   format[2]),
                       limits = c(ifelse(startAtZero, 0, NA), NA)) +
    theme_ds() +
    theme(legend.position = legendPosition,
          plot.caption = element_text(hjust = 1))

  if (graphType == "stacked") {
    gg <- gg +
      geom_text(aes(y = d[[ifelse(percentage, "percent", "c")]],
                    label = paste0(format[1],
                                   format(d[[ifelse(percentage, "percent", "c")]],
                                          big.mark = marks[1],
                                          decimal.mark = marks[2],
                                          digits = nDigits),
                                   format[2])),
                check_overlap = TRUE,
                color = ifelse(showText, colorText, "transparent"),
                position = position_stack(vjust = 0.5))
  } else {
    gg <- gg +
      geom_text(aes(y = labPos,
                    label = paste0(format[1],
                                   format(d[[ifelse(percentage, "percent", "c")]],
                                          big.mark = marks[1],
                                          decimal.mark = marks[2],
                                          digits = nDigits),
                                   format[2])),
                check_overlap = TRUE,
                color = ifelse(showText, colorText, "transparent"),
                position = position_dodge(width = 1))
  }
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
  gg
}


#' Lines (catrgories, years, numbers)
#'
#' Compare quantities among categories over years
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' gg_line_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export gg_line_CatYeaNum
gg_line_CatYeaNum <- gg_line_CatCatNum


#' Lines (catrgories, dates, numbers)
#'
#' Compare quantities among categories over dates
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' gg_line_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export gg_line_CatDatNum
gg_line_CatDatNum <- gg_line_CatCatNum


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
                            colors = NULL,
                            colorText = "black",
                            colorScale = "discrete",
                            dropNa = c(FALSE, FALSE),
                            format = c("", ""),
                            graphType = "grouped",
                            labelRatio = 1,
                            labelWrap = c(12, 12),
                            legendPosition = "right",
                            legendTitle = NULL,
                            marks = c(".", ","),
                            nDigits = 2,
                            order = NULL,
                            orientation = "ver",
                            percentage = FALSE,
                            shapeType = 19,
                            showText = TRUE,
                            spline = FALSE,
                            startAtZero = TRUE,
                            theme = NULL, ...) {

  data <- data %>%
    gather("categories", "count", names(data)[-1])
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
                         agg = "sum",
                         colors = colors,
                         colorText = colorText,
                         colorScale = colorScale,
                         dropNa = dropNa,
                         format = format,
                         graphType = graphType,
                         labelRatio = labelRatio,
                         labelWrap = labelWrap,
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



