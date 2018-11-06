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
                       colorText = "black",
                       colorScale = "no",
                       dropNa = FALSE,
                       format = c("", ""),
                       highlightValue = NULL,
                       highlightValueColor = NULL,
                       labelRatio = 0.1,
                       labelWrap = 12,
                       marks = c(".", ","),
                       nDigits = 2,
                       order = NULL,
                       orientation = "ver",
                       percentage = FALSE,
                       sort = "no",
                       sliceN = NULL,
                       showText = TRUE,
                       theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = paste("count", nms[1]),
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
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n()) %>%
    dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))


  # d <- percentColumn(d, "b", percentage, nDigits)
  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d <- labelPosition(d, "b", labelRatio, percentage)
  fillCol <- fillColors(d, "a", colors, colorScale, highlightValue, highlightValueColor, labelWrap)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], fill = a)) +
    geom_bar(stat = "identity") +
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
    scale_fill_manual(values = fillCol) +
    scale_y_continuous(labels = function(x) paste0(format[1],
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2],
                                                          digits = nDigits),
                                                   format[2])) +
    theme_ds() +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 1))
  # DESPUÉS PARA FECHAS
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%b %d %Y"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
  gg
}


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
                          colors = NULL,
                          colorText = "black",
                          colorScale = "no",
                          dropNa = FALSE,
                          format = c("", ""),
                          highlightValue = NULL,
                          highlightValueColor = NULL,
                          labelRatio = 0.1,
                          labelWrap = 12,
                          marks = c(".", ","),
                          nDigits = 2,
                          order = NULL,
                          orientation = "ver",
                          percentage = FALSE,
                          sort = "no",
                          sliceN = NULL,
                          showText = TRUE,
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

    gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], fill = a)) +
      geom_bar(stat = "identity") +
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
      scale_fill_manual(values = fillCol) +
      scale_y_continuous(labels =  function(x) paste0(format[1],
                                                      format(x,
                                                             big.mark = marks[1],
                                                             decimal.mark = marks[2],
                                                             digits = nDigits),
                                                      format[2])) +
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


#' Bar (years, numbers)
#'
#' Compare quantities over years
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Yea-Num
#' @examples
#' gg_bar_YeaNum(sampleData("Yea-Num", nrow = 10))
#' @export gg_bar_YeaNum
gg_bar_YeaNum <- gg_bar_CatNum


#' Bar (dates, numbers)
#'
#' Compare quantities over dates
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Dat-Num
#' @examples
#' gg_bar_DatNum(sampleData("Dat-Num", nrow = 10))
#' @export gg_bar_DatNum
gg_bar_DatNum <- gg_bar_CatNum


#' Grouped bar (categories, ordered categories)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' gg_bar_grouped_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export gg_bar_grouped_CatCat
gg_bar_grouped_CatCat <- function(data,
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
                                  # colors = c("#009EE3", "#F9B233"),
                                  colorText = "black",
                                  dropNa = c(FALSE, FALSE),
                                  format = c("", ""),
                                  # highlightValue = NULL,
                                  labelRatio = 0.1,
                                  labelWrap = c(12, 12),
                                  legendPosition = "right",
                                  marks = c(".", ","),
                                  nDigits = 2,
                                  order1 = NULL,
                                  order2 = NULL,
                                  orientation = "ver",
                                  percentage = FALSE,
                                  # sliceN = NULL,
                                  showText = TRUE,
                                  theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = paste("count", nms[1], nms[2]),
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
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a)

  d <- percentColumn(d, "c", percentage, nDigits)
  d <- orderCategory(d, "a", orientation, order1, labelWrap[1])
  d <- orderCategory(d, "b", orientation, order2, labelWrap[2])
  # d <- sortSlice(d, "c", sort, sliceN)
  d <- labelPosition(d, "c", labelRatio, zeroToNa = TRUE)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  # geom_col(aes(fill = grp), position = "dodge") +
  gg <- ggplot(d, aes(x = a, y = c, fill = b)) +
    # geom_col(aes(fill = b), position = "dodge") +
    # geom_bar(stat = "identity", position = position_dodge(preserve = "single", width = 1)) +
    geom_bar(stat = "identity", position = "dodge") +
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
                                 format(c, big.mark = marks[1], decimal.mark = marks[2]),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent"),
              position = position_dodge(width = 1)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = getPalette()) +
    # scale_x_discrete(limits = d$a) +
    scale_y_continuous(labels = function(x) paste0(format[1],
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2]),
                                                   format[2])) +
    theme_ds() +
    theme(legend.position = legendPosition,
          plot.caption = element_text(hjust = 1))
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
  gg
}


#' Stacked bar (categories, ordered categories)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' gg_bar_stacked_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export gg_bar_stacked_CatCat
gg_bar_stacked_CatCat <- function(data,
                                  title = NULL,
                                  subtitle = NULL,
                                  caption = NULL,
                                  horLabel = NULL,
                                  verLabel = NULL,
                                  horLine = NULL,
                                  #horLineLabel = NULL,
                                  verLine = NULL,
                                  #verLineLabel = NULL,
                                  colors = c("#009EE3", "#F9B233"),
                                  colorText = "black",
                                  dropNa = c(FALSE, FALSE),
                                  format = c("", ""),
                                  # highlightValue = NULL,
                                  labelRatio = 0.1,
                                  labelWrap = c(12, 12),
                                  leyendPosition = "right",
                                  marks = c(".", ","),
                                  nDigits = 2,
                                  order1 = NULL,
                                  order2 = NULL,
                                  orientation = "ver",
                                  percentage = FALSE,
                                  # sliceN = NULL,
                                  showText = TRUE,
                                  theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = paste("count", nms[1], nms[2]),
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
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c))


  d <- percentColumn(d, "c", percentage, nDigits)
  d <- orderCategory(d, "a", orientation, order1, labelWrap[1])
  d <- orderCategory(d, "b", orientation, order2, labelWrap[2])
  # d <- sortSlice(d, "c", sort, sliceN)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  # geom_col(aes(fill = grp), position = "dodge") +
  gg <- ggplot(d, aes(x = a, y = c, fill = b)) +
    # geom_col(aes(fill = b), position = "dodge") +
    # geom_bar(stat = "identity", position = position_dodge(preserve = "single", width = 1)) +
    geom_bar(stat = "identity", position = "stack") +
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
    geom_text(aes(y = c,
                  label = paste0(format[1],
                                 format(c, big.mark = marks[1], decimal.mark = marks[2]),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent"),
              position = position_stack(vjust = 0.5)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = getPalette()) +
    # scale_x_discrete(limits = d$a) +
    scale_y_continuous(labels = function(x) paste0(format[1],
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2]),
                                                   format[2])) +
    theme_ds() +
    theme(legend.position = leyendPosition,
          plot.caption = element_text(hjust = 1))
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
  gg
}


#' 100% Stacked bar (categories, ordered categories)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' gg_bar_stacked_100_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export gg_bar_stacked_100_CatCat
gg_bar_stacked_100_CatCat <- function(data,
                                      title = NULL,
                                      subtitle = NULL,
                                      caption = NULL,
                                      horLabel = NULL,
                                      verLabel = NULL,
                                      horLine = NULL,
                                      #horLineLabel = NULL,
                                      verLine = NULL,
                                      #verLineLabel = NULL,
                                      colors = c("#009EE3", "#F9B233"),
                                      colorText = "black",
                                      dropNa = c(FALSE, FALSE),
                                      format = c("", ""),
                                      # highlightValue = NULL,
                                      labelRatio = 0.1,
                                      labelWrap = c(12, 12),
                                      leyendPosition = "right",
                                      marks = c(".", ","),
                                      nDigits = 2,
                                      order1 = NULL,
                                      order2 = NULL,
                                      orientation = "ver",
                                      percentage = FALSE,
                                      # sliceN = NULL,
                                      showText = TRUE,
                                      theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = paste("count", nms[1], nms[2]),
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
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c))


  d <- percentColumn(d, "c", TRUE, nDigits)
  d <- orderCategory(d, "a", orientation, order1, labelWrap[1])
  d <- orderCategory(d, "b", orientation, order2, labelWrap[2])
  # d <- sortSlice(d, "c", sort, sliceN)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  # geom_col(aes(fill = grp), position = "dodge") +
  gg <- ggplot(d, aes(x = a, y = c, fill = b)) +
    # geom_col(aes(fill = b), position = "dodge") +
    # geom_bar(stat = "identity", position = position_dodge(preserve = "single", width = 1)) +
    geom_bar(stat = "identity", position = "fill") +
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
    geom_text(aes(y = c,
                  label = paste0(format[1],
                                 format(c, big.mark = marks[1], decimal.mark = marks[2]),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent"),
              position = position_stack(vjust = 0.5)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = getPalette()) +
    # scale_x_discrete(limits = d$a) +
    scale_y_continuous(labels = function(x) paste0(format[1],
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2]),
                                                   format[2])) +
    theme_ds() +
    theme(legend.position = leyendPosition,
          plot.caption = element_text(hjust = 1))
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
  gg
}


#' Grouped bar (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' @examples
#' gg_bar_grouped_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export gg_bar_grouped_CatCatNum
gg_bar_grouped_CatCatNum <- function(data,
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
                                     labelRatio = 0.1,
                                     labelWrap = c(12, 12),
                                     legendPosition = "right",
                                     legendTitle = NULL,
                                     marks = c(".", ","),
                                     nDigits = 2,
                                     order1 = NULL,
                                     order2 = NULL,
                                     orientation = "ver",
                                     percentage = FALSE,
                                     showText = TRUE,
                                     theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
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


  d <- orderCategory(d, "a", orientation, order1, labelWrap[1])
  d <- orderCategory(d, "b", orientation, order2, labelWrap[2])
  d <- labelPosition(d, "c", labelRatio, percentage, zeroToNa = TRUE)
  fillCol <- fillColors(d, "b", colors, colorScale, NULL, NULL, labelWrap[2])

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  # geom_col(aes(fill = grp), position = "dodge") +
  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "c")]], fill = b)) +
    # geom_col(aes(fill = b), position = "dodge") +
    # geom_bar(stat = "identity", position = position_dodge(preserve = "single", width = 1)) +
    geom_bar(stat = "identity", position = "dodge") +
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
                                 format(d[[ifelse(percentage, "percent", "c")]],
                                        big.mark = marks[1],
                                        decimal.mark = marks[2],
                                        digits = nDigits),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent"),
              position = position_dodge(width = 1)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = fillCol) +
    # scale_x_discrete(limits = d$a) +
    scale_y_continuous(labels = function(x) paste0(format[1],
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2],
                                                          digits = nDigits),
                                                   format[2])) +
    theme_ds() +
    theme(legend.position = legendPosition,
          legend.title = element_text(legendTitle),
          plot.caption = element_text(hjust = 1))
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
  gg
}


#' Stacked bar (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' @examples
#' gg_bar_stacked_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export gg_bar_stacked_CatCatNum
gg_bar_stacked_CatCatNum <- function(data,
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
                                     labelRatio = 0.1,
                                     labelWrap = c(12, 12),
                                     legendPosition = "right",
                                     legendTitle = NULL,
                                     marks = c(".", ","),
                                     nDigits = 2,
                                     order1 = NULL,
                                     order2 = NULL,
                                     orientation = "ver",
                                     percentage = FALSE,
                                     showText = TRUE,
                                     theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
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
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = c * 100 / sum(c, na.rm = TRUE))

  d <- orderCategory(d, "a", orientation, order1, labelWrap[1])
  d <- orderCategory(d, "b", orientation, order2, labelWrap[2])
  fillCol <- fillColors(d, "b", colors, colorScale, NULL, NULL, labelWrap[2])

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  # geom_col(aes(fill = grp), position = "dodge") +
  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "c")]], fill = b)) +
    # geom_col(aes(fill = b), position = "dodge") +
    # geom_bar(stat = "identity", position = position_dodge(preserve = "single", width = 1)) +
    geom_bar(stat = "identity", position = "stack") +
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
    geom_text(aes(y = d[[ifelse(percentage, "percent", "c")]],
                  label = paste0(format[1],
                                 format(d[[ifelse(percentage, "percent", "c")]],
                                        big.mark = marks[1],
                                        decimal.mark = marks[2],
                                        digits = nDigits),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent"),
              position = position_stack(vjust = 0.5)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = fillCol) +
    scale_y_continuous(labels = function(x) paste0(format[1],
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2],
                                                          digits = nDigits),
                                                   format[2])) +
    theme_ds() +
    theme(legend.position = legendPosition,
          legend.title = element_text(legendTitle),
          plot.caption = element_text(hjust = 1))

  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
  gg
}

#' 100% Stacked bar (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' @examples
#' gg_bar_stacked_100_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export gg_bar_stacked_100_CatCatNum
gg_bar_stacked_100_CatCatNum <- function(data,
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
                                         colorScale = "discrete",
                                         dropNa = c(FALSE, FALSE),
                                         format = c("", ""),
                                         labelRatio = 0.1,
                                         labelWrap = c(12, 12),
                                         legendPosition = "right",
                                         legendTitle = NULL,
                                         marks = c(".", ","),
                                         nDigits = 2,
                                         order1 = NULL,
                                         order2 = NULL,
                                         orientation = "ver",
                                         percentage = FALSE,
                                         showText = TRUE,
                                         theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
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


  d <- orderCategory(d, "a", orientation, order1, labelWrap[1])
  d <- orderCategory(d, "b", orientation, order2, labelWrap[2])
  fillCol <- fillColors(d, "b", colors, colorScale, NULL, NULL, labelWrap[2])

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  # geom_col(aes(fill = grp), position = "dodge") +
  gg <- ggplot(d, aes(x = a, y = c, fill = b)) +
    # geom_col(aes(fill = b), position = "dodge") +
    # geom_bar(stat = "identity", position = position_dodge(preserve = "single", width = 1)) +
    geom_bar(stat = "identity", position = "fill") +
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
    geom_text(aes(y = c,
                  label = paste0(format[1],
                                 format(c, big.mark = marks[1], decimal.mark = marks[2]),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent"),
              position = position_stack(vjust = 0.5)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = getPalette()) +
    # scale_x_discrete(limits = d$a) +
    scale_y_continuous(labels = function(x) paste0(format[1],
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2]),
                                                   format[2])) +
    theme_ds() +
    theme(legend.position = leyendPosition,
          plot.caption = element_text(hjust = 1))
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
  gg
}
