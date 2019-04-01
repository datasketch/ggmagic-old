#' Pie (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_pie_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_pie_CatNum
gg_pie_CatNum <- function(data,
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          agg = "sum",
                          colors = NULL,
                          colorText = "black",
                          colorScale = "discrete",
                          dropNa = FALSE,
                          format = c("", ""),
                          labelRatio = 0.1,
                          labelWrap = 12,
                          legendPosition = "right",
                          legendTitle = NULL,
                          marks = c(".", ","),
                          nDigits = 0,
                          order = NULL,
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

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b)) %>%
    dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))%>%
    dplyr::mutate(b = ifelse(b == 0, NA, b),
                  percent = ifelse(percent == 0, NA, percent))

  d <- sortSlice(d, "b", "a", "ver", sort, sliceN)
  d <- orderCategory(d, "a", "ver", order, labelWrap)
  fillCol <- fillColors(d, "a", colors, colorScale, NULL, NULL, labelWrap)
  lb0 <- paste0(format(d$b,
                       big.mark = marks[1],
                       decimal.mark = marks[2],
                       digits = nDigits),
                " (")
  lb1 <- "%)"
  if (percentage) {
    lb0 <- ""
    lb1 <- ""
    if (nchar(format[2]) == 0) {
    format[2] <- "%"
    }
  }

  gg <- ggplot(d, aes(x = factor(1), weight = b, fill = a)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(y = b,
                  label = paste0(lb0,
                                 format[1],
                                 format(d$percent,
                                        big.mark = marks[1],
                                        decimal.mark = marks[2],
                                        digits = nDigits,
                                        nsmall = nDigits),
                                 format[2],
                                 lb1)),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent"),
              position = position_stack(vjust = 0.5)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = "", y = "") +
    scale_fill_manual(values = fillCol,
                      name = legendTitle) +
    scale_y_continuous(labels =  function(x) paste0(format[1],
                                                    format(x,
                                                           big.mark = marks[1],
                                                           decimal.mark = marks[2],
                                                           digits = nDigits,
                                                           nsmall = nDigits),
                                                    format[2])) +
    theme_ds() +
    #theme_ds_clean() +
    theme(legend.position = legendPosition,
          plot.caption = element_text(hjust = 1))

  if (is.null(theme)) {
    gg <- gg + tma() + theme_ds_clean()
  } else {
    gg <- gg + theme
  }

  gg
}



#' Pie (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' gg_pie_Cat(sampleData("Cat", nrow = 10))
#' @export gg_pie_Cat
gg_pie_Cat <- function(data,
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       colors = NULL,
                       colorText = "black",
                       colorScale = "discrete",
                       dropNa = FALSE,
                       format = c("", ""),
                       labelRatio = 0.1,
                       labelWrap = 12,
                       legendPosition = "right",
                       legendTitle = NULL,
                       marks = c(".", ","),
                       nDigits = 0,
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

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  names(d) <- c(f$dic_$d$label, paste0("count ", f$dic_$d$label))

  gg <- gg_pie_CatNum(data = d,
                      title = title,
                      subtitle = subtitle,
                      caption = caption,
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
                      legendPosition = legendPosition,
                      legendTitle = legendTitle,
                      marks = marks,
                      nDigits = nDigits,
                      order = order,
                      orientation = orientation,
                      percentage = percentage,
                      sort = sort,
                      sliceN = sliceN,
                      showText = showText,
                      theme = theme, ...)
  gg
}



#' Donut (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_donut_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_donut_CatNum
gg_donut_CatNum <- function(data,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            agg = "sum",
                            colors = NULL,
                            colorText = "black",
                            colorScale = "discrete",
                            dropNa = FALSE,
                            format = c("", ""),
                            labelRatio = 0.1,
                            labelWrap = 12,
                            legendPosition = "right",
                            legendTitle = NULL,
                            marks = c(".", ","),
                            nDigits = 0,
                            order = NULL,
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

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b)) %>%
    dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))%>%
    dplyr::mutate(b = ifelse(b == 0, NA, b),
                  percent = ifelse(percent == 0, NA, percent))

  d <- sortSlice(d, "b", "a", "ver", sort, sliceN)
  d <- orderCategory(d, "a", "ver", order, labelWrap)
  fillCol <- fillColors(d, "a", colors, colorScale, NULL, NULL, labelWrap)
  lb0 <- paste0(format(d$b,
                       big.mark = marks[1],
                       decimal.mark = marks[2],
                       digits = nDigits),
                " (")
  lb1 <- "%)"
  if (percentage) {
    lb0 <- ""
    lb1 <- ""
    if (nchar(format[2]) == 0) {
      format[2] <- "%"
    }
  }

  gg <- ggplot(d, aes(x = 2, y = b, fill = a)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "y") +
    xlim(c(0.5, 2.5)) +
    geom_text(aes(y = b,
                  label = paste0(lb0,
                                 format[1],
                                 format(d$percent,
                                        big.mark = marks[1],
                                        decimal.mark = marks[2],
                                        digits = nDigits,
                                        nsmall = nDigits),
                                 format[2],
                                 lb1)),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent"),
              position = position_stack(vjust = 0.5)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = "", y = "") +
    scale_fill_manual(values = fillCol,
                      name = legendTitle) +
    scale_y_continuous(labels =  function(x) paste0(format[1],
                                                    format(x,
                                                           big.mark = marks[1],
                                                           decimal.mark = marks[2],
                                                           digits = nDigits,
                                                           nsmall = nDigits),
                                                    format[2])) +
    theme_ds() +
    #theme_ds_clean() +
    theme(legend.position = legendPosition,
          plot.caption = element_text(hjust = 1))

  if (is.null(theme)) {
    gg <- gg + tma() + theme_ds_clean()
  } else {
    gg <- gg + theme
  }
  gg
}



#' Donut (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' gg_donut_Cat(sampleData("Cat", nrow = 10))
#' @export gg_donut_Cat
gg_donut_Cat <- function(data,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         colors = NULL,
                         colorText = "black",
                         colorScale = "discrete",
                         dropNa = FALSE,
                         format = c("", ""),
                         labelRatio = 0.1,
                         labelWrap = 12,
                         legendPosition = "right",
                         legendTitle = NULL,
                         marks = c(".", ","),
                         nDigits = 0,
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

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  names(d) <- c(f$dic_$d$label, paste0("count ", f$dic_$d$label))

  gg <- gg_donut_CatNum(data = d,
                        title = title,
                        subtitle = subtitle,
                        caption = caption,
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
                        legendPosition = legendPosition,
                        legendTitle = legendTitle,
                        marks = marks,
                        nDigits = nDigits,
                        order = order,
                        orientation = orientation,
                        percentage = percentage,
                        sort = sort,
                        sliceN = sliceN,
                        showText = showText,
                        theme = theme, ...)
  gg
}

