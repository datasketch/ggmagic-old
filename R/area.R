#' Area (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Cat-Num
#' @examples
#' gg_area_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_area_CatNum
gg_area_CatNum <- function(data,
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
                           colorText = "#5A6B72",
                           colorOpacity = 0.7,
                           colorScale = "no",
                           dropNa = FALSE,
                           format = c("", ""),
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
  fillCol <- fillColors(d, "a", colors, colorScale, NULL, NULL, labelWrap)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  d$a <- as.factor(d$a)

  minLim <- min(d[[ifelse(percentage, "percent", "b")]])
  maxLim <- max(d[[ifelse(percentage, "percent", "b")]]) + 0.2 * max(d[[ifelse(percentage, "percent", "b")]])

  gg <- ggplot(d, aes(x=a, group = 1)) +
    geom_area(alpha = colorOpacity, aes(y = d[[ifelse(percentage, "percent", "b")]], fill = "b")) +
    scale_fill_manual(name="",
                      values = c("b" =  unique(fillCol))) +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 1)) +
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
                                        digits = nDigits,
                                        nsmall = nDigits),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_y_continuous(labels =  function(x) paste0(format[1],
                                                    format(x,
                                                           big.mark = marks[1],
                                                           decimal.mark = marks[2],
                                                           digits = nDigits,
                                                           nsmall = nDigits),
                                                    format[2]),
                       limits = c(ifelse(startAtZero, 0, minLim), maxLim)#c(ifelse(startAtZero, 0, NA), NA)
                       )


  gg + tma()
}
