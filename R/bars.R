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
                       angleX = 0,
                       colors = c("#009EE3", "#F9B233"),
                       dropNa = FALSE,
                       format = c("", ""),
                       highlightValue = NULL,
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
    dplyr::summarise(b = n())

  d <- percentColumn(d, "b", percentage)
  if (percentage) {
    d <- d[, c(1, 3, 3)]
    names(d)[1:2] <- c("a", "b")
  }
  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "b", sort, sliceN)

  gg <- ggplot(d, aes(x = a, y = b, fill = a %in% highlightValue)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = 1,
               color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_hline(yintercept = 1,
               color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_text(aes(y = b - b / 2,
                  label = paste0(ifelse(is.na(format[1]), "", format[1]),
                                 d[[ifelse(percentage, "percent", "b")]],
                                 ifelse(percentage, "%", ifelse(is.na(format[2]), "", format[2])))),
              check_overlap = TRUE,
              color = ifelse(showText, "black", "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = colors) +
    scale_x_discrete(limits = d$a) +
    # scale_y_continuous(labels = dollar_format(prefix = ifelse(is.na(format[1]), "", format[1]),
    #                                           suffix = ifelse(is.na(format[2]), "", format[2]))) +
    scale_y_continuous(limits = c(0, max(d$percent)),
      labels = ifelse(percentage,
                                       percent,
                                       dollar_format(prefix = ifelse(is.na(format[1]), "", format[1]),
                                                     suffix = ifelse(is.na(format[2]), "", format[2])))) +
    theme_ds() +
    theme(legend.position = "none", axis.text.x = element_text(angle = angleX))
    guides(fill = FALSE)
  if (f$getCtypes()[1] == "Dat")
    gg <- gg +
    scale_x_date(labels = date_format("%b %d %Y"))
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
                          angleX = 0,
                          colors = c("#009EE3", "#F9B233"),
                          dropNa = FALSE,
                          format = c("", ""),
                          highlightValue = NULL,
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
    dplyr::summarise(b = agg(agg, b))

  d <- percentColumn(d, "b", percentage)
  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "b", sort, sliceN)

  gg <- ggplot(d, aes(x = a, y = b, fill = a %in% highlightValue)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = lineXY[1],#ifelse(orientation == "hor", horLine %||% 0, verLine %||% 0),
               color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_hline(yintercept = lineXY[2],#ifelse(orientation == "hor", verLine %||% 0, horLine %||% 0),
               color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_text(aes(y = b - b / 2,
                  label = paste0(ifelse(is.na(format[1]), "", format[1]),
                                 d[[ifelse(percentage, "percent", "b")]],
                                 ifelse(percentage, "%", ifelse(is.na(format[2]), "", format[2])))),
              check_overlap = TRUE,
              color = ifelse(showText, "black", "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = colors) +
    scale_x_discrete(limits = d$a) +
    scale_y_continuous(labels = dollar_format(prefix = ifelse(is.na(format[1]), "", format[1]),
                                              suffix = ifelse(is.na(format[2]), "", format[2]))) +
    theme_ds() +
    theme(legend.position = "none", axis.text.x = element_text(angle = angleX)) +
    guides(fill = FALSE)
  if (f$getCtypes()[1] == "Dat")
    gg <- gg +
    scale_x_date(labels = date_format("%Y-%m-%d"))
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
                                  angleX = 0,
                                  colors = c("#009EE3", "#F9B233"),
                                  dropNa = FALSE,
                                  format = c("", ""),
                                  leyendLayout = "right",
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
                            y = paste("count", nms[2]),
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
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n())

  d <- percentColumn(d, "c", percentage)
  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "c", sort, sliceN)


  # geom_col(aes(fill = grp), position = "dodge") +
  gg <- ggplot(d, aes(x = a, y = c, group = b)) +
    geom_col(aes(fill = b), position = "dodge") +
    geom_bar(stat = "identity", position = "stack") +
    geom_vline(xintercept = lineXY[1],#ifelse(orientation == "hor", horLine %||% 0, verLine %||% 0),
               color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_hline(yintercept = lineXY[2],#ifelse(orientation == "hor", verLine %||% 0, horLine %||% 0),
               color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_text(aes(label = paste0(ifelse(is.na(format[1]), "", format[1]),
                                 d[[ifelse(percentage, "percent", "c")]],
                                 ifelse(percentage, "%", ifelse(is.na(format[2]), "", format[2])))),
              position = position_stack(vjust = 0.5),
              check_overlap = TRUE,
              color = ifelse(showText, "black", "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = getPalette()) +
    scale_x_discrete(limits = d$a) +
    scale_y_continuous(labels = dollar_format(prefix = ifelse(is.na(format[1]), "", format[1]),
                                              suffix = ifelse(is.na(format[2]), "", format[2]))) +
    theme_ds() +
    theme(legend.position = leyendLayout, axis.text.x = element_text(angle = angleX)) +
    guides(fill = FALSE)
  # gg <- ggplot(d, aes(x = a, y = c, fill = b)) +
  #   geom_bar(stat = "identity", position = "stack") +
  #   geom_vline(xintercept = lineXY[1],#ifelse(orientation == "hor", horLine %||% 0, verLine %||% 0),
  #              color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)),
  #                             "black",
  #                             "transparent"),
  #              linetype = "dashed") +
  #   geom_hline(yintercept = lineXY[2],#ifelse(orientation == "hor", verLine %||% 0, horLine %||% 0),
  #              color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)),
  #                             "black",
  #                             "transparent"),
  #              linetype = "dashed") +
  #   geom_text(aes(label = paste0(ifelse(is.na(format[1]), "", format[1]),
  #                                d[[ifelse(percentage, "percent", "c")]],
  #                                ifelse(percentage, "%", ifelse(is.na(format[2]), "", format[2])))),
  #             position = position_stack(vjust = 0.5),
  #             check_overlap = TRUE,
  #             color = ifelse(showText, "black", "transparent")) +
  #   labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
  #   scale_fill_manual(values = getPalette()) +
  #   scale_x_discrete(limits = d$a) +
  #   scale_y_continuous(labels = dollar_format(prefix = ifelse(is.na(format[1]), "", format[1]),
  #                                             suffix = ifelse(is.na(format[2]), "", format[2]))) +
  #   theme_ds() +
  #   theme(legend.position = leyendLayout, axis.text.x = element_text(angle = angleX)) +
  #   guides(fill = FALSE)
  if (f$getCtypes()[1] == "Dat")
    gg <- gg +
    scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
  gg
}







