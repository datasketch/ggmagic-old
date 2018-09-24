##### ORDEN DE LAS FUNCIONES... PRIMERO LA DE SORT DESPUÉS LA DE ORDER PARA
# QUE TENGA PRIORIDAD EL SORT
# FALTA HACER EL TEMA, también tiene que admitir temas viejos o ¿¿qué??...

### arreglado el sort de horizontal... colores ejemplo...

# gg_bar_Cat(data.frame(a = letters[1:8]), colors = colorNumeric(c("#009EE3",
# "#E5007D", "#95C11E"), 1:8)(1:8)[sample(8)], diffColorsBar = TRUE)

### script para probar los argumentos de las funciones
# función
# f0 <- gg_bar_Cat
# data <- s0
# order <- "g"
# hg <- c("g", "NA")
# f0(data)
# # # títulos
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO")
# # labels ejes, líneas
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2)
# # orientatio
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor")
# # dropna, format (que le gana a percentage), order, labelwrap, labelRatio
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    dropNa = TRUE, format = c("pre", " &$pos"), order = order, labelWrap = 3,
#    labelRatio = 1)
# # percentage, marks, ndigits
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    dropNa = TRUE, format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4)
# # sort, sliceN, showText, colorText
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    dropNa = TRUE, format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc", sliceN = 1,
#    colorText = "darkgreen")
# # colors
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    dropNa = TRUE, format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc",
#    colorText = "darkgreen", colors = "red")
# # colors, diffColorsBar
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    dropNa = TRUE, format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc",
#    colors = c("blue", "yellow", "orange"), diffColorsBar = TRUE)
# # colors, diffColorsBar
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
# horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
# dropNa = TRUE, format = c("pre", " &$pos"), order = order, percentage = TRUE,
# marks = c(",", "."), nDigits = 4, sort = "asc",
# colors = c("blue", "yellow"), diffColorsBar = TRUE)
# # colors, highlightvalue
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc",
#    colors = "purple", highlightValue = hg)
# # colors, highlightvalue
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc",
#    colors = c("purple", "red"), highlightValue = hg)
# # colors, highlightvalue, diffcolorsbar
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc",
#    colors = c("purple", "red", "yellow"), highlightValue = "NA")

## CAT CAT
# gg_bar_grouped_CatCat(s1, title = "title", subtitle = "subtitle",
#                       caption = "JSHAOFJ JDJ", horLabel = "HORZ",
#                       verLabel = "VERZZZ", orientation = "hor",
#                       colorText = "blue", dropNa = c(FALSE, TRUE),
#                       format = c("sjo ", " ksh&"))

# gg_bar_CatNum <- function(data,
#                           title = NULL,
#                           subtitle = NULL,
#                           caption = NULL,
#                           horLabel = NULL,
#                           verLabel = NULL,
#                           horLine = NULL,
#                           #horLineLabel = NULL,
#                           verLine = NULL,
#                           #verLineLabel = NULL,
#                           agg = "sum",
#                           colors = c("#009EE3", "#F9B233"),
#                           colorText = "black",
#                           diffColorsBar = FALSE,
#                           dropNa = FALSE,
#                           format = c("", ""),
#                           highlightValue = NULL,
#                           highlightValueColor = "#F9B233",
#                           labelRatio = 0.1,
#                           labelWrap = 12,
#                           marks = c(".", ","),
#                           nDigits = 2,
#                           order = NULL,
#                           orientation = "ver",
#                           percentage = FALSE,
#                           sort = "no",
#                           sliceN = NULL,
#                           showText = TRUE,
#                           theme = NULL, ...) {
#   f <- fringe(data)
#   nms <- getClabels(f)
#   d <- f$d
#
#   title <-  title %||% ""
#   subtitle <- subtitle %||% ""
#   caption <- caption %||% ""
#   labelsXY <- orientationXY(orientation,
#                             x = nms[1],
#                             y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2])),
#                             hor = horLabel,
#                             ver = verLabel)
#   lineXY <- orientationXY(orientation,
#                           0,
#                           0,
#                           hor = horLine,
#                           ver = verLine)
#
#   d <- d  %>%
#     tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
#                            b = NA)) %>%
#     dplyr::group_by(a) %>%
#     dplyr::summarise(b = agg(agg, b))
#
#   if (dropNa)
#     d <- d %>%
#     tidyr::drop_na()
#   ### ARREGLAR LO DE PROCENTAJE
#
#   d <- percentColumn(d, "b", percentage, nDigits)
#   d <- orderCategory(d, "a", order, labelWrap)
#   d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
#   d <- labelPosition(d, "b", labelRatio)
#   fillCol <- fillColors(d, "a", colors, diffColorsBar, highlightValue, highlightValueColor, labelWrap)
#
#   if (percentage & nchar(format[2]) == 0) {
#     format[2] <- "%"
#   }
#
#   gg <- ggplot(d, aes(x = a, y = b, fill = a)) +
#     geom_bar(stat = "identity") +
#     geom_vline(xintercept = lineXY[1],#ifelse(orientation == "hor", horLine %||% 0, verLine %||% 0),
#                color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)),
#                               "black",
#                               "transparent"),
#                linetype = "dashed") +
#     geom_hline(yintercept = lineXY[2],#ifelse(orientation == "hor", verLine %||% 0, horLine %||% 0),
#                color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)),
#                               "black",
#                               "transparent"),
#                linetype = "dashed") +
#     geom_text(aes(y = labPos,
#                   label = paste0(format[1],
#                                  format(b, big.mark = marks[1], decimal.mark = marks[2]),
#                                  format[2])),
#               check_overlap = TRUE,
#               color = ifelse(showText, colorText, "transparent")) +
#     labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
#     scale_fill_manual(values = fillCol) +
#     scale_y_continuous(labels =  function(x) paste0(format[1],
#                                                     format(x,
#                                                            big.mark = marks[1],
#                                                            decimal.mark = marks[2]),
#                                                     format[2])) +
#     theme_ds() +
#     theme(legend.position = "none")
#   ### FECHAAA
#   # if (f$getCtypes()[1] == "Dat")
#   #   gg <- gg +
#   #   scale_x_date(labels = date_format("%Y-%m-%d"))
#   if (orientation == "hor")
#     gg <- gg +
#     coord_flip()
#   gg
# }


barCatCat <- function(data,
         title = NULL,
         subtitle = NULL,
         caption = NULL,
         horLabel = NULL,
         verLabel = NULL,
         horLine = NULL,
         #horLineLabel = NULL,
         verLine = NULL,
         #verLineLabel = NULL,
         colors = "#009EE3",
         colorText = "black",
         diffColorsBar = FALSE,
         dropNa = FALSE,
         format = c("", ""),
         highlightValue = NULL,
         highlightValueColor = "#F9B233",
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
    dplyr::summarise(b = n())


  d <- percentColumn(d, "b", percentage, nDigits)
  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d <- labelPosition(d, "b", labelRatio)
  fillCol <- fillColors(d, "a", colors, diffColorsBar, highlightValue, highlightValueColor, labelWrap)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  gg <- ggplot(d, aes(x = a, y = b, fill = a)) +
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
                                 format(b, big.mark = marks[1], decimal.mark = marks[2]),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = fillCol) +
    scale_y_continuous(labels = function(x) paste0(format[1],
                                                   format(x,
                                                          big.mark = marks[1],
                                                          decimal.mark = marks[2]),
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
                          colors = c("#009EE3", "#F9B233"),
                          colorText = "black",
                          diffColorsBar = FALSE,
                          dropNa = FALSE,
                          format = c("", ""),
                          highlightValue = NULL,
                          highlightValueColor = "#F9B233",
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
    dplyr::summarise(b = agg(agg, b))

  ### ARREGLAR LO DE PROCENTAJE

  d <- percentColumn(d, "b", percentage, nDigits)
  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d <- labelPosition(d, "b", labelRatio)
  fillCol <- fillColors(d, "a", colors, diffColorsBar, highlightValue, highlightValueColor, labelWrap)

  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  gg <- ggplot(d, aes(x = a, y = b, fill = a)) +
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
    geom_text(aes(y = labPos,
                  label = paste0(format[1],
                                 format(b, big.mark = marks[1], decimal.mark = marks[2]),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = fillCol) +
    scale_y_continuous(labels =  function(x) paste0(format[1],
                                                    format(x,
                                                           big.mark = marks[1],
                                                           decimal.mark = marks[2]),
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

