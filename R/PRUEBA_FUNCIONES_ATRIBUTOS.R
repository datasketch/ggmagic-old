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


# gg_bar_Cat(s0, title = "title", subtitle = "skrkrj",
#            caption = "sjjsfasdf ", colorScale = "no",
#            highlightValueColor = "blue", dropNa = TRUE, sort = "aasc",
#            order = "X_C", orientation = "ver", percentage = TRUE, nDigits = 5,
#            colors = c("yellow", "orange"))

## PRUEBA CAT CAT NUM
# FALTA LEGEND TITLE

### PREGUNTAS ----- stacked con percentage TRUE no es stacked 100?¿?¿?¿
# poner sólo bar CATCATNUM y como parámetro stacked, grouped, 100

# s0 <- rbind(sampleData("Cat-Cat-Num"),
#             c("ajs sjjsj sjjsjsj sj", "sjaja ja ajds j sjs", 1432))
# gg_bar_grouped_CatCatNum(s0, title = "title", subtitle = "skrkrj",
#                          caption = "sjjsfasdf", horLabel = " HOR",
#                          verLabel = " VER", agg = "sum", dropNa = c(TRUE, TRUE),
#                          labelRatio = 1, legendPosition = "right", legendTitle = "d",
#                          marks = c(",", "."), percentage = TRUE, nDigits = 4,
#                          order1 = "CatE", order2 = "TypeD", colorScale = "continuous",
#                          colors = c("red", "yellow", "brown", "blue"), labelWrap = c(20, 40))


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
