library(devtools)
load_all()
document()
install()

library(hgchmagic)
library(ggmagic)

s0 <- sampleData("Cat")

# faltan las divisiones del eje y (calcularlas) ggplot

gg_bar_Cat(s0, orientation = "hor")
hgch_bar_Cat(s0, orientation = "hor") # color scale no... salen distintos
gg_bar_Cat(s0, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj", colorScale = "discrete")
hgch_bar_Cat(s0, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj", colorScale = "discrete")
gg_bar_Cat(s0, labelRatio = 1, colorScale = "no", colors = c("blue", "red"))
hgch_bar_Cat(s0, labelRatio = 1, colorScale = "no", colors = c("blue", "red")) # ¿no funciona el color?

gg_bar_Cat(s0, percentage = TRUE)
hgch_bar_Cat(s0, percentage = TRUE)
gg_bar_Cat(s0, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous")
hgch_bar_Cat(s0, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous") # parece que no sale escala continua
gg_bar_Cat(s0, labelRatio = 1, percentage = TRUE, suffix ="pres")
hgch_bar_Cat(s0, labelRatio = 1, percentage = TRUE, suffix ="pres") # no sale sufijo que reemplazaría porcentaje

gg_area_Cat(s0) # los labels salen un poco montados en los puntos, si se agranda la ventana se arregla
hgch_area_Cat(s0) # color default amarillo en hgchmagic, verde en ggmagic
gg_area_Cat(s0, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj")
hgch_area_Cat(s0, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj") # no salen sufijo ni prefixo en labels
gg_area_Cat(s0, labelRatio = 1, colorScale = "no", colors = c("blue", "red"))
hgch_area_Cat(s0, labelRatio = 1, colorScale = "no", colors = c("blue", "red")) # ¿no funciona el color?

gg_area_Cat(s0, percentage = TRUE)
hgch_area_Cat(s0, percentage = TRUE) # no sale porcentaje en labels ni sufijo
gg_area_Cat(s0, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous") # salen los puntos de colores continuous, ¿no debería pasar?
hgch_area_Cat(s0, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous") # no sale sufijos en labels (en particular el porcentaje)
gg_area_Cat(s0, labelRatio = 1, percentage = TRUE, suffix ="pres")
hgch_area_Cat(s0, labelRatio = 1, percentage = TRUE, suffix ="pres")

gg_line_Cat(s0) # líneas y pie no sirven es por algo del tema... no han sido modificada, 'alinearlas' con area
hgch_line_Cat(s0)
gg_line_Cat(s0, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj", colorScale = "discrete")
hgch_line_Cat(s0, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj", colorScale = "discrete")
gg_line_Cat(s0, labelRatio = 1, colorScale = "no", colors = c("blue", "red"))
hgch_line_Cat(s0, labelRatio = 1, colorScale = "no", colors = c("blue", "red"))

gg_line_Cat(s0, percentage = TRUE)
hgch_line_Cat(s0, percentage = TRUE)
gg_line_Cat(s0, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous")
hgch_line_Cat(s0, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous")
gg_line_Cat(s0, labelRatio = 1, percentage = TRUE, suffix ="pres")
hgch_line_Cat(s0, labelRatio = 1, percentage = TRUE, suffix ="pres")

s1 <- sampleData("Cat-Num")
s1 <- rbind(s1, c("tT", 2))

gg_bar_CatNum(s1)
hgch_bar_CatNum(s1)
gg_bar_CatNum(s1, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj", colorScale = "discrete")
hgch_bar_CatNum(s1, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj", colorScale = "discrete")
gg_bar_CatNum(s1, labelRatio = 1, colorScale = "no", colors = c("blue", "red"))
hgch_bar_CatNum(s1, labelRatio = 1, colorScale = "no", colors = c("blue", "red")) # salen distintos colores y no salen los marks (punto en miles) en los labels

gg_bar_CatNum(s1, percentage = TRUE)
hgch_bar_CatNum(s1, percentage = TRUE)
gg_bar_CatNum(s1, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous")
hgch_bar_CatNum(s1, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous")
gg_bar_CatNum(s1, labelRatio = 1, percentage = TRUE, suffix ="pres")
hgch_bar_CatNum(s1, labelRatio = 1, percentage = TRUE, suffix ="pres") # prioridad sufijo

gg_area_CatNum(s1)
hgch_area_CatNum(s1)
gg_area_CatNum(s1, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj")
hgch_area_CatNum(s1, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj")
gg_area_CatNum(s1, labelRatio = 1, colorScale = "no", colors = c("blue", "red"))
hgch_area_CatNum(s1, labelRatio = 1, colorScale = "no", colors = c("blue", "red"))

gg_area_CatNum(s1, percentage = TRUE)
hgch_area_CatNum(s1, percentage = TRUE) #falta porcentaje, sufijos en labels
gg_area_CatNum(s1, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous")
hgch_area_CatNum(s1, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous")
gg_area_CatNum(s1, labelRatio = 1, percentage = TRUE, suffix ="pres")
hgch_area_CatNum(s1, labelRatio = 1, percentage = TRUE, suffix ="pres")

gg_line_CatNum(s1)
hgch_line_CatNum(s1)
gg_line_CatNum(s1, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj", colorScale = "discrete")
hgch_line_CatNum(s1, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj", colorScale = "discrete")
gg_line_CatNum(s1, labelRatio = 1, colorScale = "no", colors = c("blue", "red"))
hgch_line_CatNum(s1, labelRatio = 1, colorScale = "no", colors = c("blue", "red"))

gg_line_CatNum(s1, percentage = TRUE)
hgch_line_CatNum(s1, percentage = TRUE)
gg_line_CatNum(s1, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous")
hgch_line_CatNum(s1, labelRatio = 0.5, percentage = TRUE, colorScale = "continuous")
gg_line_CatNum(s1, labelRatio = 1, percentage = TRUE, suffix ="pres")
hgch_line_CatNum(s1, labelRatio = 1, percentage = TRUE, suffix ="pres")

s2 <- sampleData("Cat-Cat")

# mejorar la división de los ejes
gg_bar_CatCat(s2)
hgch_bar_CatCat(s2)
gg_bar_CatCat(s2, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj")
hgch_bar_CatCat(s2, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj")
gg_bar_CatCat(s2, labelRatio = 1)
hgch_bar_CatCat(s2, labelRatio = 1)


# REVISAR
gg_bar_CatCat(s2, percentage = TRUE)
hgch_bar_CatCat(s2, percentage = TRUE)
gg_bar_CatCat(s2, labelRatio = 0.5, percentage = TRUE, legendPosition = "top")
hgch_bar_CatCat(s2, labelRatio = 0.5, percentage = TRUE, legendPosition = "right")
gg_bar_CatCat(s2, labelRatio = 1, percentage = TRUE, suffix ="pres")
hgch_bar_CatCat(s2, labelRatio = 1, percentage = TRUE, suffix ="pres")


gg_bar_CatCat(s2, graphType = "stacked") +
  scale_y_continuous(limits = c(0, 200))
hgch_bar_CatCat(s2, graphType = "stacked")
gg_bar_CatCat(s2, graphType = "stacked", labelRatio = 0.1, prefix = "dskk", suffix = " jdjdj") +
  scale_y_continuous(limits = c(0, 200))
hgch_bar_CatCat(s2, graphType = "stacked", labelRatio = 0.1, prefix = "dskk", suffix = " jdjdj")
gg_bar_CatCat(s2, graphType = "stacked", labelRatio = 1) +
  scale_y_continuous(limits = c(0, 200))
hgch_bar_CatCat(s2, graphType = "stacked", labelRatio = 1)

gg_bar_CatCat(s2, graphType = "stacked", percentage = TRUE) +
  scale_y_continuous(limits = c(0, 100))
hgch_bar_CatCat(s2, graphType = "stacked", percentage = TRUE)
gg_bar_CatCat(s2, graphType = "stacked", labelRatio = 0.1, percentage = TRUE, legendPosition = "top") +
  scale_y_continuous(limits = c(0, 100))
hgch_bar_CatCat(s2, graphType = "stacked", labelRatio = 0.1, percentage = TRUE, legendPosition = "right")
gg_bar_CatCat(s2, graphType = "stacked", labelRatio = 1, percentage = TRUE, suffix ="pres") +
  scale_y_continuous(limits = c(0, 100))
hgch_bar_CatCat(s2, graphType = "stacked", labelRatio = 1, percentage = TRUE, suffix ="pres")

gg_area_CatCat(s2)
hgch_area_CatCat(s2)
gg_area_CatCat(s2, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj")
hgch_area_CatCat(s2, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj") # faltan prefijos y sufijos en labels
gg_area_CatCat(s2, labelRatio = 1)
hgch_area_CatCat(s2, labelRatio = 1)

# hay una diferencia entre los dos paquetes con el porcentaje, mirar fotos
gg_area_CatCat(s2, percentage = TRUE)
hgch_area_CatCat(s2, percentage = TRUE)
gg_area_CatCat(s2, labelRatio = 0.5, percentage = TRUE, legendPosition = "top")
hgch_area_CatCat(s2, labelRatio = 0.5, percentage = TRUE, legendPosition = "right")
gg_area_CatCat(s2, labelRatio = 1, percentage = TRUE, suffix ="pres")
hgch_area_CatCat(s2, labelRatio = 1, percentage = TRUE, suffix ="pres")

# la gráfica es diferente mirar fotos
gg_area_CatCat(s2, graphType = "stacked")
hgch_area_CatCat(s2, graphType = "stacked")
gg_area_CatCat(s2, labelRatio = 0.5, graphType = "stacked", prefix = "dskk", suffix = " jdjdj") # faltan sufijos y prefijos en el eje y
hgch_area_CatCat(s2, labelRatio = 0.5, graphType = "stacked", prefix = "dskk", suffix = " jdjdj")
gg_area_CatCat(s2, labelRatio = 1, graphType = "stacked")
hgch_area_CatCat(s2, labelRatio = 1, graphType = "stacked")

# REVISAR
gg_area_CatCat(s2, graphType = "stacked", percentage = TRUE)
hgch_area_CatCat(s2, graphType = "stacked", percentage = TRUE)
gg_area_CatCat(s2, labelRatio = 0.5, graphType = "stacked", percentage = TRUE, legendPosition = "top")
hgch_area_CatCat(s2, labelRatio = 0.5, graphType = "stacked", percentage = TRUE, legendPosition = "right")
gg_area_CatCat(s2, labelRatio = 1, graphType = "stacked", percentage = TRUE, suffix ="pres")
hgch_area_CatCat(s2, labelRatio = 1, graphType = "stacked", percentage = TRUE, suffix ="pres")

gg_line_CatCat(s2)
hgch_line_CatCat(s2)
gg_line_CatCat(s2, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj")
hgch_line_CatCat(s2, labelRatio = 0.5, prefix = "dskk", suffix = " jdjdj")
gg_line_CatCat(s2, labelRatio = 1)
hgch_line_CatCat(s2, labelRatio = 1)

gg_line_CatCat(s2, percentage = TRUE)
hgch_line_CatCat(s2, percentage = TRUE)
gg_line_CatCat(s2, labelRatio = 0.5, percentage = TRUE, legendPosition = "top")
hgch_line_CatCat(s2, labelRatio = 0.5, percentage = TRUE, legendPosition = "right")
gg_line_CatCat(s2, labelRatio = 1, percentage = TRUE, suffix ="pres")
hgch_line_CatCat(s2, labelRatio = 1, percentage = TRUE, suffix ="pres")




gg_line_CatNum
function (data, title = NULL, subtitle = NULL, caption = NULL,
          horLabel = NULL, verLabel = NULL, horLine = NULL, verLine = NULL,
          agg = "sum", colors = NULL, colorText = "black", colorScale = "no",
          dropNa = FALSE, format = c("", ""), highlightValue = NULL,
          highlightValueColor = NULL, labelRatio = 1, labelWrap = 12,
          marks = c(".", ","), nDigits = 0, order = NULL, orientation = "ver",
          percentage = FALSE, shapeType = 19, sort = "no", sliceN = NULL,
          showText = TRUE, spline = FALSE, startAtZero = TRUE, theme = NULL,
          ...)
{
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  title <- title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2])),
                            hor = horLabel, ver = verLabel)
  lineXY <- orientationXY(orientation,
                          0,
                          0,
                          hor = horLine,
                          ver = verLine)
  if (dropNa)
    d <- d %>% tidyr::drop_na()
  d <- d %>% tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                                    b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg,
                             b)) %>% dplyr::mutate(percent = b * 100/sum(b, na.rm = TRUE))
  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d <- labelPosition(d, "b", labelRatio, percentage)
  fillCol <- fillColors(d, "a", colors, colorScale, highlightValue,
                        highlightValueColor, labelWrap)
  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }
  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], colour = a, group = 1)) +
    geom_line() +
    geom_point(shape = as.integer(shapeType)) +
    geom_vline(xintercept = lineXY[2], color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)), "black", "transparent"), linetype = "dashed") +
    geom_hline(yintercept = lineXY[1], color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)), "black", "transparent"), linetype = "dashed") +
    geom_text(aes(y = labPos, label = paste0(format[1], format(d[[ifelse(percentage, "percent", "b")]], big.mark = marks[1], decimal.mark = marks[2],
                                                               digits = nDigits, nsmall = nDigits), format[2])),
              check_overlap = TRUE, color = ifelse(showText, colorText,
                                                   "transparent")) + labs(title = title, subtitle = subtitle,
                                                                          caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_color_manual(values = fillCol) + scale_y_continuous(labels = function(x) paste0(format[1],
                                                                                          format(x, big.mark = marks[1], decimal.mark = marks[2],
                                                                                                 digits = nDigits, nsmall = nDigits), format[2]),
                                                              limits = c(ifelse(startAtZero, 0, NA), NA)) + theme_ds() +
    theme(legend.position = "none", plot.caption = element_text(hjust = 1))
  if (orientation == "hor")
    gg <- gg + coord_flip()
  gg
}

labelPosition
function (data, col, labelRatio, percentage = FALSE, zeroToNa = FALSE)
{
  col <- ifelse(percentage, "percent", col)
  half <- data[[col]] - data[[col]]/2
  small <- half < max(data[[col]] * labelRatio)
  half[small] <- data[[col]][small] + max(data[[col]])/50
  data$labPos <- half
  if (zeroToNa) {
    data$labPos[data[[col]] == 0] <- NA
  }
  data
}



gg_bar_CatNum

f0 <- function (data, title = NULL, subtitle = NULL, caption = NULL,
                horLabel = NULL, verLabel = NULL, horLine = NULL, verLine = NULL,
                agg = "sum", agg_text = NULL, colors = NULL, colorText = "#5A6B72",
                colorScale = "no", dropNa = FALSE, prefix = NULL, suffix = NULL,
                highlightValue = NULL, highlightValueColor = NULL, labelRatio = 0.1,
                labelWrap = 12, marks = c(".", ","), nDigits = NULL, order = NULL,
                orientation = "ver", percentage = FALSE, sort = "no", sliceN = NULL,
                showText = TRUE, sizeText = 3, theme = NULL, ...)
{
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  title <- title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  Lc <- length(unique(d$a))
  angleText <- ifelse(Lc >= 7 & Lc < 15, 45, ifelse(Lc >= 15,
                                                    90, 0))
  prefix_agg <- ifelse(is.null(agg_text), agg, agg_text)
  labelsXY <- orientationXY(orientation, x = nms[1], y = ifelse(nrow(d) ==
                                                                  dplyr::n_distinct(d$a), nms[2], paste(prefix_agg, nms[2])),
                            hor = horLabel, ver = verLabel)
  lineXY <- orientationXY(orientation, 0, 0, hor = horLine,
                          ver = verLine)
  if (dropNa)
    d <- d %>% tidyr::drop_na()
  if (is.null(nDigits)) {
    nDig <- 0
  }
  else {
    nDig <- nDigits
  }
  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA), b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = round(agg(agg, b), nDig)) %>%
    dplyr::mutate(percent = round(b * 100/sum(b, na.rm = TRUE), nDig))

  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d1 <- labelPosition(d, "b", labelRatio, percentage)
  fillCol <- fillColors(d, "a", colors, colorScale, highlightValue,
                        highlightValueColor, labelWrap)
  if (percentage & is.null(suffix)) {
    suffix <- "%"
  }
  varP <- ifelse(percentage, "percent", "b")
  minLim <- ifelse(min(d[[varP]], na.rm = T) < 0, min(d[[varP]],
                                                      na.rm = T), 0)
  maxLim <- max(d[[varP]], na.rm = T) + 0.3 * max(d[[varP]],
                                                  na.rm = T)

  sq <- nchar(round(maxLim - minLim, 0)) - 2
  sq <- floor(seq(minLim, maxLim, round(maxLim/Lc, 0)) * 10^(-df)) * 10^df
  sq <- c(0, minLim, sq)

  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], fill = a)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = lineXY[2], color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)), "#5A6B72",
                                                      "transparent"), linetype = "dashed") +
    geom_hline(yintercept = lineXY[1], color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)), "#5A6B72", "transparent"), linetype = "dashed") +
    geom_text(aes(y = labPos, label = paste0(prefix, format(d[[ifelse(percentage, "percent", "b")]], big.mark = marks[1], decimal.mark = marks[2], nsmall = nDig), suffix)), check_overlap = TRUE, size = sizeText, color = ifelse(showText, colorText, "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = labelsXY[1], y = labelsXY[2]) + scale_fill_manual(values = fillCol) +
    scale_y_continuous(labels = function(x) paste0(prefix,
                                                   format(x, big.mark = marks[1], decimal.mark = marks[2],
                                                          nsmall = nDig), suffix),
                       breaks = seq(minLim,maxLim, round(maxLim/Lc, nDig)),
                       limits = c(minLim, maxLim))
  if (orientation == "hor")
    gg <- gg + coord_flip()
  if (is.null(theme)) {
    gg <- gg + tma()
  }
  else {
    gg <- gg + theme
  }
  gg + theme(legend.position = "none", plot.caption = element_text(hjust = 1),
             axis.text.x = element_text(angle = angleText))
}



lp0 <- function(data, col, labelRatio, percentage = FALSE, zeroToNa = FALSE) {
  col <- ifelse(percentage, "percent", col)
  half <- data[[col]] - data[[col]] / 2
  small <- half < max(data[[col]] * labelRatio)
  half[small] <- data[[col]][small] + max(data[[col]]) / 50
  data$labPos <- half
  # do I want zero labels to be shown?
  if (zeroToNa) {
    data$labPos[data[[col]] == 0] <- NA
  }
  data
}




sq <- nchar(round(maxLim - minLim, 0)) - 2
sq <- floor(seq(minLim, maxLim, round(maxLim/Lc, 0)) * 10^(-df)) * 10^df
sq <- unique(c(0, minLim, sq))

d2 <- lp0(d0, "b", 1, FALSE)

ggplot(d, aes(x = a, y = d[[ifelse(FALSE, "percent", "b")]], fill = a)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0("", format(x, big.mark = ".", decimal.mark = ",", nsmall = 0), ""),
                     breaks = sq, limits = c(minLim, maxLim))





f0 <- function(data,
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
  fillCol <- fillColors(d, "a", colors, colorScale, highlightValue, highlightValueColor, labelWrap)
  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  # if (spline) {
  #   d <- as.data.frame(spline(d))
  # }

  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], colour = a, group = 1)) +
    geom_line() +
    geom_point(shape = as.integer(shapeType)) +
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
    scale_color_manual(values = fillCol) +
    scale_y_continuous(labels =  function(x) paste0(format[1],
                                                    format(x,
                                                           big.mark = marks[1],
                                                           decimal.mark = marks[2],
                                                           digits = nDigits,
                                                           nsmall = nDigits),
                                                    format[2]),
                       limits = c(ifelse(startAtZero, 0, NA), NA)) +
    tm() +
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


tm <- function(...){
  type <- 'outer'
  inner <- type == 'inner'
  palette <- list(
    background = "#ffffff", # #ffffff #F0EDFF #F8EDFA #FDF8FD
    text = list(inner = "#555555", outer = "#111111"),
    line = list(inner = "#826A50", outer = "#362C21"),
    gridline = "#c9c7d3",
    swatch = c("#111111","#65ADC2","#233B43","#E84646","#C29365","#362C21","#316675","#168E7F","#109B37"),
    gradient = list(low = "#65ADC2", high = "#362C21")
  )
  spacing <- 0.5
  line_colour <- "#1d1d1d"
  text_colour <- "#555555"
  text_size <- 12
  line_weight <- 0.5
  x_title_spacing <- function(spacing)
    max(-1.2, -(spacing / 1.25) + 0.5)
  y_title_spacing <- function(spacing)
    max(0.8, min(2.4, spacing))

  theme(
    #legend.title=element_blank(),
    line = element_line(
      colour = line_colour,
      size = line_weight,
      linetype = 1,
      lineend = "butt"),
    rect = element_rect(
      fill = "white",
      colour = text_colour,
      size = 0.5,
      linetype = 1),
    text = element_text(
      debug=FALSE,
      margin=margin(),
      family = '',
      face = "plain",
      colour = text_colour,
      size = text_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 0.9),
    axis.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8),
      colour = text_colour),
    strip.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8)),
    axis.line = element_line(
      colour = line_colour),
    axis.line.x = element_line(colour = line_colour),
    axis.line.y = element_line(colour = line_colour),
    axis.text.x = element_text(
      debug=FALSE,
      margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
      vjust = 1,
      colour = text_colour,
      face='bold'),
    axis.text.y = element_text(
      debug=FALSE,
      margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
      hjust = 1,
      colour = text_colour,
      face='bold'),
    axis.ticks = element_line(colour = line_colour),
    axis.title = element_text(face='bold',colour = text_colour),
    axis.title.x = element_text(
      debug=FALSE,
      margin=margin(),
      vjust=x_title_spacing(spacing)),
    axis.title.y = element_text(
      debug=FALSE,
      margin=margin(),
      angle = 90,
      vjust=y_title_spacing(spacing)),
    axis.ticks.length = grid::unit(0.15, "cm"),
    axis.ticks.length.x.bottom = grid::unit(0.15, "cm"),
    axis.ticks.length.x.top = grid::unit(0.15, "cm"),
    axis.ticks.length.y.left = grid::unit(0.15, "cm"),
    axis.ticks.length.y.right = grid::unit(0.15, "cm"),
    legend.background = element_rect(
      colour = ifelse(inner, 'white', palette$background),
      fill = ifelse(inner, 'white', palette$background)),
    legend.margin = grid::unit(0.2 * spacing, "cm"),
    legend.key = element_rect(
      colour = ifelse(inner, 'white', palette$background),
      fill = palette$background),
    legend.key.size = grid::unit(
      1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8)),
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    panel.background = element_rect(fill = palette$background,colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(linetype='dashed',colour = palette$gridline),
    panel.grid.minor = element_blank(),
    panel.margin = grid::unit(0.5 * spacing, 'cm'),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(
      fill = ifelse(inner, 'white', palette$background),
      colour = NA),
    strip.text.x = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(1.1),
      face = 'bold'),
    strip.text.y = element_text(
      debug=FALSE,
      margin=margin(),
      angle = -90,
      face = 'bold',
      size = rel(1.1)),
    strip.switch.pad.grid = grid::unit(0, 'cm'),
    strip.switch.pad.wrap = grid::unit(0, 'cm'),
    plot.background = element_rect(
      colour = ifelse(inner, 'white', palette$background),
      fill = ifelse(inner, 'white', palette$background)),

    plot.title = element_text(
      debug=FALSE,
      margin=margin(0, 0, 6.6, 0),
      size = rel(1.2),
      vjust = spacing,
      face='bold'),
    plot.margin = grid::unit(c(0.625, 0.625, 0.625, 0.625) * spacing, 'cm'),
    complete = TRUE
  )
}
#
#
# tm0 <- function(...){
#   type <- 'outer'
#   inner <- type == 'inner'
#   palette <- list(
#     background = "#ffffff", # #ffffff #F0EDFF #F8EDFA #FDF8FD
#     text = list(inner = "#555555", outer = "#111111"),
#     line = list(inner = "#826A50", outer = "#362C21"),
#     gridline = "#c9c7d3",
#     swatch = c("#111111","#65ADC2","#233B43","#E84646","#C29365","#362C21","#316675","#168E7F","#109B37"),
#     gradient = list(low = "#65ADC2", high = "#362C21")
#   )
#   spacing <- 0.5
#   line_colour <- "#1d1d1d"
#   text_colour <- "#555555"
#   text_size <- 12
#   line_weight <- 0.5
#   x_title_spacing <- function(spacing)
#     max(-1.2, -(spacing / 1.25) + 0.5)
#   y_title_spacing <- function(spacing)
#     max(0.8, min(2.4, spacing))
#
#   theme(
#     #legend.title=element_blank(),
#     line = element_line(
#       colour = line_colour,
#       size = line_weight,
#       linetype = 1,
#       lineend = "butt"),
#     rect = element_rect(
#       fill = "white",
#       colour = text_colour,
#       size = 0.5,
#       linetype = 1),
#     text = element_text(
#       debug=FALSE,
#       margin=margin(),
#       family = '',
#       face = "plain",
#       colour = text_colour,
#       size = text_size,
#       hjust = 0.5,
#       vjust = 0.5,
#       angle = 0,
#       lineheight = 0.9),
#     axis.text = element_text(
#       debug=FALSE,
#       margin=margin(),
#       size = rel(0.8),
#       colour = text_colour),
#     strip.text = element_text(
#       debug=FALSE,
#       margin=margin(),
#       size = rel(0.8)),
#     axis.line = element_line(
#       colour = line_colour),
#     axis.line.x = element_line(colour = line_colour),
#     axis.line.y = element_line(colour = line_colour),
#     axis.text.x = element_text(
#       debug=FALSE,
#       margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
#       vjust = 1,
#       colour = text_colour,
#       face='bold'),
#     axis.text.y = element_text(
#       debug=FALSE,
#       margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
#       hjust = 1,
#       colour = text_colour,
#       face='bold'),
#     axis.ticks = element_line(colour = line_colour),
#     axis.title = element_text(face='bold',colour = text_colour),
#     axis.title.x = element_text(
#       debug=FALSE,
#       margin=margin(),
#       vjust=x_title_spacing(spacing)),
#     axis.title.y = element_text(
#       debug=FALSE,
#       margin=margin(),
#       angle = 90,
#       vjust=y_title_spacing(spacing)),
#     axis.ticks.length = grid::unit(0.15, "cm"),
#     axis.ticks.length.x.bottom = grid::unit(0.15, "cm"),
#     axis.ticks.length.x.top = grid::unit(0.15, "cm"),
#     axis.ticks.length.y.left = grid::unit(0.15, "cm"),
#     axis.ticks.length.y.right = grid::unit(0.15, "cm"),
#     legend.background = element_rect(
#       colour = ifelse(inner, 'white', palette$background),
#       fill = ifelse(inner, 'white', palette$background)),
#     legend.margin = grid::unit(0.2 * spacing, "cm"),
#     legend.key = element_rect(
#       colour = ifelse(inner, 'white', palette$background),
#       fill = palette$background),
#     legend.key.size = grid::unit(
#       1.2, "lines"),
#     legend.key.height = NULL,
#     legend.key.width = NULL,
#     legend.text = element_text(
#       debug=FALSE,
#       margin=margin(),
#       size = rel(0.8)),
#     legend.position = "right",
#     legend.direction = NULL,
#     legend.justification = "center",
#     legend.box = NULL,
#     panel.background = element_rect(fill = palette$background,colour = NA),
#     panel.border = element_blank(),
#     panel.grid.major = element_line(linetype='dashed',colour = palette$gridline),
#     panel.grid.minor = element_blank(),
#     panel.margin = grid::unit(0.5 * spacing, 'cm'),
#     panel.margin.x = NULL,
#     panel.margin.y = NULL,
#     panel.ontop = FALSE,
#     strip.background = element_rect(
#       fill = ifelse(inner, 'white', palette$background),
#       colour = NA),
#     strip.text.x = element_text(
#       debug=FALSE,
#       margin=margin(),
#       size = rel(1.1),
#       face = 'bold'),
#     strip.text.y = element_text(
#       debug=FALSE,
#       margin=margin(),
#       angle = -90,
#       face = 'bold',
#       size = rel(1.1)),
#     strip.switch.pad.grid = grid::unit(0, 'cm'),
#     strip.switch.pad.wrap = grid::unit(0, 'cm'),
#     plot.background = element_rect(
#       colour = ifelse(inner, 'white', palette$background),
#       fill = ifelse(inner, 'white', palette$background)),
#
#     plot.title = element_text(
#       debug=FALSE,
#       margin=margin(0, 0, 6.6, 0),
#       size = rel(1.2),
#       vjust = spacing,
#       face='bold'),
#     plot.margin = grid::unit(c(0.625, 0.625, 0.625, 0.625) * spacing, 'cm'),
#     complete = TRUE
#   )
# }

### Area plots

df <- sampleData('Yea-Num')
gg_area_CatNum(df)
gg_area_CatNum(df, showText = F)
gg_area_CatNum(df, colors = "darkred")
gg_area_CatNum(df,
               title = "Titulo",
               subtitle = "subtitulo" ,
               caption = "Caption")

gg_area_CatNum(df, percentage = TRUE)

gg_area_CatNumP(sampleData("Cat-NumP"))

dfCdn <- sampleData("Cat-Cat-Num")
gg_area_CatCatNum(dfCdn)
gg_area_CatCatNum(dfCdn, graphType = "stacked")
gg_area_CatCatNum(dfCdn, percentage = TRUE, graphType = "stacked")
gg_area_CatCatNum(dfCdn, showText = F)

dfCc <- sampleData('Cat-Cat')
gg_area_CatCat(dfCc)
gg_area_CatCat(dfCc, graphType = "stacked")
gg_area_CatCat(dfCc, percentage = TRUE, graphType = "stacked")
gg_area_CatCat(dfCc, showText = F)


## Bar plots

df <- sampleData('Cat-Num')
gg_bar_CatNum(df)
gg_bar_CatNum(df, colorScale = "discrete")
gg_bar_CatNum(df, orientation = "hor")
gg_bar_CatNum(df, highlightValue = "FormC", highlightValueColor = "red")
gg_bar_CatNum(df, agg = "mean")
gg_bar_CatNum(df, horLabel = "nombre del eje horizontal",
              verLabel = "nombre del eje vertical")
gg_bar_CatNum(df, format = c("$", ""))
gg_bar_CatNum(df, marks = c(",", "."))
gg_bar_CatNum(df, horLine = 4000)



dfCyn <- sampleData('Cat-Yea-Num')
gg_bar_CatCatNum(dfCyn, colorScale = "continuous")
gg_bar_CatCatNum(dfCyn, graphType = "stacked")
gg_bar_CatCatNum(dfCyn, graphType = "stacked", percentage = T)
gg_bar_CatCatNum(dfCyn, format = c("$", ""))


dCN <- sampleData("Cat-Num")
gg_treemap_CatNum(dCN)
gg_treemap_CatNum(dCN,
                  title = "titulo",
                  subtitle =  "subtitulo",
                  caption = "caption",
                  colors = c("#AFCC1D", "#FFDC1A", "#FCDAA4"))
gg_treemap_CatNum(dCN,
                  colorScale = "discrete",
                  highlightValue = "IlkD", highlightValueColor = "red")

gg_treemap_CatNum(dCN,
                  showLegend = F,
                  colorScale = "continuous",
                  agg = "mean",
                  marks = c(".", ","))

gg_treemap_CatNum(dCN,
                  percentage = T,
                  nDigits = 5,
                  sliceN = 3,
                  dropNa = T,
                  legendPosition = c("bottom", "bottom"),
                  showText = F)

dC <- sampleData("Cat")
gg_treemap_Cat(dC)

data <- sampleData('Cat-Cat-Num')
gg_treemap_CatCatNum(data, colorGroup = '#cccccc')
gg_treemap_CatCatNum(data, colorScale = 'continuous')
gg_treemap_CatCatNum(data, colorScale = 'continuous', showLegend = F)
gg_treemap_CatCatNum(data,
                     colorScale = 'continuous',
                     showLegend = F,
                     showText = F,
                     colorGroup = '#cccccc',
                     colorText = c('#ffffff'))

data <- sampleData('Cat-Cat')
gg_treemap_CatCat(data)

data <- sampleData('Cat-NumP')
gg_treemap_CatNumP(data)
