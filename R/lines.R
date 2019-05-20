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
gg_line_CatNum <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  opts$title <-  opts$title %||% ""
  opts$subtitle <- opts$subtitle %||% ""
  opts$caption <- opts$caption %||% ""

  Lc <- length(unique(d$a))
  angleText <- ifelse( Lc >= 10 & Lc < 15,
                       45,
                       ifelse(Lc >= 15, 90, 0))

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)

  labelsXY <- ggmagic::orientationXY(opts$orientation,
                                     x = nms[1],
                                     y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(prefix_agg, nms[2])),
                                     hor = opts$horLabel,
                                     ver = opts$verLabel)
  lineXY <- ggmagic::orientationXY(opts$orientation,
                                   0,
                                   0,
                                   hor = opts$horLine,
                                   ver = opts$verLine)

  if (opts$dropNa)
    d <- d %>%
    tidyr::drop_na()

  opts$nDigits <- ifelse(!is.null(opts$nDigits), opts$nDigits, 0)

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = ggmagic::agg(opts$agg, b)) %>%
    dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), opts$nDigits))

  # d$a <- as.character(d$a)
  # d$a[is.na(d$a)] <- "NA"

  d <- ggmagic::sortSlice(d, "b", "a", opts$orientation, opts$sort, opts$sliceN)
  d <- ggmagic::orderCategory(d, "a", opts$orientation, opts$order, opts$label_wrap)
  d <- ggmagic::labelPosition(d, "b", opts$label_ratio, opts$percentage)
  fillCol <- ggmagic::fillColors(d, "a", opts$colors, opts$color_scale, opts$highlight_value, opts$highlight_valueColor, opts$label_wrap)

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  # if (spline) {
  #   d <- as.data.frame(spline(d))
  # }

  varP <- ifelse(opts$percentage, "percent", "b")
  minLim <- ifelse(min(d[[varP]], na.rm = T) < 0, min(d[[varP]], na.rm = T), 0)
  maxLim <- max(d[[varP]], na.rm = T) + 0.3 * max(d[[varP]], na.rm = T)

  gg <- ggplot(d, aes(x = a, y = d[[varP]], colour = a, group = 1)) +
    geom_line() +
    geom_point(shape = as.integer(opts$shape_type)) +
    geom_vline(xintercept = lineXY[2],
               color = ifelse((opts$orientation == "hor" & !is.null(opts$horLine)) | (opts$orientation == "ver" & !is.null(opts$verLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_hline(yintercept = lineXY[1],
               color = ifelse((opts$orientation == "hor" & !is.null(opts$verLine)) | (opts$orientation == "ver" & !is.null(opts$horLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_text(aes(y = labPos,
                  label = paste0(opts$prefix,
                                 format(d[[varP]],
                                        big.mark = opts$marks[1],
                                        decimal.mark = opts$marks[2],
                                        digits = opts$nDigits,
                                        nsmall = opts$nDigits),
                                 opts$suffix)),
              check_overlap = TRUE,
              size = opts$text_size,
              color = ifelse(opts$text_show, opts$text_color, "transparent")) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_color_manual(values = fillCol) +
    scale_y_continuous(labels =  function(x) paste0(opts$prefix,
                                                    format(x,
                                                           big.mark = opts$marks[1],
                                                           decimal.mark = opts$marks[2],
                                                           digits = opts$nDigits,
                                                           nsmall = opts$nDigits),
                                                    opts$suffix),
                       breaks = seq(ifelse(opts$startAtZero, 0, minLim), maxLim, round(maxLim/Lc, 2)),
                       limits = c(ifelse(opts$startAtZero, 0, minLim), maxLim))#c(ifelse(startAtZero, 0, NA), NA))

  if (opts$orientation == "hor") {
    gg <- gg +
      coord_flip()
  }

  if (is.null(opts$theme)) {
    gg <- gg + ggmagic::tma(orientation = opts$orientation)
  } else {
    gg <- gg + opts$theme
  }

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
gg_line_Cat <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label))

  gg <- gg_line_CatNum(data = d, opts = opts)
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
gg_line_CatCatNum <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  Lc <- length(unique(d$a))
  angleText <- ifelse( Lc >= 10 & Lc < 15,
                       45,
                       ifelse(Lc >= 15, 90, 0))

  opts$title <-  opts$title %||% ""
  opts$subtitle <- opts$subtitle %||% ""
  opts$caption <- opts$caption %||% ""
  opts$legend_title <- opts$legend_title %||% nms[1]

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, as.character(opts$agg_text))

  labelsXY <- ggmagic::orientationXY(opts$orientation,
                                     x = nms[2],
                                     y = ifelse(nrow(d) == dplyr::n_distinct(d$a) & nrow(d) == dplyr::n_distinct(d$b),
                                                nms[3],
                                                paste(prefix_agg, nms[3])),
                                     hor = opts$horLabel,
                                     ver = opts$verLabel)
  lineXY <- ggmagic::orientationXY(opts$orientation,
                                   0,
                                   0,
                                   hor = opts$horLine,
                                   ver = opts$verLine)

  if (any(opts$dropNaV))
    d <- d %>%
    tidyr::drop_na(which(opts$dropNaV))

  opts$nDigits <- ifelse(!is.null(opts$nDigits), opts$nDigits, 0)

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = ggmagic::agg(opts$agg, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::mutate(percent = round(c * 100 / sum(c, na.rm = TRUE), opts$nDigits))

  #REVISAR PARA QUÉ ESTO...
  # d$c[is.na(d$c)] <- NA
  # d$a <- as.character(d$a)
  # d$a[is.na(d$a)] <- NA
  # d$b <- as.character(d$b)
  # d$b[is.na(d$b)] <- NA


  d <- ggmagic::orderCategory(d, "a", opts$orientation, opts$order1, opts$label_wrapV[1])
  d <- ggmagic::orderCategory(d, "b", opts$orientation, opts$order2, opts$label_wrapV[2])
  d <- ggmagic::labelPosition(d, "c", opts$label_ratio, opts$percentage, zeroToNa = TRUE)
  fillCol <- ggmagic::fillColors(d, "a", opts$colors, opts$color_scale, NULL, NULL, opts$label_wrapV[1])

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  varP <- ifelse(opts$percentage, "percent", "c")
  minLim <- min(d[[varP]], na.rm = T)
  maxLim <- max(d[[varP]], na.rm = T) + 0.3 * max(d[[varP]], na.rm = T)

  gg <- ggplot(d, aes(x = b, y = d[[varP]], colour = a, group = a)) +
    geom_line() +
    geom_point(shape = as.integer(opts$shape_type)) +
    geom_vline(xintercept = lineXY[2],
               color = ifelse((opts$orientation == "hor" & !is.null(opts$horLine)) | (opts$orientation == "ver" & !is.null(opts$verLine)),
                              "#5A6B72",
                              "transparent"),
               linetype = "dashed") +
    geom_hline(yintercept = lineXY[1],
               color = ifelse((opts$orientation == "hor" & !is.null(opts$verLine)) | (opts$orientation == "ver" & !is.null(opts$horLine)),
                              "#5A6B72",
                              "transparent"),
               linetype = "dashed") +
    geom_text(aes(y = labPos,
                  label = paste0(opts$prefix,
                                 format(d[[varP]],
                                        big.mark = opts$marks[1],
                                        decimal.mark = opts$marks[2],
                                        digits = opts$nDigits,
                                        nsmall = opts$nDigits),
                                 opts$suffix)),
              check_overlap = TRUE,
              size = opts$text_size,
              color = ifelse(opts$text_show, opts$text_color, "transparent"),
              position = position_dodge(width = 1)) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_colour_manual(values = fillCol, name = opts$legend_title) +
    scale_y_continuous(labels = function(x) paste0(opts$prefix,
                                                   format(x,
                                                          big.mark = opts$marks[1],
                                                          decimal.mark = opts$marks[2],
                                                          digits = opts$nDigits,
                                                          nsmall = opts$nDigits),
                                                   opts$suffix),
                       breaks = seq(ifelse(opts$startAtZero, 0, minLim), maxLim, round(maxLim/Lc, 2)),
                       limits = c(ifelse(opts$startAtZero, 0, minLim), maxLim))

  if (opts$orientation == "hor")
    gg <- gg +
    coord_flip()

  if (is.null(opts$theme)) {
    gg <- gg + ggmagic::tma(orientation = opts$orientation)
  } else {
    gg <- gg + opts$theme
  }

  gg +
    theme(axis.text.x = element_text(angle = angleText),
          plot.caption = element_text(hjust = 1),
          legend.position= opts$legend_position) +
    theme_leg() +
    guides(fill = guide_legend(nrow = 1))
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
gg_line_CatCat <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label[1]))

  gg <- gg_line_CatCatNum(data = d, opts = opts)
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
gg_line_CatNumP <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  names(d) <- f$dic_$d$label

  data <- d %>%
    gather("categories", "count", names(d)[-1])

  gg <- gg_line_CatCatNum(data = d, opts = opts)
  gg
}



