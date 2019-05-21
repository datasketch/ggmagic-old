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
gg_bar_CatNum <- function(data = NULL, opts = NULL, ...) {

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
    dplyr::summarise(b = round(ggmagic::agg(opts$agg, b), opts$nDigits)) %>%
    dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), opts$nDigits))

  d <- ggmagic::sortSlice(d, "b", "a", opts$orientation, opts$sort, opts$sliceN)
  d <- ggmagic::orderCategory(d, "a", opts$orientation, opts$order, opts$label_wrap)
  d <- ggmagic::labelPosition(d, "b", opts$label_ratio, opts$percentage)
  fillCol <- ggmagic::fillColors(d, "a", opts$colors, opts$color_scale, opts$highlight_value, opts$highlight_valueColor, opts$label_wrap)

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  varP <- ifelse(opts$percentage, "percent", "b")
  minLim <- ifelse(min(d[[varP]], na.rm = T) < 0, min(d[[varP]], na.rm = T), 0)
  maxLim <- max(d[[varP]], na.rm = T) + 0.3 * max(d[[varP]], na.rm = T)
  # sq <- nchar(round(maxLim - minLim, 0)) - 1
  # minLim <- round(minLim *  10^(-sq), 0) * 10^sq
  # maxLim <- round(maxLim *  10^(-sq), 0) * 10^sq
  # sq <- seq(minLim, maxLim, 10^sq)
  # sq <- unique(c(0, minLim, sq))

  gg <- ggplot(d, aes(x = a, y = d[[varP]], fill = a)) +
    geom_bar(stat = "identity") +
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
              color = ifelse(opts$text_show, opts$text_color, "transparent")) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = fillCol) +
    scale_y_continuous(labels =  function(x) paste0(opts$prefix,
                                                    format(x,
                                                           big.mark = opts$marks[1],
                                                           decimal.mark = opts$marks[2],
                                                           digits = opts$nDigits,
                                                           nsmall = opts$nDigits),
                                                    opts$suffix),
                       breaks = seq(ifelse(opts$startAtZero, 0, minLim), maxLim, round(maxLim/Lc, 2)),
                       limits = c(minLim, maxLim))

  if (opts$orientation == "hor")
    gg <- gg +
    coord_flip()

  if (is.null(opts$theme)) {
    gg <- gg + ggmagic::tma(orientation = opts$orientation)
  } else {
    gg <- gg + opts$theme
  }

  gg + theme(legend.position = "none",
             plot.caption = element_text(hjust = 1),
             axis.text.x = element_text(angle = angleText))
}


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
gg_bar_Cat <- function(data = NULL, opts = NULL, ...) {

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

  gg <- gg_bar_CatNum(data = d, opts = opts)
  gg
}



#' Bar (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' @examples
#' gg_bar_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export gg_bar_CatCatNum
gg_bar_CatCatNum <- function(data = NULL, opts = NULL, ...) {

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
    dplyr::group_by(b) %>%
    dplyr::mutate(percent = round(c * 100 / sum(c, na.rm = TRUE), opts$nDigits))

  pd <- position_dodge(width = 0.6)
  if (opts$graph_type == "stacked") {
    pd <- "stack"
    opts$label_ratio <- 0.5
    d <- d %>%
      dplyr::mutate(c = ifelse(c == 0, NA, c),
                    percent = ifelse(percent == 0, NA, percent))
  }

  d <- ggmagic::orderCategory(d, "a", opts$orientation, opts$order1, opts$label_wrapV[1])
  d <- ggmagic::orderCategory(d, "b", opts$orientation, opts$order2, opts$label_wrapV[2])

  if (opts$graph_type == "grouped") {
    d <- ggmagic::labelPosition(d, "c", opts$label_ratio, opts$percentage, zeroToNa = TRUE)
  }

  fillCol <- ggmagic::fillColors(d, "a", opts$colors, opts$color_scale, NULL, NULL, opts$label_wrapV[1])

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  varP <- ifelse(opts$percentage, "percent", "c")
  minLim <- ifelse(min(d[[varP]], na.rm = T) < 0, min(d[[varP]], na.rm = T), 0)
  maxLim <- max(d[[varP]], na.rm = T) + 0.3 * max(d[[varP]], na.rm = T)
  # sq <- nchar(round(maxLim - minLim, 0)) - 2
  # minLim <- round(minLim *  10^(-sq), 0) * 10^sq
  # maxLim <- round(maxLim *  10^(-sq), 0) * 10^sq
  # sq <- seq(minLim, maxLim, 10^sq)
  # sq <- unique(c(0, minLim, sq))

  gg <- ggplot(d, aes(x = b, y = d[[varP]], fill = a)) +
    # geom_bar(width = 0.5, stat = "identity", position = ifelse(graphType == "stacked", "stack", "dodge")) +
    geom_bar(width = 0.5, stat = "identity", position = pd) +
    # geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
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
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_fill_manual(values = fillCol, name = opts$legend_title) +
    scale_y_continuous(labels = function(x) paste0(opts$prefix,
                                                   format(x,
                                                          big.mark = opts$marks[1],
                                                          decimal.mark = opts$marks[2],
                                                          digits = opts$nDigits,
                                                          nsmall = opts$nDigits),
                                                   opts$suffix),
                       # breaks = sq,
                       # limits = c(minLim, maxLim)) +
                       breaks = seq(minLim, maxLim, maxLim/Lc),
                       limits = c(minLim, maxLim))

  if (opts$graph_type == "stacked") {
    gg <- gg +
      geom_text(aes(y = d[[varP]],
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
                position = position_stack(vjust = opts$label_ratio))
  } else {
    gg <- gg +
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
                position = position_dodge(width = 0.65))
  }
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
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


#' Bar (categories, ordered categories)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' gg_bar_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export gg_bar_CatCat
gg_bar_CatCat <- function(data = NULL, opts = NULL, ...) {

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

  gg <- gg_bar_CatCatNum(data = d, opts = opts)
  gg
}


#' Bar (ordered category, n numbers)
#'
#' Compare n quantities among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-NumP
#' @examples
#' gg_bar_CatNumP(sampleData("Cat-NumP", nrow = 10))
#' @export gg_bar_CatNumP
gg_bar_CatNumP <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  names(d) <- f$dic_$d$label

  d <- d %>%
    gather("categories", "count", names(d)[-1])

  gg <- gg_bar_CatCatNum(data = d, opts = opts)
  gg
}
