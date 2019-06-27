#' Scatter Num Num
#'
#' @param data
#'
#' @export
gg_scatter_NumNum <- function(data = NULL,
                              opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  labelsXY <- ggmagic::orientationXY('ver',
                                     x = nms[1],
                                     y = nms[2],
                                     hor = opts$horLabel,
                                     ver = opts$verLabel)
  lineXY <- ggmagic::orientationXY('ver',
                                   0,
                                   0,
                                   hor = opts$horLine,
                                   ver = opts$verLine)


  if (is.null(opts$colors)) opts$colors <- '#3DB26F'

  d <- d %>% drop_na()

  if (is.null(opts$nDigitsY)) {
    nDigY <- 0
  } else {
    nDigY <- opts$nDigitsY
  }

  if (is.null(opts$nDigitsX)) {
    nDigX <- 0
  } else {
    nDigX <- opts$nDigitsX
  }


  d$a <- round(d$a, nDigX)
  d$b <- round(d$b, nDigY)

  Lc <- length(unique(d$a))
  minLim <- min(d[['b']], na.rm = T)
  maxLim <- max(d[['b']], na.rm = T) + 0.3 * max(d[['b']], na.rm = T)

  gg <- ggplot(d, aes(x=a, y=b)) +
    geom_point(colour = opts$colors) +
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
    scale_x_continuous(labels =  function(x) paste0(opts$prefixX,
                                                    format(x,
                                                           big.mark = opts$marks[1],
                                                           decimal.mark = opts$marks[2],
                                                           digits = nDigX,
                                                           nsmall = nDigX),
                                                    opts$suffixY)) +
    scale_y_continuous(labels =  function(x) paste0(opts$prefixY,
                                                    format(x,
                                                           big.mark = opts$marks[1],
                                                           decimal.mark = opts$marks[2],
                                                           digits = nDigY,
                                                           nsmall = nDigY),
                                                    opts$suffixY),
                       limits = c(minLim, maxLim))


  if (opts$regression) {
   gg <- gg + geom_smooth(method=lm,
                          se=FALSE,
                          fullrange=TRUE,
                          colour = opts$regression_color)
  }

  if (is.null(opts$theme)) {
    gg <- gg + ggmagic::tma(orientation = "ver")
  } else {
    gg <- gg + opts$theme
  }

  gg

}


#' Scatter Num Num Num
#'
#' @param data
#'
#' @export
gg_scatter_NumNumNum <- function(data = NULL,
                                 opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  labelsXY <- ggmagic::orientationXY('ver',
                                     x = nms[1],
                                     y = nms[2],
                                     hor = opts$horLabel,
                                     ver = opts$verLabel)
  lineXY <- ggmagic::orientationXY('ver',
                                   0,
                                   0,
                                   hor = opts$horLine,
                                   ver = opts$verLine)


  if (is.null(opts$colors)) opts$colors <- '#3DB26F'

  d <- d %>% drop_na()

  if (is.null(opts$nDigitsY)) {
    nDigY <- 0
  } else {
    nDigY <- opts$nDigitsY
  }

  if (is.null(opts$nDigitsX)) {
    nDigX <- 0
  } else {
    nDigX <- opts$nDigitsX
  }

  if (is.null(opts$nDigitsSize)) {
    nDigS <- 0
  } else {
    nDigS <- opts$nDigitsSize
  }

  d$a <- round(d$a, nDigX)
  d$b <- round(d$b, nDigY)
  d$c <- round(d$c, nDigS)

  Lc <- length(unique(d$a))
  minLim <- min(d[['b']], na.rm = T)
  maxLim <- max(d[['b']], na.rm = T) + 0.3 * max(d[['b']], na.rm = T)

  gg <- ggplot(d, aes(x=a, y=b, size =c)) +
    geom_point(colour = opts$colors) +
    theme(legend.position = "none") +
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
    scale_x_continuous(labels =  function(x) paste0(opts$prefixX,
                                                    format(x,
                                                           big.mark = opts$marks[1],
                                                           decimal.mark = opts$marks[2],
                                                           digits = nDigX,
                                                           nsmall = nDigX),
                                                    opts$suffixY)) +
    scale_y_continuous(labels =  function(x) paste0(opts$prefixY,
                                                    format(x,
                                                           big.mark = opts$marks[1],
                                                           decimal.mark = opts$marks[2],
                                                           digits = nDigY,
                                                           nsmall = nDigY),
                                                    opts$suffixY),
                       limits = c(minLim, maxLim))


  if (opts$regression) {
    gg <- gg + geom_smooth(method=lm,
                           se=FALSE,
                           fullrange=TRUE,
                           colour = opts$regression_color)
  }

  if (is.null(opts$theme)) {
    gg <- gg + ggmagic::tma(orientation = "ver")
  } else {
    gg <- gg + opts$theme
  }

  gg

}



#' Scatter Cat Num Num
#'
#' @param data
#'
#' @export
gg_scatter_CatNumNum <- function(data = NULL,
                                 opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  labelsXY <- ggmagic::orientationXY('ver',
                                     x = nms[1],
                                     y = nms[2],
                                     hor = opts$horLabel,
                                     ver = opts$verLabel)
  lineXY <- ggmagic::orientationXY('ver',
                                   0,
                                   0,
                                   hor = opts$horLine,
                                   ver = opts$verLine)



  d <- d %>% drop_na()
  if (opts$color_scale == 'discrete') {
    colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    colorDefault <- ggmagic::discreteColorSelect(colorDefault, d)
  } else if (opts$color_scale == "no"){
    colorDefault <- rep("#3DB26F", length(unique(d$a)))
  } else {
    colorDefault <- leaflet::colorNumeric(c("#53255E", "#ff4097"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }

  if (is.null(opts$colors)) {
    opts$colors <- colorDefault
  } else {
    opts$colors <- ggmagic::fillColors(d, "a", opts$colors, opts$color_scale, opts$highlight_value, opts$highlight_valueColor, opts$label_wrap)
 }

  if (is.null(opts$nDigitsY)) {
    nDigY <- 0
  } else {
    nDigY <- opts$nDigitsY
  }

  if (is.null(opts$nDigitsX)) {
    nDigX <- 0
  } else {
    nDigX <- opts$nDigitsX
  }


  d$b <- round(d$b, nDigX)
  d$c <- round(d$c, nDigY)

  Lc <- length(unique(d$a))
  minLim <- min(d[['c']], na.rm = T)
  maxLim <- max(d[['c']], na.rm = T) + 0.3 * max(d[['c']], na.rm = T)

  gg <- ggplot(d, aes(x=b, y=c, colour = a)) +
    geom_point() +
    scale_colour_manual(values = opts$colors) +
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
    scale_x_continuous(labels =  function(x) paste0(opts$prefixX,
                                                    format(x,
                                                           big.mark = opts$marks[1],
                                                           decimal.mark = opts$marks[2],
                                                           digits = nDigX,
                                                           nsmall = nDigX),
                                                    opts$suffixY)) +
    scale_y_continuous(labels =  function(x) paste0(opts$prefixY,
                                                    format(x,
                                                           big.mark = opts$marks[1],
                                                           decimal.mark = opts$marks[2],
                                                           digits = nDigY,
                                                           nsmall = nDigY),
                                                    opts$suffixY),
                       limits = c(minLim, maxLim))


  if (opts$regression) {
    gg <- gg + geom_smooth(method=lm,
                           se=FALSE,
                           fullrange=TRUE)
  }

  if (is.null(opts$theme)) {
    gg <- gg + ggmagic::tma(orientation = "ver")
  } else {
    gg <- gg + opts$theme
  }

  gg +
   theme_leg() +
    guides(fill = guide_legend(nrow = 1))

}

