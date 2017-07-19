
#' Waterfall
#' Waterfall
#' @name gg_waterfall_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                              yLabel =  NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a))

  data_graph <- data %>% mutate(xorder = 1:nrow(.))
  graph <- ggplot_waterfall(data_graph, 'xorder', 'a') +
    scale_color_manual(breaks = c("+","-", ""), values = getPalette()) +
    theme_ds() + theme(legend.position="none") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))



  graph
}


#' Waterfall
#' Waterfall
#' @name gg_waterfall_YeaNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Yea-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_YeaNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel =  NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot_waterfall(data,'a','b') + theme_ds() + theme(legend.position="none") +
    scale_color_manual(breaks = c("+",  "-", ""), values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Waterfall
#' Waterfall
#' @name gg_waterfall_NumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel =  NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot_waterfall(data,'a','b')  +
    scale_color_manual(breaks = c("+", "-", ""), values = getPalette()) +
    theme_ds() + theme(legend.position="none")+
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}
#' Waterfall
#' Waterfall
#' @name gg_waterfall_DatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel =  NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot_waterfall(data,'a','b') +
    scale_color_manual(values = getPalette()) +
    theme_ds() +
    theme(legend.position="none") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Kagi
#' Kagi
#' @name gg_kagi_DatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_kagi_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                            yLabel = NULL, hline = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data, aes(x = a, y = b)) +
    geom_line(aes(color=ifelse(c(diff(b), NA) > 0, "Gain", "Loss"), group = NA)) +
    scale_color_manual(guide="none",values = getPalette()) +
    theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(!is.null(hline)){
    graph <- graph + geom_hline(data = data.frame(valores = hline),
                                aes(yintercept = valores), linetype="dotted")
  }

  return(graph)
}

