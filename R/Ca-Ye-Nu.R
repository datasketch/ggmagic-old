#' gg_bar_grp_ver_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_grp_ver_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grp_ver_CaYeNu. <- function(data, title = NULL, xlab = NULL, ylab = NULL,
                                   leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d
  xlab <- xlab %||% nms[2]
  ylab <- ylab %||% nms[3]
  graph <- ggplot(data, aes(x = b, y = c, group = a, fill = a)) +
    geom_bar( position = "dodge", stat="identity") +
    scale_y_continuous(labels = comma) +
    ggtitle(title) + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position = leg_pos) +
    labs(title = title, x = xlab, y = ylab)

  return(graph)
}

#' gg_bar_grp_hor_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_grp_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grp_hor_CaYeNu. <- function(data, title = NULL, xlab = NULL, ylab = NULL,
                                   leg_pos = "right", ...){

  graph <- gg_bar_grp_ver_CaYeNu.(data, title, xlab, ylab, leg_pos) + coord_flip()

  return(graph)
}



#' gg_bar_stk_hor_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_stk_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stk_hor_CaYeNu. <- function(data, title = NULL, xlab = NULL, ylab = NULL,
                                   leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[2]
  ylab <- ylab %||% nms[3]
  data <- f$d
  graph <- ggplot(data, aes(x=a,y=c,group=factor(b),fill=factor(b))) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = comma) +
    coord_flip() + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position = leg_pos) +
    labs(title = title, x = xlab, y = ylab)

  return(graph)
}

#' gg_bar_stk_ver_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_stk_ver_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stk_ver_CaYeNu. <- function(data, title = NULL, xlab = NULL, ylab = NULL,
                                   leg_pos = "right", ...){

  graph <- gg_bar_stk_hor_CaYeNu.(data, title, xlab, ylab, leg_pos) + coord_flip()

  return(graph)
}

#' gg_lines_hor_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_lines_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lines_hor_CaYeNu. <- function(data, title = "",xlab = NULL, ylab = NULL,
                                 leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[2]
  ylab <- ylab %||% nms[3]
  data <- f$d
  graph <- ggplot(data, aes(x=b,y=c,group=a,colour=a)) +
    geom_line(stat = "identity") +
    scale_y_continuous(labels = comma) +
    scale_color_manual(values = getPalette()) + theme_ds() +
    theme(legend.position = leg_pos) +
    labs(title = title, x = xlab, y = ylab)

  return(graph)
}


#' gg_circle_CaYeNu.: title.
#' circle
#' ciculos
#' @name gg_circle_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_circle_CaYeNu. <- function(data, title = "",xlab = NULL, ylab = NULL,
                              leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[2]
  ylab <- ylab %||% nms[3]
  data <- f$d


  graph <- ggplot(data, aes(x = b, y = a)) +
        geom_point(aes(size = c, colour = "")) +
        theme_ds() + scale_color_manual(values = getPalette()) + guides(size = FALSE) +
    theme(legend.position = leg_pos) +
    labs(title = title, x = xlab, y = ylab)

  return(graph)
}



#' gg_steam_CaYeNu.
#' Steam
#' @name gg_steam_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CaYeNu. <-  function(data, titleLabel = "", xLabel = NULL, yLabel = NULL,
                                   leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[2]
  ylab <- ylab %||% nms[3]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    tidyr::spread(b, c) %>% tidyr::gather(b, c, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$b <- as.numeric(data_graph$b)

  graph <- ggplot(data_graph, aes(x = b, y = c, group = a, fill = a)) +
    stat_steamgraph() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, x = xlab, y = ylab)  +
    theme(legend.position = leg_pos)

  return(graph)
}

#' gg_slope_CaYeNu.
#' Slope
#' @name gg_slope_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_slope_CaYeNu. <-  function(data, titleLabel = "", xLabel = NULL, yLabel = NULL,
                              leg_pos="right", size_text = 6, size_vjust = 1.5,
                              size_hjust = 0.5, size_point = 3, size_line = 1,...){


  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data) +
    geom_text(aes(x = as.factor(b), y = c + 0.5, group = a, color = a, label = c),
              size = size_text, vjust = size_vjust, hjust = size_hjust,
              show.legend = FALSE, check_overlap = TRUE) +
    geom_text(aes(x = as.factor(b), y = min(c) - mean(c), label = b),
              size = size_text, show.legend = FALSE, check_overlap = TRUE) +
    geom_line(aes(x = as.factor(b), y = c, group = a, color = a), size = size_line) +
    geom_point(aes(x = as.factor(b), y = c, group = a, color = a), size = size_point) +
    theme_ds() + theme_ds_clean() +
    labs(title = titleLabel, x = xlab, y = ylab) +
    scale_color_manual(values = getPalette()) + theme(legend.position = leg_pos)


  return(graph)

}
