#' gg_steam_CaNuNu.
#' Steamgraph
#' @name gg_steam_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CaNuNu. <- function(data, titleLabel = "", xLabel = NULL,
                              yLabel = NULL, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    tidyr::spread(b, c) %>% tidyr::gather(b, c, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$b <- as.numeric(data_graph$b)
  data_graph$c <- as.numeric(data_graph$c)

  graph <- ggplot(data_graph, aes(x = b, y = c, group = a, fill = a)) +
    stat_steamgraph() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position=leg_pos) + theme_ds() +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, x = xlab, y = ylab)

  return(graph)
}

#' gg_lines_CaNuNu.
#' lines
#' @name gg_lines_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lines_CaNuNu. <- function(data, titleLabel = "", xLabel = NULL,
                             yLabel = NULL, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data$a <- as.factor(data$a)
  graph <- ggplot(data) +  geom_line(aes(x = b, y = c, group = a, colour = a))  +
    theme(legend.position = leg_pos) +
    scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, x = xlab, y = ylab)

  return(graph)

}

#' gg_point_lines_CaNuNu.
#' Point Lines
#' @name gg_point_lines_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_lines_CaNuNu. <- function(data, titleLabel = "", xLabel = NULL,
                             yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data$a <- as.factor(data$a)
  graph <- ggplot(data) +  geom_line(aes(x = b, y = c, group = a, colour = a))  +
    geom_point(aes(x = b, y = c, group = a, colour = a)) +
    theme(legend.position = leg_pos) +
    scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, x = xlab, y = ylab)

  return(graph)

}

#' gg_point_CaNuNu.
#' point
#' @name gg_point_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_CaNuNu. <- function(data, titleLabel = "", xLabel = NULL,
                             yLabel = NULL, leg_pos="right",
                             shape_point = 1, ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data) + theme(legend.position=leg_pos) +
    geom_point(aes(x = b, y = c, colour = a), shape = shape_point) +
    scale_color_manual(values = getPalette()) + theme_ds() +
    labs(title = titleLabel, x = xlab, y = ylab)

  return(graph)
}

