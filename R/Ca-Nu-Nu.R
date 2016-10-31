#' gg_steamgraph_CaNuNu.
#' steamgraph
#' @name gg_steamgraph_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steamgraph_CaNuNu. <- function(data, titleLabel = "", fillLabel = NULL, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  ggplot(data, aes(x = b, y = c, group = a, fill = a)) +
  stat_steamgraph() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position=leg_pos) +
  scale_fill_manual(values = getPalette()) +
  guides(text = FALSE) + theme_bw()
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
gg_lines_CaNuNu. <- function(data, titleLabel = "", fillLabel = NULL, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data$a <- as.factor(data$a)
  ggplot(data, aes(b, c, colour = a)) +  geom_line(aes(group = a))  +
  theme(legend.position=leg_pos) +
  scale_fill_manual(values = getPalette()) +
  guides(text = FALSE) + theme_bw()

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
gg_point_CaNuNu. <- function(data, titleLabel = "", fillLabel = NULL, leg_pos="right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  ggplot(data, aes(x = b, y = c, fill = a, color = a))  +
  geom_point(size = 10) +  theme(legend.position=leg_pos) +
  scale_fill_manual(values = getPalette()) +
  guides(text = FALSE) + theme_bw()
}

