
#' gg_treemap_density_x_CaCaNuNu.
#' Treemap Fill by second Nu
#' @name gg_treemap_density_x_CaCaNuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_x_CaCaNuNu. <- function(data, titleLabel = "Report", fillLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data$a <- as.factor(data$a)
  data$b <- as.factor(data$b)

  graph <- ggplotify(treemapify(data, area = "c", fill = 'd', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel)

  return(graph)
}

#' gg_treemap_density_y_CaCaNuNu.
#' Treemap Density by second Nu
#' @name gg_treemap_density_y_CaCaNuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_y_CaCaNuNu. <- function(data, titleLabel = "Report", fillLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data$a <- as.factor(data$a)
  data$b <- as.factor(data$b)

  graph <- ggplotify(treemapify(data, area = "d", fill = 'c', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel)

  return(graph)
}
