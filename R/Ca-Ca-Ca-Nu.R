
#' gg_treemap_x_CaCaCaNu.
#' Treemap fill first Ca
#' @name gg_treemap_x_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CaCaCaNu. <- function(data, titleLabel = "Report", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(Sum = sum(d)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'a', group = "b", label = 'c'),
                     group.label.colour = "black", label.colour = "black") + #guides(fill=FALSE) +
    labs(title = titleLabel, fill = flabel)

  return(graph)
}

#' gg_treemap_y_CaCaCaNu.
#' Treemap fill second Ca
#' @name gg_treemap_y_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CaCaCaNu. <- function(data, titleLabel = "Report", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(Sum = sum(d)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'b', group = "a", label = "c"),
                     group.label.colour = "black", label.colour = "black") + #guides(fill=FALSE) +
    labs(title = titleLabel, fill = flabel)

  return(graph)
}

#' gg_treemap_z_CaCaCaNu.
#' Treemap fill third Ca
#' @name gg_treemap_z_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_z_CaCaCaNu. <- function(data, titleLabel = "Report", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(Sum = sum(d)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'c', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel)

  return(graph)
}
