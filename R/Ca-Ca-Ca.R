
#' gg_treemap_x_CaCaCa.
#' Treemap fill first Ca
#' @name gg_treemap_x_CaCaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CaCaCa. <- function(data, titleLabel = "Report", fillLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "b", label = 'c'),
                     group.label.colour = "black", label.colour = "black") + #guides(fill=FALSE) +
    labs(title = titleLabel, fill = flabel)

  return(graph)
}

#' gg_treemap_y_CaCaCa.
#' Treemap fill second Ca
#' @name gg_treemap_y_CaCaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CaCaCa. <- function(data, titleLabel = "Report", fillLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a", label = "c"),
                     group.label.colour = "black", label.colour = "black") + #guides(fill=FALSE) +
    labs(title = titleLabel, fill = flabel)

  return(graph)
}

#' gg_treemap_z_CaCaCa.
#' Treemap fill third Ca
#' @name gg_treemap_z_CaCaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_z_CaCaCa. <- function(data, titleLabel = "Report", fillLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'c', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel)

  return(graph)
}
