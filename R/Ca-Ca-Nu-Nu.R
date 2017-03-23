#' Treemap density first numeric variable
#' Treemap Fill by second Nu
#' @name gg_treemap_density_x_CaCaNuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_x_CaCaNuNu. <- function(data, titleLabel = "",  subtitle = "", caption = "",
                                           fillLabel = NULL, reverse = FALSE,
                                           text = TRUE, color_text = "black", aggregation = "sum", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)


  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }


  graph
}

#' Treemap density second numeric variable
#' Treemap Density by second Nu
#' @name gg_treemap_density_y_CaCaNuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_y_CaCaNuNu. <- function(data, titleLabel = "",  subtitle = "", caption = "",
                                           fillLabel = NULL,reverse = FALSE,
                                           text = TRUE, color_text = "black", aggregation = "sum", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)


  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }


  graph
}
