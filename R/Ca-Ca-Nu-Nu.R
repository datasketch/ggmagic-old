
#' gg_treemap_density_x_CaCaNuNu.
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
                                           fillLabel = NULL, reverse = FALSE, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data$a <- as.factor(data$a)
  data$b <- as.factor(data$b)

  graph <- ggplotify(treemapify(data, area = "c", fill = 'd', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption)


  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }


  graph
}

#' gg_treemap_density_y_CaCaNuNu.
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
                                           fillLabel = NULL,reverse = FALSE, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data$a <- as.factor(data$a)
  data$b <- as.factor(data$b)

  graph <- ggplotify(treemapify(data, area = "d", fill = 'c', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }
  graph
}
