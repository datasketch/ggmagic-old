library(ggplot2)
library(waffle)
library(extrafont)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)
library(RColorBrewer)

#' gg_polar_bar_CaNu.
#' Polar Bar
#' @name gg_polar_bar_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_polar_bar_CaNu. <- function(data, width = 0.95, titleLabel = "Report",
                                   fillLabel = NULL, leg_pos= "right"){
  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, weight = b, fill = a)) + geom_bar(width = width) +
    coord_polar() + labs(title = titleLabel, fill = fillLabel, x = "", y = "") + theme_bw() +
    theme(legend.position=leg_pos)

  return(graph)
}






#' gg_stacked_hist_ver_CaNu.
#' Stacked Vertical Histogram
#' @name gg_stacked_hist_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_hist_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                            fillLabel = NULL, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_histogram(aes(fill = a), binwidth = 10) +
    labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) +
    theme_minimal() + theme(legend.position=leg_pos)
  return(graph)
}

#' gg_coloured_multi_density_dist_CaNu.
#' Coloured Density Distribution
#' @name gg_coloured_multi_density_dist_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_coloured_multi_density_dist_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                                 fillLabel = NULL, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a)) +
    labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) +
    theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_area_multi_density_dist_CaNu.
#' Filled Density Distribution
#' @name gg_area_multi_density_dist_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_multi_density_dist_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                             fillLabel = NULL, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(fill = a)) +
    labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) +
    theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_facet_dist_ver_CaNu.
#' Facet Vertical Dist
#' @name gg_facet_dist_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dist_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                            fillLabel = NULL, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a)) + theme(legend.position=leg_pos) +
    labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) + theme_minimal()
  graph <- graph + facet_grid(. ~a)

  return(graph)
}

#' gg_facet_dist_hor_CaNu.
#' Facet Horizontal Dist
#' @name gg_facet_dist_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dist_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                            fillLabel = NULL, leg_pos="right"){

  graph <- gg_facet_density_dist_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_hist_ver_CaNu.
#' Facet Vertical Histogram + Mean
#' @name gg_facet_hist_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_hist_mean_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                                    yLabel = "Count", fillLabel = NULL, leg_pos='right'){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram() + theme(legend.position=leg_pos) +
    facet_grid(. ~a) + geom_vline(aes(xintercept = mean(b)), linetype = "dashed", size = 1, colour = "red")
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) + theme_minimal()

  return(graph)
}

#' gg_facet_hist_mean_hor_CaNu.
#' Facet Horizontal Histogram + Mean
#' @name gg_facet_hist_mean_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_hist_mean_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                    fillLabel = NULL, leg_pos="right"){

  graph <- gg_facet_hist_mean_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_hist_ver_CaNu.
#' Facet Vertical Histogram
#' @name gg_facet_hist_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_hist_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                                         yLabel = "Count", fillLabel = NULL, leg_pos='right'){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram() + theme(legend.position=leg_pos) +
    facet_grid(. ~a) + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) + theme_minimal()

  return(graph)
}

#' gg_facet_hist_hor_CaNu.
#' Facet Horizontal Histogram
#' @name gg_facet_hist_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_hist_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                         fillLabel = NULL, leg_pos="right"){

  graph <- gg_facet_hist_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_dist_hist_hor_CaNu.
#' Facet Vertical Histogram + Dist
#' @name gg_facet_dist_hist_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dist_hist_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                         fillLabel = NULL, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density..)) + geom_density(col="red") +
    theme(legend.position=leg_pos) + theme_minimal()
  graph <- graph + facet_grid(. ~a) + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel)

  return(graph)
}

#' gg_facet_dist_hist_hor_CaNu.
#' Facet Horizontal Histogram + Dist
#' @name gg_facet_dist_hist_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dist_hist_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                    fillLabel = NULL, leg_pos="right"){

  graph <- gg_facet_dist_hist_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_dist_hist_mean_ver_CaNu.
#' Facet Vertical Histogram + Dist + Mean
#' @name gg_facet_dist_hist_mean_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dist_hist_mean_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                         fillLabel = NULL, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density..)) + geom_density(col="blue") +
    geom_vline(aes(xintercept = mean(b)), linetype = "dashed", size = 1, colour = "red") +
    theme(legend.position=leg_pos) + theme_minimal()
  graph <- graph + facet_grid(. ~a) + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel)

  return(graph)
}

#' gg_facet_dist_hist_mean_hor_CaNu.
#' Facet Horizontal Histogram + Dist + Mean
#' @name gg_facet_dist_hist_mean_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dist_hist_mean_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                         fillLabel = NULL, leg_pos="right"){

  graph <- gg_facet_dist_hist_mean_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_dot_dist_ver_CaNu.
#' Facet Vertical Dot Dist
#' @name gg_facet_dot_dist_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_dist_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                    fillLabel = NULL, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a)) + geom_point(aes(y=0),size = 5,alpha = 0.3, color = "#D55E00") +
    theme(legend.position=leg_pos) + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) + theme_minimal()
  graph <- graph + facet_grid(. ~a)

  return(graph)
}

#' gg_facet_dot_dist_hor_CaNu.
#' Facet Horizontal Dot Dist
#' @name gg_facet_dot_dist_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_dist_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                        fillLabel = NULL, leg_pos="right"){

  graph <- gg_facet_dot_dist_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_dot_hist_ver_CaNu.
#' Facet Vertical Dot Histogram
#' @name gg_facet_dot_hist_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_hist_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                                    yLabel = "Count", fillLabel = NULL, leg_pos='right'){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram() + geom_point(aes(y=0),size = 5,alpha = 0.3, color = "#D55E00") +
    theme(legend.position=leg_pos) + facet_grid(. ~a) +
    labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) + theme_minimal()

  return(graph)
}

#' gg_facet_dot_hist_hor_CaNu.
#' Facet Horizontal Histogram + Dot
#' @name gg_facet_dot_hist_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_hist_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                    fillLabel = NULL, leg_pos="right"){

  graph <- gg_facet_dot_hist_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_dot_hist_mean_ver_CaNu.
#' Facet Vertical Histogram + Mean + Dot
#' @name gg_facet_dot_hist_mean_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_hist_mean_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                                         yLabel = "Count", fillLabel = NULL, leg_pos='right'){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram() + theme(legend.position=leg_pos) +
    facet_grid(. ~a) + geom_vline(aes(xintercept = mean(b)), linetype = "dashed", size = 1, colour = "red") +
    geom_point(aes(y=0),size = 5,alpha = 0.3, color = "#D55E00")
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) + theme_minimal()

  return(graph)
}

#' gg_facet_dot_hist_mean_hor_CaNu.
#' Facet Horizontal Histogram + Mean + Dot
#' @name gg_facet_dot_hist_mean_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_hist_mean_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                         fillLabel = NULL, leg_pos="right"){

  graph <- gg_facet_dot_hist_mean_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_dot_dist_hist_ver_CaNu.
#' Facet Vertical Histogram + Dist + Dot
#' @name gg_facet_dot_dist_hist_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_dist_hist_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                         fillLabel = NULL, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density..)) + geom_density(col="red") +
    geom_point(aes(y=0),size = 5,alpha = 0.3, color = "#D55E00") +
    theme(legend.position=leg_pos) + theme_minimal()
  graph <- graph + facet_grid(. ~a) + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel)

  return(graph)
}

#' gg_facet_dot_dist_hist_hor_CaNu.
#' Facet Horizontal Histogram + Dist + Dot
#' @name gg_facet_dot_dist_hist_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_dist_hist_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                         fillLabel = NULL, leg_pos="right"){

  graph <- gg_facet_dot_dist_hist_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}


#' gg_facet_dot_dist_hist_mean_ver_CaNu.
#' Facet Vertical Histogram + Dist + Mean + Dot
#' @name gg_facet_dot_dist_hist_mean_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_dist_hist_mean_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                              fillLabel = NULL, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density..)) + geom_density(col="blue") +
    geom_vline(aes(xintercept = mean(b)), linetype = "dashed", size = 1, colour = "red") +
    geom_point(aes(y=0),size = 5,alpha = 0.3, color = "#D55E00") +
    theme(legend.position=leg_pos) + theme_minimal()
  graph <- graph + facet_grid(. ~a) + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel)

  return(graph)
}

#' gg_facet_dot_dist_hist_mean_hor_CaNu.
#' Facet Horizontal Histogram + Dist + Mean + Dot
#' @name gg_facet_dot_dist_hist_mean_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_dist_hist_mean_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                              fillLabel = NULL, leg_pos="right"){

  graph <- gg_facet_dot_dist_hist_mean_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_point_CaNu.
#' Facet Point
#' @name gg_facet_point_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_point_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index', yLabel = NULL,
                                 fillLabel = NULL, type = 1){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x=xorder, y=b)) + geom_point(shape = type)
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab)
  graph <- graph + theme_minimal() + facet_grid(. ~a)

  return(graph)
}

#' gg_facet_line_point_CaNu.
#' Facet Line Point
#' @name gg_facet_line_point_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_line_point_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index', yLabel = NULL,
                                 fillLabel = NULL, type = 1){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x=xorder, y=b)) + geom_point(shape = type) + geom_line()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab)
  graph <- graph + theme_minimal() + facet_grid(. ~a)

  return(graph)
}

#' gg_facet_line_CaNu.
#' Facet Line
#' @name gg_facet_line_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_line_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index', yLabel = NULL,
                                      fillLabel = NULL, type = 1){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x=xorder, y=b)) + geom_line()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab)
  graph <- graph + theme_minimal() + facet_grid(. ~a)

  return(graph)
}

#' gg_facet_area_ver_CaNu.
#' Facet Vertical Area
#' @name gg_facet_area_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_area_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = "Index",
                                      yLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- xLabel %||% nms[1]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data = data, aes(x=xorder, y=b, group=a)) + geom_area()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab)
  graph <- graph + theme_minimal() + facet_grid(. ~a)

  return(graph)
}


#' gg_facet_area_hor_CaNu.
#' Facet Horizontal Area
#' @name gg_facet_area_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_area_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = "Index",
                                    yLabel = NULL){

  graph <- gg_facet_area_ver_CaNu.(data, titleLabel, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_stacked_area_100_ver_CaCa.
#' Stacked Vertical Area 100%
#' @name gg_stacked_area_100_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_area_100_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index',
                                          yLabel = NULL, fillLabel = NULL,
                                          leg_pos = "top"){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  data_graph <- data %>% arrange(xorder) %>%
    tidyr::spread(xorder, b) %>% tidyr::gather(xorder, b, -a)
  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph,
                  aes(x=xorder, y=b, group=a)) + geom_area(aes(fill = a), position = "fill")
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab, fill = flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)
  return(graph)
}

#' gg_stacked_area_100_hor_CaNu.
#' Stacked Horizontal Area 100%
#' @name gg_stacked_area_100_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_area_100_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index',
                                          yLabel = NULL, fillLabel = NULL,
                                          leg_pos = "top"){

  graph <- gg_stacked_area_100_ver_CaNu.(data, titleLabel, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_stacked_area_ver_CaNu.
#' Stacked Vertical Area
#' @name gg_stacked_area_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_area_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index',
                                          yLabel = NULL, fillLabel = NULL,
                                          leg_pos = "top"){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  data_graph <- data %>% arrange(xorder) %>%
    tidyr::spread(xorder, b) %>% tidyr::gather(xorder, b, -a)
  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph,
                  aes(x=xorder, y=b, group=a)) + geom_area(aes(fill = a), position = "stack")
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab, fill = flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)
  return(graph)
}

#' gg_stacked_area_hor_CaNu.
#' Stacked Horizontal Area
#' @name gg_stacked_area_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_area_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index',
                                          yLabel = NULL, fillLabel = NULL,
                                          leg_pos = "top"){

  graph <- gg_stacked_area_ver_CaNu.(data, titleLabel, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_grouped_point_CaNu.
#' Grouped Color Point
#' @name gg_grouped_point_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_grouped_point_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index', yLabel = NULL,
                                 fillLabel = NULL, leg_pos="right", type = 1){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x=xorder, y=b)) + geom_point(aes(color = a), shape = type)
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab, fill = flabel)
  graph <- graph + theme_minimal()

  return(graph)
}

#' gg_multi_line_point_CaNu.
#' Grouped Line Color Point
#' @name gg_multi_line_point_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_point_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index', yLabel = NULL,
                                   fillLabel = NULL, leg_pos="right", type = 1){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x=xorder, y=b)) + geom_point(aes(color = a), shape = type) + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab, fill = flabel)
  graph <- graph + theme_minimal()

  return(graph)
}

#' gg_multi_line_CaNu.
#' Grouped Line Coloured
#' @name gg_multi_line_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index', yLabel = NULL,
                                        fillLabel = NULL, leg_pos="right", type = 1){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x=xorder, y=b)) + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab, fill = flabel)
  graph <- graph + theme_minimal()

  return(graph)
}

#' gg_facet_point_trend_line_CaNu.
#' Facet Trend Line
#' @name gg_facet_point_trend_line_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_point_trend_line_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index', yLabel = NULL,
                                            fillLabel = NULL, type = 1){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x = xorder, y = b)) + geom_point(shape = type) +
    geom_smooth(method=lm, se=FALSE,colour="red", fill = "red", alpha = 0.05)
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab)
  graph <- graph + theme_minimal() + facet_grid(. ~a)

  return(graph)
}

gg_facet_trend_ribbon_CaNu. <- function(data, titleLabel = "Report", xLabel = 'Index', yLabel = NULL,
                                        fillLabel = NULL, type = 1){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x = xorder, y = b)) + geom_point(shape = type) +
    geom_smooth(colour="red", fill = "red", alpha = 0.05)
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab)
  graph <- graph + theme_minimal() + facet_grid(. ~a)

  return(graph)
}

#Width debe de ser un parámetro.  0 < width < 1.
#' gg_donut_CaNu.
#' Donut
#' @name gg_donut_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_CaNu. <- function(data, titleLabel = "Report", fillLabel = NULL,
                         width = 0.3, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = b)) +
    geom_bar(width = width) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_bullseye_CaNu.
#' Bullseye
#' @name gg_bullseye_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_CaNu. <- function(data, titleLabel = "Report", fillLabel = NULL,
                            leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = b)) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_dot_bar_ver_CaNu.
#' Vertical Dot Bar
#' @name gg_dot_bar_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_ver_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                               fillLabel = NULL, leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = sum())

  data_graph <- data_graph %>%
    mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = order, fill = factor(a), weight = b)) + geom_dotplot(method="histodot")

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel,  fill = flabel)
  graph <- graph + theme_minimal() + scale_y_continuous(breaks = NULL) +
    theme(legend.position=leg_pos)


  return(graph)
}

#' gg_dot_bar_ver_CaNu.
#' Vertical Dot Bar
#' @name gg_dot_bar_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_hor_CaNu. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                                 fillLabel = NULL, leg_pos = "right"){

  graph <- gg_dot_bar_ver_CaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_single_stacked_bar_hor_CaNu.
#' Single Horizontal Stacked Bar
#' @name gg_single_stacked_bar_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_single_stacked_bar_hor_CaNu. <- function(data, titleLabel = "Report", yLabel = "Count",
                                          fillLabel = "Types", leg_pos="right", width = 0.3){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = b)) + geom_bar(width = width)
  graph <- graph + labs(title = titleLabel, x = "", y = yLabel, fill = fillLabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}


#' gg_single_stacked_bar_ver_CaNu.
#' Single Vertical Stacked Bar
#' @name gg_single_stacked_bar_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_single_stacked_bar_ver_CaNu. <- function(data, titleLabel = "Report", yLabel = "Frequency",
                                          fillLabel = "Types", leg_pos="right", width = 0.3){

  graph <- gg_single_stacked_bar_hor_CaNu.(data, titleLabel, yLabel, fillLabel, leg_pos, width)
  graph <- graph + coord_flip()

  return(graph)
}


#' gg_gauge_CaNu.
#' Gauge
#' @name gg_gauge_CaNu.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_gauge_CaNu. <- function(data){

  gg.gauge <- function(pos, breaks=c(0,30,70,100)) {
    require(ggplot2)
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
      th.start <- pi*(1-a/100)
      th.end   <- pi*(1-b/100)
      th       <- seq(th.start,th.end,length=100)
      x        <- c(r1*cos(th),rev(r2*cos(th)))
      y        <- c(r1*sin(th),rev(r2*sin(th)))
      return(data.frame(x,y))
    }
    graph <- ggplot()+
      geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
      geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
      geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="forestgreen")+
      geom_polygon(data=get.poly(as.numeric(pos[[1]])-1,as.numeric(pos[[1]])+1,0.2),aes(x,y))+
      geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
                aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
      annotate("text",x=0,y=0,label=as.character(pos[[2]]),vjust=0,size=8,fontface="bold")+
      coord_fixed()+
      theme_bw()+
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    return(graph)
  }

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(sum = sum(b)) %>%
    dplyr::arrange(desc(sum)) %>%
    dplyr::mutate(order = 1:nrow(.), prop = sum/sum(sum))

  newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
                    data_graph$a, SIMPLIFY=FALSE)

  graphList <- lapply(newList, gg.gauge)
  grid.newpage()
  grid.draw(arrangeGrob(grobs = graphList,ncol=2))
}

#' gg_gauge_dial_Ca.
#' Gauge
#' @name gg_gauge_dial_Ca.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_gauge_dial_Ca. <- function(data){

  gg.gauge <- function(pos, breaks=c(0,50,100)) {
    require(ggplot2)
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
      th.start <- pi*(1-a/100)
      th.end   <- pi*(1-b/100)
      th       <- seq(th.start,th.end,length=100)
      x        <- c(r1*cos(th),rev(r2*cos(th)))
      y        <- c(r1*sin(th),rev(r2*sin(th)))
      return(data.frame(x,y))
    }
    graph <- ggplot()+
      geom_polygon(data=get.poly(breaks[1],as.numeric(pos[[1]])),aes(x,y),fill="skyblue")+
      geom_polygon(data=get.poly(as.numeric(pos[[1]]),breaks[3]),aes(x,y),fill="gold")+
      annotate("text",x=0,y=0,label=as.character(pos[[2]]),vjust=0,size=8,fontface="bold")+
      coord_fixed()+
      theme_bw()+
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    return(graph)
  }

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(sum = sum(b)) %>%
    dplyr::arrange(desc(sum)) %>%
    dplyr::mutate(order = 1:nrow(.), prop = sum/sum(sum))


  newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
                    data_graph$a, SIMPLIFY=FALSE)

  graphList <- lapply(newList, gg.gauge)
  grid.newpage()
  grid.draw(arrangeGrob(grobs = graphList,ncol=2))
}




#boxplots
boxSpltPlot <- function(data, titleLabel = "", xLabel = "", yLabel = "", fillLabel = "", voltear = TRUE){
  boxplot <- ggplot(data, mapping = aes(x = a, y = b, fill = a))
  boxPlot <- boxplot + geom_boxplot() + theme_bw()+ labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel)
  if(voltear){
    boxPlot <- boxPlot + coord_flip()

  }


  return(boxPlot)
}

#violin plots

ViolinMultPlot <- function(data, titleLabel = "", xLabel = "", yLabel = "", fillLabel = "", voltear = TRUE){
  violin <- ggplot(data, mapping = aes(x = a, y = b, fill = a))
  violin <- violin + geom_violin() + theme_bw()+ labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel)


  if(voltear){
    violin <- violin + coord_flip()

  }
  return(violin)
}

#violin plots + obs dots

ViolinDotMultPlot <- function(data, titleLabel = "", xLabel = "", yLabel = "", fillLabel = "", voltear = TRUE){
  violin <- ggplot(data, mapping = aes(x = a, y = b, fill = a))
  violin <- violin  + geom_jitter(aes(alpha = 1), height = 1, size = 1) + geom_violin() + theme_bw()+ labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel)

  if(voltear){
    violin <- violin + coord_flip()

  }
  return(violin)
}

