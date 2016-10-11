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
    geom_smooth(method=lm, se=FALSE,colour="coral2", fill = "red", alpha = 0.05)
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
    geom_smooth(colour="coral2", fill = "red", alpha = 0.05)
  graph <- graph + labs(title = titleLabel, x = xLabel, y = ylab)
  graph <- graph + theme_minimal() + facet_grid(. ~a)

  return(graph)
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

