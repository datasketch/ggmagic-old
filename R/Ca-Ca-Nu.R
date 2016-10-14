library(reshape2)
library(ggplot2)
library(waffle)
library(extrafont)
library(dplyr)
library(grid)
library(gridExtra)
library(RColorBrewer)



#' circleAreaPlotCCN
#' circle area plot
#' @name circleAreaPlotCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
circleAreaPlotCCN  <- function(data, titleLabel = "Report", xLabel = "Category",
                              yLabel = "Category", leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(sum = sum(c)) %>%
    dplyr::arrange(desc(sum))
  graph <- ggplot(data_graph, aes(x=factor(a), y=factor(b), size=sum))
  graph <- graph + geom_point()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}


#' flip_circleAreaPlotCCN
#' flip circle area plot
#' @name flip_circleAreaPlotCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
flip_circleAreaPlotCCN  <- function(data, titleLabel = "Report", xLabel = "Category",
                                   yLabel = "Category", leg_pos = "top"){

  graph <- circleAreaPlotCCN(data, titleLabel, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' vertical_bargraphCCN
#' vertical bar graph
#' @name vertical_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
vertical_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                yLabel = "Frequency", fillLabel = "Types", leg_pos = "top"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(a, fill=b, weights = c)) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}


#' ordered_vertical_bargraphCCN
#' ordered vertical bar graph
#' @name ordered_vertical_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
ordered_vertical_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Frequency",
                                        yLabel =  "Categories", fillLabel = "Types",
                                        leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x=reorder(data$b,rep(1,length(data$b)),sum),fill=a, weights = c)) +
    geom_bar()

  graph <- graph + labs(title = titleLabel, x = yLabel, y = xLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}



#' ordered_horizontal_bargraphCCN
#' ordered horizontal bar graph
#' @name ordered_horizontal_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
ordered_horizontal_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Frequency",
                                          yLabel =  "Categories", fillLabel = "Types",
                                          leg_pos = "right"){

  graph <- ordered_vertical_bargraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}


#' horizontal_bargraphCCN
#' horizontal bar graph
#' @name horizontal_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
horizontal_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                  yLabel = "Category", fillLabel = "Types", leg_pos = "top"){

  graph <- vertical_bargraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}


#' vertical_dotgraphCCN
#' vertical dot graph
#' @name vertical_dotgraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
vertical_dotgraphCCN <- function(data, titleLabel = "Report", xLabel = "Categories", yLabel = "Frequency",
                                fillLabel = "Types", leg_pos = "right"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data = data, aes(a, fill = factor(b), weights = c)) +
    geom_dotplot(stackgroups = TRUE, binpositions = "all")

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + scale_y_continuous(breaks = NULL) +
    theme(legend.position=leg_pos)

  return(graph)
}



#' horizontal_dotgraphCCN
#' horizontal dot graph
#' @name horizontal_dotgraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
horizontal_dotgraphCCN <- function(data, titleLabel = "Report", xLabel = "Categories", yLabel = "Frequency",
                                  fillLabel = "Types", leg_pos = "top"){

  graph <- vertical_dotgraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}




#' vertical_unstacked_bargraphCCN
#' vertical unstacked bargraph
#' @name vertical_unstacked_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
vertical_unstacked_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                          yLabel = "Frequency", fillLabel = "Types",
                                          leg_pos = "top"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(a, weights = c)) + geom_bar(aes(fill=data$b), position = "dodge")
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}



#' horizontal_unstacked_bargraphCCN
#' horizontal unstacked bargraph
#' @name horizontal_unstacked_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
horizontal_unstacked_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                            yLabel = "Frequency", fillLabel = "Types",
                                            leg_pos = "top"){
  graph <- vertical_unstacked_bargraphCCN(data, titleLabel, xLabel, yLabel,
                                         fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}


#' horizontal_linegraphCCN
#' horizontal linegraph
#' @name horizontal_linegraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
horizontal_linegraphCCN <- function(data, titleLabel = "Report", xLabel = "Types",
                                   yLabel = "Frequency"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(sum = sum(c)) %>%
    dplyr::arrange(desc(sum))

  graph <- ggplot(data = data_graph, aes(x = a, y = sum, group=b)) + geom_line() +
    geom_point() + facet_grid(. ~b)
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)

  graph <- graph + theme_minimal()

  return(graph)
}

#' vertical_linegraphCCN
#' vertical linegraph
#' @name vertical_linegraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
vertical_linegraphCCN <- function(data, titleLabel = "Report", xLabel = "Types",
                                 yLabel = "Frequency"){

  graph <- horizontal_linegraphCCN(data, titleLabel, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}


#' gg_stacked_bar_ver_CaCaNu.
#' vertical stacked bar graph
#' @name gg_stacked_bar_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_bar_ver_CaCaNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                                        yLabel = NULL, fillLabel = NULL,
                                        leg_pos = "top"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(a, y = c, fill=b)) + geom_bar(stat="identity", position = "stack")
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill = flab)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_stacked_bar_hor_CaCaNu.
#' horizontal stacked bar graph
#' @name gg_stacked_bar_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_bar_hor_CaCaNu. <- function(data, titleLabel = "Report", xLabel = "Category",
                                          yLabel = "Frequency", fillLabel = "Types",
                                          leg_pos = "top"){


  graph <- gg_stacked_bar_ver_CaCaNu.(data, titleLabel, xLabel, yLabel,
                                       fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_stacked_bar_100_ver_CaCaNu.
#' 100% vertical stacked bar graph
#' @name gg_stacked_bar_100_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_bar_100_ver_CaCaNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL,
                                       leg_pos = "top"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(a, y = c, fill=b)) + geom_bar(stat="identity", position = "fill")
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill = flab)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_stacked_bar_100_hor_CaCaNu.
#' 100% horizontal stacked bar graph
#' @name gg_stacked_bar_100_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_bar_100_hor_CaCaNu. <- function(data, titleLabel = "Report", xLabel = "Category",
                                       yLabel = "Frequency", fillLabel = "Types",
                                       leg_pos = "top"){


  graph <- gg_stacked_bar_100_ver_CaCaNu.(data, titleLabel, xLabel, yLabel,
                                      fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' horizontal_area_bargraphCC
#' horizontal area bar graph
#' @name horizontal_area_bargraphCC
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
horizontal_area_bargraphCC <- function(data, titleLabel = "Report", xLabel = "Category",
                                       yLabel = "Frequency", fillLabel = "Types",
                                       leg_pos = "top"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(sum = sum(c)) %>%
    dplyr::arrange(desc(sum))
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=sum, group=b)) + geom_area(aes(fill = b), position = "stack")
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}
