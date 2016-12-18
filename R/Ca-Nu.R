#' gg_pie_CaNu.
#' Pie
#' @name gg_pie_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                         text = TRUE, type = 'percent', text_size = 3,
                         leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4))

  graph <- ggplot(data=data_graph, aes(x = factor(1), weight = count, fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph + theme(legend.position=leg_pos) + guides(text = FALSE)
  graph <- graph + theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette())


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(count,2)), size = text_size))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")), size = text_size))
    }else{
      graph
    }
  }
}

#' gg_bar_coloured_x_ver_CaNu.
#' vertical bar
#' @name gg_bar_coloured_x_ver_CaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_ver_CaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,text = TRUE, type = 'percent', text_size = 3,
                                       yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = factor(a))) + geom_bar(stat = "identity") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +  theme_ds() +
    theme(legend.position=leg_pos) + scale_fill_manual(values = getPalette()) + guides(fill = FALSE)


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = count + 0.05, label = round(count,2)), size = text_size, position = position_dodge(0.9), vjust = 0))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = count + 0.05, label = paste(percent, "%", sep = "")), size = text_size, position = position_dodge(0.9), vjust = 0))
    }else{
      graph
    }
  }

}

#' gg_bar_coloured_x_hor_CaNu.
#' horizontal bar
#' @name gg_bar_coloured_x_hor_CaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_hor_CaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, text = TRUE, type = 'percent', text_size = 3,
                                       yLabel = NULL, leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = factor(a))) + geom_bar(stat = "identity") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +  theme_ds() +
    theme(legend.position=leg_pos) + scale_fill_manual(values = getPalette()) + guides(fill = FALSE) + coord_flip()


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = count, label = round(count,2)), size = text_size))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = count, label = paste(percent, "%", sep = "")), size = text_size))
    }else{
      graph
    }
  }
}

#' gg_bar_coloured_y_ver_CaNu.
#' vertical bar
#' @name gg_bar_coloured_y_ver_CaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_y_ver_CaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, reverse = FALSE,
                                       text = TRUE, type = 'percent', text_size = 3,
                                       yLabel = NULL, leg_pos = "right", ...){



  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(suma=sum(b)) %>%
    dplyr::mutate(percent = 100 * round(suma/sum(suma), 4))

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = suma)) + geom_bar(stat = "identity") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    theme(legend.position=leg_pos)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = suma, label = round(suma,2)), size = text_size))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = suma, label = paste(percent, "%", sep = "")), size = text_size))
    }else{
      graph
    }
  }
}

#' gg_bar_coloured_y_hor_CaNu.
#' horizontal bar
#' @name gg_bar_coloured_y_hor_CaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_y_hor_CaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       reverse = FALSE, text = TRUE, type = 'percent', text_size = 3,
                                       yLabel = NULL, leg_pos = "right", ...){

  graph <- gg_bar_coloured_y_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel,
                                       reverse, text, type, text_size,
                                       yLabel, leg_pos)
  graph + coord_flip()
}

#' gg_bar_coloured_parameter_ver_CaNu.
#' Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, parameter = NULL,
                                                leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  p <-  parameter %||% sample(unique(data[,nms[1]]), 1)
  data <- f$d
  graph <- ggplot(data, aes(x = a, y = b)) +
    geom_bar(stat="identity", aes(fill = a %in% p ))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + guides(fill = FALSE) + theme(legend.position = leg_pos) +
    theme_ds() + scale_fill_manual(values = getPalette())
  graph
}

#' gg_bar_coloured_parameter_hor_CaNu.
#' Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, parameter = NULL,
                                                leg_pos = "right", ...){

  graph <- gg_bar_coloured_parameter_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel,
                                               yLabel, parameter, leg_pos = "right")
  graph <- graph + coord_flip()
  graph
}

#' gg_bubble_CaNu.
#' Bubble
#' @name gg_bubble_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = 0, size = b, color = ""))
  graph <- graph + geom_point(show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = "") +
    scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  graph
}

#' gg_bubble_coloured_CaNu.
#' Coloured Bubble
#' @name gg_bubble_coloured_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_CaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = 0, size = b))
  graph <- graph + geom_point(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = "")

  graph <- graph + scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  graph
}

#' gg_bar_polar_CaNu.
#' Polar Bar
#' @name gg_bar_polar_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_CaNu. <- function(data, width = 0.95, titleLabel = "", subtitle = "",
                               caption = "", leg_pos= "right", ...){
  f <- fringe(data)
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, weight = b, fill = a)) + geom_bar(width = width) +
    coord_polar() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
    theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette())

  graph
}


#' gg_bar_circular_CaNu.
#' Circular Bar
#' @name gg_bar_circular_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                  leg_pos="right", width = 0.85, ...){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::arrange(count)

  graph <- ggplot(data_graph, aes(x = reorder(a,count), y = count , fill = a )) +
    geom_bar(width = width, stat="identity") + coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
    theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette())

  graph
}


#' gg_hist_stacked_ver_CaNu.
#' Stacked Vertical Histogram
#' @name gg_hist_stacked_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_stacked_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = 'Count', leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_histogram(aes(fill = a), binwidth = 10) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
    theme_ds() + theme(legend.position=leg_pos) + scale_fill_manual(values = getPalette())
  graph
}

#' gg_density_multi_dist_coloured_CaNu.
#' Coloured Density Distribution
#' @name gg_density_multi_dist_coloured_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_density_multi_dist_coloured_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                 xLabel = NULL, yLabel = 'Count', leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
    theme_ds() + theme(legend.position=leg_pos) + scale_color_manual(values = getPalette())

  graph
}

#' gg_area_multi_density_dist_CaNu.
#' Filled Density Distribution
#' @name gg_area_multi_density_dist_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_multi_density_dist_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = 'Count', leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(fill = a)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
    theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

  graph
}

#' gg_dist_ver_facet_CaNu.
#' Facet Vertical Dist
#' @name gg_dist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = 'Count', leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a), show.legend = FALSE) + theme(legend.position=leg_pos) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) + theme_ds()
  graph <- graph + facet_grid(. ~a) + scale_color_manual(values = getPalette())

  graph
}

#' gg_dist_hor_facet_CaNu.
#' Facet Horizontal Dist
#' @name gg_dist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = 'Count', leg_pos="right", ...){

  graph <- gg_dist_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)

  graph <- graph + coord_flip()

  graph
}

#' gg_hist_ver_mean_facet_CaNu.
#' Facet Vertical Histogram + Mean
#' @name gg_hist_ver_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_ver_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = "Count", leg_pos='right', ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
    theme(legend.position=leg_pos) + facet_grid(. ~a) +
    geom_vline(aes(xintercept = mean(b), color = ""), linetype = "dotted", size = 1, show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) + theme_ds() +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2])

  graph
}

#' gg_hist_hor_mean_facet_CaNu.
#' Facet Horizontal Histogram + Mean
#' @name gg_hist_hor_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_hor_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = 'Count', leg_pos="right", ...){

  graph <- gg_hist_ver_mean_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)

  graph <- graph + coord_flip()

  graph
}

#' gg_hist_ver_facet_CaNu.
#' Facet Vertical Histogram
#' @name gg_hist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = "Count", leg_pos='right', ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
    theme(legend.position=leg_pos) + facet_grid(. ~a) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) + theme_ds() +
    scale_fill_manual(values = getPalette())

  graph
}

#' gg_hist_hor_facet_CaNu.
#' Facet Horizontal Histogram
#' @name gg_hist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = 'Count', leg_pos="right", ...){

  graph <- gg_hist_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)

  graph <- graph + coord_flip()

  graph
}

#' gg_dist_hist_ver_facet_CaNu.
#' Facet Vertical Histogram + Dist
#' @name gg_dist_hist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = 'Count', leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    theme(legend.position=leg_pos) + theme_ds() +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + facet_grid(. ~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel)

  graph
}

#' gg_dist_hist_hor_facet_CaNu.
#' Facet Horizontal Histogram + Dist
#' @name gg_dist_hist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = 'Count', leg_pos="right", ...){

  graph <- gg_dist_hist_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)

  graph <- graph + coord_flip()

  graph
}

#' gg_dist_hist_ver_mean_facet_CaNu.
#' Facet Vertical Histogram + Dist + Mean
#' @name gg_dist_hist_ver_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_ver_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = 'Count', leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    geom_vline(aes(xintercept = mean(b), color = "*"), linetype = "dotted", size = 1, show.legend = FALSE) +
    theme(legend.position=leg_pos) + theme_ds() +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + facet_grid(. ~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel)

  graph
}

#' gg_dist_hist_hor_mean_facet_CaNu.
#' Facet Horizontal Histogram + Dist + Mean
#' @name gg_dist_hist_hor_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_hor_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = 'Count', leg_pos="right", ...){

  graph <- gg_dist_hist_ver_mean_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)

  graph <- graph + coord_flip()

  graph
}

#' gg_dot_dist_ver_facet_CaNu.
#' Facet Vertical Dot Dist
#' @name gg_dot_dist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = 'Count', leg_pos="right", size = 3, alpha = 0.3, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a), show.legend = FALSE) +
    geom_point(aes(y = 0), color = "#D55E00", size = size, alpha = alpha, show.legend = FALSE) +
    theme(legend.position = leg_pos) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) + theme_ds() +
    scale_color_manual(values = getPalette())
  graph <- graph + facet_grid(. ~a)

  graph
}

#' gg_dot_dist_hor_facet_CaNu.
#' Facet Horizontal Dot Dist
#' @name gg_dot_dist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = 'Count', leg_pos="right", size = 3, alpha = 0.3, ...){

  graph <- gg_dot_dist_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, size, alpha)

  graph <- graph + coord_flip()

  graph
}

#' gg_dot_hist_ver_facet_CaNu.
#' Facet Vertical Dot Histogram
#' @name gg_dot_hist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = "Count", leg_pos='right', size = 3, alpha = 0.3,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
    geom_point(aes(y=0),size = size,alpha = alpha, color = "#D55E00") +
    theme(legend.position=leg_pos) + facet_grid(. ~a) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) + theme_ds()

  graph
}

#' gg_dot_hist_hor_facet_CaNu.
#' Facet Horizontal Histogram + Dot
#' @name gg_dot_hist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = 'Count', leg_pos="right", size = 3, alpha = 0.3, ...){

  graph <- gg_dot_hist_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, size, alpha)

  graph <- graph + coord_flip()

  graph
}

#' gg_dot_hist_ver_mean_facet_CaNu.
#' Facet Vertical Histogram + Mean + Dot
#' @name gg_dot_hist_ver_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_ver_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = "Count", leg_pos='right', size = 3, alpha = 0.3, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
    theme(legend.position=leg_pos) + facet_grid(. ~a) +
    geom_vline(aes(xintercept = mean(b), color = ""), linetype = "dotted", size = 1, show.legend = FALSE) +
    geom_point(aes(y = 0), size = size, alpha = alpha, color = "#D55E00")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) + theme_ds() +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2])

  graph
}

#' gg_dot_hist_hor_mean_facet_CaNu.
#' Facet Horizontal Histogram + Mean + Dot
#' @name gg_dot_hist_hor_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_hor_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = 'Count', leg_pos="right",  size = 3, alpha = 0.3, ...){

  graph <- gg_dot_hist_ver_mean_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, size, alpha)

  graph <- graph + coord_flip()

  graph
}

#' gg_dot_dist_hist_ver_facet_CaNu.
#' Facet Vertical Histogram + Dist + Dot
#' @name gg_dot_dist_hist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = 'Count', leg_pos="right", size = 3, alpha = 0.3, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color=""), show.legend = FALSE) +
    geom_point(aes(y = 0), size = size, alpha = alpha, color = "#D55E00") +
    theme(legend.position=leg_pos) + theme_ds() +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette())
  graph <- graph + facet_grid(. ~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel)

  graph
}

#' gg_dot_dist_hist_hor_facet_CaNu.
#' Facet Horizontal Histogram + Dist + Dot
#' @name gg_dot_dist_hist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = 'Count', leg_pos="right", size = 3, alpha = 0.3, ...){

  graph <- gg_dot_dist_hist_ver_facet_CaNu.(data, titleLabel,subtitle, caption, xLabel, yLabel, leg_pos, size, alpha)

  graph <- graph + coord_flip()

  graph
}


#' gg_dot_dist_hist_ver_mean_facet_CaNu.
#' Facet Vertical Histogram + Dist + Mean + Dot
#' @name gg_dot_dist_hist_ver_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_ver_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = 'Count',
                                                  leg_pos="right", size = 3, alpha = 0.3, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    geom_vline(aes(xintercept = mean(b), colour = "*"), linetype = "dotted", size = 1, show.legend = FALSE) +
    geom_point(aes(y=0),size = size, alpha = alpha, color = "#D55E00") +
    theme(legend.position=leg_pos) + theme_ds() + scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette())
  graph <- graph + facet_grid(. ~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel)

  graph
}

#' gg_dot_dist_hist_hor_mean_facet_CaNu.
#' Facet Horizontal Histogram + Dist + Mean + Dot
#' @name gg_dot_dist_hist_hor_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_hor_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = 'Count', leg_pos="right", size = 3, alpha = 0.3, ...){

  graph <- gg_dot_dist_hist_ver_mean_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, size, alpha)

  graph <- graph + coord_flip()

  graph
}

#' gg_point_facet_CaNu.
#' Facet Point
#' @name gg_point_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index',
                                 yLabel = NULL, type = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x=xorder, y=b)) + geom_point(shape = type, aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + facet_grid(. ~a)

  graph
}

#' gg_line_point_facet_CaNu.
#' Facet Line Point
#' @name gg_line_point_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index',
                                      yLabel = NULL, type = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x = xorder, y = b)) + geom_point(shape = type, aes(color = ""), show.legend = FALSE) +
    geom_line(aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + facet_grid(. ~a)

  graph
}

#' gg_line_facet_CaNu.
#' Facet Line
#' @name gg_line_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index',
                                yLabel = NULL, type = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x=xorder, y=b)) + geom_line(aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + facet_grid(. ~a)

  graph
}

#' gg_area_ver_facet_CaNu.
#' Facet Vertical Area
#' @name gg_area_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Index",
                                    yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
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

  graph <- ggplot(data = data, aes(x=xorder, y=b, group=a)) + geom_area(aes(fill = ""), show.legend = FALSE) +
    scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + facet_grid(. ~a)

  graph
}


#' gg_area_hor_facet_CaNu.
#' Facet Horizontal Area
#' @name gg_area_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Index",
                                    yLabel = NULL, ...){

  graph <- gg_area_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel)
  graph <- graph + coord_flip()

  graph
}

#' gg_area_stacked_100_ver_CaNu.
#' Stacked Vertical Area 100
#' @name gg_area_stacked_100_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index',
                                          yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
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
  data_graph$xorder <- as.numeric(data_graph$xorder)

  graph <- ggplot(data = data_graph,
                  aes(x=xorder, y=b, group=a)) +
    geom_area(aes(fill = a), position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent)
  graph
}

#' gg_area_stacked_100_hor_CaNu.
#' Stacked Horizontal Area 100
#' @name gg_area_stacked_100_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index',
                                          yLabel = NULL, leg_pos = "right", ...){

  graph <- gg_area_stacked_100_ver_CaNu.(data, titleLabel,subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_area_stacked_ver_CaNu.
#' Stacked Vertical Area
#' @name gg_area_stacked_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index',
                                      yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
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
  data_graph$xorder <- as.numeric(data_graph$xorder)

  graph <- ggplot(data = data_graph,
                  aes(x=xorder, y=b, group=a)) +
    geom_area(aes(fill = a), position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph
}

#' gg_area_stacked_hor_CaNu.
#' Stacked Horizontal Area
#' @name gg_area_stacked_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index',
                                      yLabel = NULL, leg_pos = "right", ...){

  graph <- gg_area_stacked_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_point_grouped_CaNu.
#' Grouped Color Point
#' @name gg_point_grouped_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_grouped_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index',
                                   yLabel = NULL, leg_pos="right", type = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x=xorder, y=b)) + geom_point(aes(color = a), shape = type) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds()

  graph
}

#' gg_line_point_multi_CaNu.
#' Grouped Line Color Point
#' @name gg_line_point_multi_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_multi_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index', yLabel = NULL,
                                      leg_pos="right", type = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
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
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette())

  graph
}

#' gg_line_multi_CaNu.
#' Grouped Line Coloured
#' @name gg_line_multi_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_multi_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index', yLabel = NULL, leg_pos="right", type = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
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
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette())

  graph
}

#' gg_point_trend_line_facet_CaNu.
#' Facet Trend Line
#' @name gg_point_trend_line_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_trend_line_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index',
                                            yLabel = NULL, type = 1, alpha = 0.3, se = FALSE, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x = xorder, y = b)) + geom_point(shape = type, aes(color = ""), show.legend = FALSE) +
    geom_smooth(method=lm, se=se, aes(colour = "*", fill = "*"), alpha = alpha, show.legend = FALSE) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + facet_grid(. ~a)

  graph
}



#' gg_trend_ribbon_facet_CaNu.
#' Facet Trend ribbon
#' @name gg_trend_ribbon_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_trend_ribbon_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = 'Index',
                                        yLabel = NULL, type = 1, alpha = 0.3, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  count <- data_count$count
  count <- unlist(lapply(count, function(i){
    return(1:i)
  }))

  data$xorder <- count

  graph <- ggplot(data, aes(x = xorder, y = b)) + geom_point(aes(color = ""), shape = type, show.legend = FALSE) +
    geom_smooth(aes(colour="*", fill = "*"), alpha = alpha, show.legend = FALSE) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_minimal() + facet_grid(. ~a)

  graph
}

#Width debe de ser un parámetro.  0 < width < 1.

#' gg_donut_CaNu.
#' dount
#' @name gg_donut_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_donut_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", width = 0.3, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4))

  graph <- ggplot(data=data_graph, aes(x = factor(1), fill = a, weight = count)) +
    geom_bar(width = width) + coord_polar(theta = "y") +
    geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
    scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  graph
}




#' gg_dot_bar_ver_CaNu.
#' Vertical Dot Bar
#' @name gg_dot_bar_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_ver_CaNu.<- function(data, titleLabel = "", subtitle = "", caption = "",
                                xLabel = NULL, yLabel = NULL, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(suma = sum(b))

  data_graph <- data_graph %>%
    dplyr::mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = order, fill = factor(a))) + geom_dotplot(method="histodot")

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, subtitle = subtitle, caption = caption)
  graph <- graph + scale_y_continuous(breaks = NULL) +
    theme(legend.position=leg_pos) + theme_ds() +
    scale_fill_manual(values = getPalette())

  graph
}

#' gg_dot_bar_hor_CaNu.
#' Horizontal Dot Bar
#' @name gg_dot_bar_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, leg_pos = "right", ...){

  graph <- gg_dot_bar_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}


#' gg_bullseye_CaNu.
#' Bullseye
#' @name gg_bullseye_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                              leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>% dplyr::arrange(count)
  graph <- ggplot(data = data_graph,
                  aes(x = factor(1), fill = a, y = reorder(a, count))) +
    geom_bar(stat = "identity", width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
    scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  graph
}


#' gg_bar_single_stacked_hor_CaNu.
#' Single Horizontal Stacked Bar
#' @name gg_bar_single_stacked_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_single_stacked_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                            yLabel = "Count", leg_pos="right", width = 0.3, ...){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4))

  graph <- ggplot(data=data_graph, aes(x = factor(1), fill = a, weight = count)) +
    geom_bar(width = width) +
    geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
    scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  graph
}


#' gg_bar_single_stacked_ver_CaNu.
#' Single Vertical Stacked Bar
#' @name gg_bar_single_stacked_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_single_stacked_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                            yLabel = "Count", leg_pos="right", width = 0.3, ...){

  graph <- gg_bar_single_stacked_hor_CaNu.(data, titleLabel, subtitle, caption, yLabel, leg_pos, width)
  graph <- graph + coord_flip()

  graph
}


#' gg_gauge_CaNu.
#' Gauge
#' @name gg_gauge_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_gauge_CaNu. <- function(data, ...){

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
      geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill = getPalette()[1])+
      geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill = getPalette()[2])+
      geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill = getPalette()[3])+
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

#' gg_gauge_dial_CaNu.
#' Gauge
#' @name gg_gauge_dial_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_gauge_dial_CaNu. <- function(data, ...){

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
      geom_polygon(data=get.poly(breaks[1],as.numeric(pos[[1]])),aes(x,y),fill = getPalette()[1]) +
      geom_polygon(data=get.poly(as.numeric(pos[[1]]),breaks[3]),aes(x,y),fill = getPalette()[2]) +
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


#' gg_boxplot_CaNu.
#' Boxplot
#' @name gg_boxplot_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL, leg_pos = 'right', ...){


  f <- fringe(data)
  nms <- getClabels(f)
  data <- f$d

  graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) +
    geom_boxplot(show.legend = FALSE) + theme(legend.position=leg_pos)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)

  graph
}

#' gg_boxplot_flip_CaNu.
#' Boxplot flipped
#' @name gg_boxplot_flip_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_flip_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                  yLabel = NULL, leg_pos = 'right', ...){


  graph <- gg_boxplot_CaNu.(data, titleLabel, subtitle, caption, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_boxplot_dot_CaNu.
#' Boxplot + dot jitter
#' @name gg_boxplot_dot_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_dot_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, leg_pos = 'right', ...){


  f <- fringe(data)
  data <- f$d

  graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) + geom_jitter(color = "#D55E00", show.legend = FALSE) +
    geom_boxplot(show.legend = FALSE) + theme(legend.position=leg_pos)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)

  graph
}

#' gg_boxplot_dot_flip_CaNu.
#' Boxplot + dot jitter flipped
#' @name gg_boxplot_dot_flip_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_dot_flip_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos = 'right', ...){


  graph <- gg_boxplot_dot_CaNu.(data, titleLabel, subtitle, caption, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_violin_mult_CaNu.
#' Violin
#' @name gg_violin_mult_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_violin_mult_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, leg_pos = 'right', ...){

  f <- fringe(data)
  data <- f$d

  graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) +
    geom_violin(show.legend = FALSE) + theme(legend.position=leg_pos)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)

  graph
}

#' gg_violin_mult_flip_CaNu.
#' Violin multi flipped
#' @name gg_violin_mult_flip_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_violin_mult_flip_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos = 'right', ...){

  graph <- gg_violin_mult_CaNu.(data, titleLabel, subtitle, caption, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_violin_dot_mult_CaNu.
#' Violin + dot jitter
#' @name gg_violin_dot_mult_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_violin_dot_mult_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, leg_pos = 'right', ...){
  f <- fringe(data)
  data <- f$d

  graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) +
    geom_jitter(color = "#D55E00", show.legend = FALSE) + geom_violin(show.legend = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)

  graph
}

#' gg_violin_dot_mult_flip_CaNu.
#' Violin + dot jitter flipped
#' @name gg_violin_dot_mult_flip_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_violin_dot_mult_flip_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos = 'right', ...){

  graph <- gg_violin_dot_mult_CaNu.(data, titleLabel, subtitle, caption, yLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)

}

#' gg_bar_ordered_ver_CaNu.
#' Ordered vertical Bars
#' @name gg_bar_ordered_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel =  NULL, leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(x = reorder(a, count), y = count, fill = "")) +
    geom_bar(stat = "identity")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    scale_fill_manual(values = getPalette()) + theme_ds() + guides(fill = FALSE)
  graph <- graph + theme(legend.position=leg_pos)

  graph
}

#' gg_bar_ordered_hor_CaNu.
#' Ordered horizontal Bars
#' @name gg_bar_ordered_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel =  NULL, leg_pos = "right", ...){

  graph <- gg_bar_ordered_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_bar_ver_CaNu.
#' Vertical Bars
#' @name gg_bar_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL, leg_pos = "right", text = TRUE, type = "count",
                             text_size = 5, aggregation = "mean",...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(pos = count - count/10,
                  percent = 100 * round(count / sum(count), 4))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = "")) +
    geom_bar(stat = "identity")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    scale_fill_manual(values = getPalette()) + theme_ds() + guides(fill = FALSE)
  graph <- graph + theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(count,2)),
                             size = text_size, color = "white"))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               size = text_size, color = "white"))
    }else{
      graph
    }
  }

  graph
}

#' gg_bar_hor_CaNu.
#' Horizontal Bars
#' @name gg_bar_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL, leg_pos = "right", text = TRUE, type = "count",
                             text_size = 5,aggregation = "sum",...){

  graph <- gg_bar_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel,
                            leg_pos, text, type, text_size, aggregation)
  graph <- graph + coord_flip()

  graph
}

#' gg_steam_CaNu.
#' Steam
#' @name gg_steam_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                           yLabel =  NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n()) %>%
    tidyr::spread(xorder, b) %>% tidyr::gather(xorder, b, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$xorder <- as.numeric(data_graph$xorder)

  graph <- ggplot(data_graph, aes(x = xorder, y = b, group = a, fill = a)) +
    stat_steamgraph() +
    labs(tittle = titleLabel, x = xLabel, y = ylab) +
    scale_fill_manual(values = getPalette()) + theme_ds()
  graph <- graph + theme(legend.position=leg_pos)

  graph
}

#' gg_treemap_x_CaNu.
#' Treemap fill by first Ca
#' @name gg_treemap_x_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", ...){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a"),
                     group.label.colour = "black") + guides(fill=FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette())

  graph
}

#' gg_treemap_density_y_CaNu.
#' Treemap Density by Nu
#' @name gg_treemap_density_y_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_y_CaNu. <- function(data, titleLabel = "", subtitle = "",
                                       caption = "", reverse = FALSE, ...){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(Sum = sum(b)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'Sum', group = "a"),
                     group.label.colour = "black") + theme(legend.title=element_blank()) +
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



#' gg_bubble_CaNu2.
#' Bubbles
#' @name gg_bubble_CaNu2.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_bubble_CaNu2. <- function(data, titleLabel = "", subtitle = "", caption = "",  sep = 3, lim_inf =-80,
                             lim_sup = 80, xLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% drop_na(b)
  data <- data %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b))
  data$b <- rescale(data$b, to = c(5, 30))
  ncircles <- dim(data)[1]
  limits <- c(lim_inf , lim_sup)
  inset <- diff(limits) / sep

  set.seed(7321)
  xyr <- data.frame(
    x = runif(ncircles, min(limits) + inset, max(limits) - inset),
    y = runif(ncircles, min(limits) + inset, max(limits) - inset),
    r = (data$b)) %>% arrange(desc(r))

  res <- circleLayout(xyr, limits, limits, maxiter = 1000)

  dat.after <- circlePlotData(res$layout)

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(id = 1:n(), categoria = a) %>%
    dplyr::select(id, categoria)

  fi <- data.frame(id = 1:dim(data)[1], categoria = data$a)
  fi <- inner_join(data_graph, dat.after)

  cent <- fi %>% dplyr::group_by(categoria) %>%
    dplyr::summarise(x = mean(x), y = mean(y))


  graph <- ggplot(fi) +
    geom_polygon(aes(x, y, group=id, fill = categoria)) +
    scale_fill_manual(values = getPalette()) +
    coord_equal(xlim=limits, ylim=limits) +
    geom_text(data=cent, aes(x, y, label=categoria)) +
    theme_ds() + theme_ds_clean() +
    labs(title=titleLabel) + guides(fill = FALSE)

  return(graph)

}

#' gg_slope_CaNu.
#' Slope
#' @name gg_slope_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_slope_CaNu. <-  function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                            leg_pos="right", overlap = TRUE, text_size = 6,
                            size_point = 3, size_line = 1,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>% group_by(a) %>% dplyr::mutate(xorder = 1:n())

  graph <- ggplot(data_graph) +
    geom_line(aes(x = as.factor(xorder), y = b, group = a, color = a), size = size_line) +
    geom_point(aes(x = as.factor(xorder), y = b, group = a, color = a), size = size_point)+
    theme_ds_clean() +  labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    geom_text(aes(x = as.factor(xorder), y = min(b) - mean(b), label = xorder),
              size = text_size, show.legend = FALSE, check_overlap = TRUE) +
    annotate("text", x = filter(data_graph,xorder == 1)$xorder-.15, y = filter(data_graph,xorder == 1)$b,
             label = filter(data_graph,xorder == 1)$b, check_overlap = overlap) +
    annotate("text", x = filter(data_graph,xorder == 2)$xorder+.15, y = filter(data_graph,xorder == 2)$b,
             label = filter(data_graph,xorder == 2)$b, check_overlap = overlap)+
    scale_color_manual(values = getPalette()) + theme(legend.position = leg_pos)

  return(graph)

}

