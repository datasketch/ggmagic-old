#' Horizon
#' Horizon
#' @name gg_horizon_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_horizon_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel =  NULL, leg_pos = "right",reverse = FALSE, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% mutate(xorder = 1:nrow(.))

  graph <- ggplot_horizon(data_graph, 'xorder', 'a')
  graph <- graph + theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption =caption, x = xLabel, y = ylab)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph
}

#' Waterfall
#' Waterfall
#' @name gg_waterfall_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel =  NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% mutate(xorder = 1:nrow(.))
  graph <- ggplot_waterfall(data_graph, 'xorder', 'a') +
           scale_color_manual(breaks = c("+","-", ""), values = getPalette()) +
           theme_ds() + theme(legend.position="none") +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)



  graph
}


#' Histogram
#' Histogram
#' @name gg_hist_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "",
                        yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x=a)) + geom_histogram(aes(fill= ""), show.legend = FALSE)

  graph <- graph + geom_vline(aes(xintercept=mean(a), color = ""), linetype="dotted",
                              show.legend = FALSE) +
    scale_color_manual(values = getPalette()[2]) + scale_fill_manual(values = getPalette())


  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
  graph <- graph + theme_ds()

  return(graph)

}

#' Histogram + density
#' Histograms with density
#' @name gg_hist_dens_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_dens_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL,  alfa = 0.5, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x=a)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
            geom_density(alpha=alfa, aes(color = ""), show.legend = FALSE)
  graph <- graph + geom_vline(aes(xintercept=mean(a), color = ""),
                              linetype = "dotted", show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2])

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
  graph <- graph + theme_ds()

  return(graph)

}

#' Cumulative distribution function
#' Cumulative distribution function
#' @name gg_dist_cum_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_cum_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                            yLabel = NULL, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(a)) + geom_step(aes(y=..y.., color = ""), stat="ecdf", show.legend = FALSE) +
    scale_color_manual(values = getPalette())

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds()

  return(graph)

}


#' Vertical line + point
#' Line wiht point plot
#' @name gg_line_point_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                       yLabel = NULL, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  xlab <- xLabel %||% "Index"
  data <- f$d


  data_graph <- data %>%
                dplyr::mutate(order = 1:nrow(data))

  graph <- ggplot(data_graph, aes(x=order, y=a)) + geom_line(aes(color = ""), show.legend = FALSE) +
    geom_point(aes(color = ""), shape = shape_type, show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette())

  graph
}


#' Horizontal line + point
#' Line with point plot
#' @name gg_line_point_flip_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_flip_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                            yLabel = NULL, shape_type = 19, ...){

  graph <- gg_line_point_Nu.(data, titleLabel, subtitle, caption, xLabel, yLabel, shape_type, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical scatter
#' Scatter plot
#' @name gg_point_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                          yLabel = NULL, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  xlab <- xLabel %||% "Index"
  data <- f$d

  data_graph <- data %>%
                dplyr::mutate(order = 1:nrow(data))

  graph <- ggplot(data_graph, aes(x=order, y=a)) + geom_point(shape = shape_type, aes(color = ""))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette())

  graph
}

#' Horizontal scatter
#' Line plot
#' @name gg_point_flip_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_flip_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, shape_type = 19, ...){

  graph <- gg_point_Nu.(data, titleLabel, subtitle, caption, xLabel, yLabel, shape_type, ...)
  graph <- graph + coord_flip()

  graph
}


#' Histogram density
#' density histogram
#' @name gg_density_hist_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_density_hist_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x=a)) + geom_density(aes(fill = ""), show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())

  graph
}

#' Vertical boxplot
#' Box plot
#' @name gg_box_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_box_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", yLabel = NULL,
                       xLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x=factor(""), y=a)) + geom_boxplot(aes(fill = ""), show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())

  graph
}


#' Horizontal boxplot
#' Box plot flipped
#' @name gg_box_flip_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_box_flip_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", yLabel = NULL,
                            xLabel = NULL, ...){

  graph <- gg_box_Nu.(data, titleLabel, subtitle, caption, yLabel, xLabel)
  graph <- graph + coord_flip()

  graph
}

# Gauge media, moda (discutir con JP)

#' Horizontal violin
#' Violin
#' @name gg_violin_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_violin_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", yLabel = NULL,
                          xLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
              dplyr::mutate(order = rep(1, nrow(data)))

  graph <- ggplot(data_graph, aes(factor(""), a)) + geom_violin(aes(fill = ""), show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())

  graph
}

#' Vertical violin
#' Violin flipped
#' @name gg_violin_flip_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_violin_flip_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "", yLabel = NULL,
                                xLabel = NULL, ...){

  graph <- gg_violin_Nu.(data, titleLabel, subtitle, caption, yLabel, xLabel, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical dot bar
#' Dot bar
#' @name gg_dot_bar_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                           xLabel = NULL, yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  xlab <- xLabel %||% "Index"
  data <- f$d

  graph <- ggplot(data, aes(a)) + geom_dotplot(aes(fill = ""), show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())

  graph
}

#' Vertical dot bar
#' Dot bar flipped
#' @name gg_dot_bar_flip_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_flip_Nu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                xLabel = NULL, yLabel = NULL, ...){

  graph <- gg_dot_bar_Nu.(data, titleLabel, subtitle, caption, xLabel, yLabel)
  graph <- graph + coord_flip()

  graph
}
