#' gg_horizon_NuNu.
#' Horizon
#' @name gg_horizon_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_horizon_NuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, reverse = FALSE,
                           yLabel =  NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot_horizon(data, 'a', 'b') +
    labs(title = titleLabel, subtitle, caption, x = xlab, y = ylab) + theme_ds()

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph
}

#' gg_waterfall_NuNu.
#' Waterfall
#' @name gg_waterfall_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_NuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot_waterfall(data,'a','b')  +
    scale_color_manual(breaks = c("+", "-", ""), values = getPalette()) +
    theme_ds()  + theme(legend.position="none")+
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  graph
}


#' gg_dens_NuNu.
#' Density in 2D
#' @name gg_dens_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dens_NuNu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                            yLabel=NULL, reverse = FALSE, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b)) +
           stat_density2d(geom = "tile", aes(fill = ..density..), contour = FALSE) +
           theme_ds() +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }
  return(graph)

}


#' gg_dens_flip_NuNu.
#' Density 2D flipped
#' @name gg_dens_flip_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dens_flip_NuNu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel=NULL, reverse = FALSE, ...){

  graph <- gg_dens_NuNu.(data, title, subtitle, caption, xLabel, yLabel, reverse)
  graph <- graph + coord_flip()

  graph
}


#' gg_hist_NuNu.
#' Histogram 2D
#' @name gg_hist_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_NuNu. <- function(data, title = "", subtitle = "", caption = "",
                            xLabel = NULL, yLabel = NULL, reverse = FALSE, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  binNumber <- floor(sqrt(nrow(data)))
  graph <- ggplot(data, aes(x = a, y = b)) + stat_bin2d(bins = binNumber) +
           theme_ds() +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }
  graph
}

#' gg_hist_flip_NuNu.
#' Histogram 2d flipped
#' @name gg_hist_flip_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_flip_NuNu. <- function(data, title = "", subtitle = "", caption = "",
                               xLabel = NULL, yLabel = NULL, reverse = FALSE, ...){

  graph <- gg_hist_NuNu.(data, title, subtitle, caption, xLabel, yLabel, reverse)
  graph <- graph + coord_flip()

  graph
}

#' gg_line_multi_NuNu.
#' Line for each Nu
#' @name gg_line_multi_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_multi_NuNu. <- function(data, title = "", subtitle = "", caption = "",
                                xLabel = NULL, yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% dplyr::mutate(xorder = 1:nrow(.)) %>%
    tidyr::gather(type, value, -xorder)

  graph <- ggplot(data_graph, aes(x = xorder, y = value, group = type, colour = type)) +
           geom_line() + theme_ds() +
           scale_color_manual(values = getPalette())
  graph <- graph  + labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  return(graph)

}


#' gg_point_NuNu.
#' Scatter plot
#' @name gg_point_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_NuNu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, shape = 1,...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

graph <- ggplot(data = data, aes(x = a, y = b)) +
         geom_point(shape = shape, aes(color = ''), show.legend = FALSE) + theme_ds() +
         labs(title = title, subtitle = subtitle, caption = caption, y = ylab, x = xlab) +
  scale_color_manual(values = getPalette())
return(graph)

}

#' gg_line_point_NuNu.
#' Line point plot
#' @name gg_line_point_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_NuNu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                           yLabel = NULL, shape = 1, size = 3,...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data = data, aes(x = a, y = b)) +
    geom_point(shape = shape, aes(color = ''), show.legend = FALSE, size = size) +
    geom_line(aes(color = ''), show.legend = FALSE, size = size) + theme_ds() +
    labs(title = title, subtitle = subtitle, caption = caption, y = ylab, x = xlab) +
    scale_color_manual(values = getPalette())
  return(graph)

}


