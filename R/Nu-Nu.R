#' gg_horizon_NuNu.
#' Horizon
#' @name gg_horizon_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_horizon_NuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, reverse = FALSE,
                           yLabel =  NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
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

  return(graph)
}

#' gg_waterfall_NuNu.
#' Waterfall
#' @name gg_waterfall_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_NuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot_waterfall(data,'a','b')  +
    scale_color_manual(breaks = c("+", "-", ""), values = c("#E5007D", "#009EE3", "black")) +
    theme_ds()  + theme(legend.position="none")+
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  return(graph)
}


#' gg_dens2D_NuNu.
#' Waterfall
#' @name gg_dens2D_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dens2D_NuNu. <- function(data, title = "", subtitle = "", caption = "", xLabel=NULL,
                        yLabel=NULL, labelText = "",reverse = FALSE, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x=a, y=b)) + geom_point() +
           stat_density2d(geom = "tile", aes(fill = ..density..), contour = F) +
           theme_ds_clean() +
           labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel, fill = labelText)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }
  return(graph)

}


#' gg_flip_dens2D_NuNu.
#' Waterfall
#' @name gg_flip_dens2D_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_flip_dens2D_NuNu. <- function(data, title = "", subtitle = "", caption = "", xLabel="", yLabel="", labelText = "", ...){

  graph <- gg_dens2D_NuNu.(data, title, subtitle, caption, xLabel, yLabel, labelText)
  graph <- graph + coord_flip()

  return(graph)
}


#' gg_hist2D_NuNu.
#' Waterfall
#' @name gg_hist2D_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist2D_NuNu. <- function(data, title = "", subtitle = "", caption = "",
                            xLabel="", yLabel="", labelText = "", reverse = FALSE, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  binNumber <- floor(sqrt(nrow(data)))
  graph <- ggplot(data, aes(x=a, y=b)) + stat_bin2d(bins=binNumber) +
           theme_ds() +
           labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel, fill = labelText)

   if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }
  return(graph)
}

#' gg_flip_hist2D_NuNu.
#' Waterfall
#' @name gg_flip_hist2D_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_flip_hist2D_NuNu. <- function(data, title = "", subtitle = "", caption = "",
                             xLabel="", yLabel="", labelText = "", reverse = FALSE, ...){
  graph <- gg_hist2D_NuNu.(data, title, subtitle, caption, xLabel, yLabel, labelText, reverse)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_mult_line_NuNu.
#' Waterfall
#' @name gg_mult_line_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_mult_line_NuNu. <- function(data, title = "", subtitle = "", caption = "", xLabel=NULL,
                           yLabel=NULL, labelText = "", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  label <- names(data)
  data$idx <- seq(nrow(data))
  variables <- labels
  df <- melt(data, id.vars = "idx")

  graph <- ggplot(df, aes(x = idx, y = value, colour = variable)) +
           geom_line() +
           theme_ds() +
           scale_color_manual(values = getPalette())
  graph <- graph  + labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel, fill = labelText)

  return(graph)

}


#' gg_scatter_NuNu.
#' Scatterplot
#' @name gg_scatter_NuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_NuNu. <- function(data, title = "", subtitle = "", caption = "", xLabel=NULL,
                               yLabel=NULL, labelText = "", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

graph <- ggplot(data = data, aes(x = a, y = b)) +
         geom_point(shape=1,color = '#009EE3') + theme_ds() +
         labs(title = title, subtitle = subtitle, caption = caption, y = yLabel, x = xLabel)
return(graph)

}



