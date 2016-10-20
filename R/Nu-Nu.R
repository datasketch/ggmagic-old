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
gg_horizon_NuNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                           yLabel =  NULL, leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot_horizon(data, 'a', 'b')
  graph <- graph + scale_fill_continuous(low = 'green', high = 'red') + theme_minimal() +
    labs(tittle = titleLabel, x = xlab, y = ylab)

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
gg_waterfall_NuNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                             yLabel =  NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot_waterfall(data,'a','b') + theme_minimal() +
    labs(tittle = titleLabel, x = xlab, y = ylab)

  return(graph)
}


dens2D_Plot <- function(data, titl="", xLabel="", yLabel="", labelText = ""){

  graph <- ggplot(data, aes(x=a, y=b)) + geom_point() +
    stat_density2d(geom = "tile", aes(fill = ..density..), contour = F) +
    theme_bw() + labs(title = titl, x = xLabel, y = yLabel, fill = labelText)

  return(graph)

}

flip_dens2D_Plot <- function(data, titl="", xLabel="", yLabel="", labelText = ""){

  graph <- dens2D_Plot(data, titl, xLabel, yLabel, labelText)
  graph <- graph + coord_flip()

  return(graph)
}



hist2D_Plot <- function(data, titl="", xLabel="", yLabel="", labelText = ""){

  binNumber <- floor(sqrt(nrow(data)))
  graph <- ggplot(data, aes(x=a, y=b)) + stat_bin2d(bins=binNumber) +
    scale_fill_gradient(low="aliceblue", high="coral2") +
    theme_bw() + labs(title = titl, x = xLabel, y = yLabel, fill = labelText)

  return(graph)
}

flip_hist2D_Plot <- function(data, titl="", xLabel="", yLabel="", labelText = ""){
  graph <- hist2D_Plot(data, titl, xLabel, yLabel, labelText)
  graph <- graph + coord_flip()

  return(graph)
}


mult_Line_Plot <- function(data, titl="", xLabel="", yLabel="", labelText = ""){

  label <- names(data)
  dataNumNum$idx <- seq(nrow(data))
  variables <- labels
  df <- melt(dataNumNum, id.vars = "idx")
  graph <- ggplot(df, aes(x = idx, y = value, colour = variable)) +
    geom_line() + labs(title = titl, x = xLabel, y = yLabel, fill = labelText) + theme_bw()

  return(graph)

}
