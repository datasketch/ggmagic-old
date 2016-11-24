#' gg_horizon_Nu.
#' Horizon
#' @name gg_horizon_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_horizon_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel =  NULL, leg_pos = "right",reverse = FALSE, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% mutate(xorder = 1:nrow(.))

  graph <- ggplot_horizon(data_graph, 'xorder', 'a')
  graph <- graph + theme_ds() +
    labs(tittle = title, subtitle = subtitle, caption =caption, x = xLabel, y = ylab)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  return(graph)
}

#' gg_waterfall_Nu.
#' Waterfall
#' @name gg_waterfall_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel =  NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% mutate(xorder = 1:nrow(.))
  graph <- ggplot_waterfall(data_graph, 'xorder', 'a') +
           scale_color_manual(breaks = c("+","-", ""), values = c("#009EE3", "#E5007D", "black")) +
           theme_ds() + theme(legend.position="none") +
           labs(tittle = title, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)



  return(graph)
}


#' gg_hist_Nu.
#' Histograms
#' @name gg_hist_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = "",
                        yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x=a)) + geom_histogram(fill= "#009EE3", color = "white")

  graph <- graph + geom_vline(aes(xintercept=mean(a)),
                              color="#E5007D", linetype="dashed", size=1)

  graph <- graph + labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
  graph <- graph + theme_ds()

  return(graph)

}

#' gg_hist_dens_Nu.
#' Histograms with density
#' @name gg_hist_dens_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_dens_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL,  alfa = 0.5, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x=a)) + geom_histogram(aes(y=..density..), fill = "#009EE3", color = "white") +
            geom_density(alpha=alfa, col="#E5007D")
  graph <- graph + geom_vline(aes(xintercept=mean(a)),
                              color="#E5007D", linetype="dashed", size=1)

  graph <- graph + labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
  graph <- graph + theme_ds()

  return(graph)

}

#' gg_cumm_dist_Nu.
#' Cumulative distribution function
#' @name gg_cumm_dist_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_cumm_dist_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                            yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(a)) + geom_step(aes(y=..y..),stat="ecdf", color = "#009EE3")

  graph <- graph + labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
  graph <- graph + theme_ds()

  return(graph)

}

#' gg_flip_cumm_dist_Nu.
#' Cumulative distribution function
#' @name gg_flip_cumm_dist_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_flip_cumm_dist_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, ...){

  graph <- gg_cumm_dist_Nu.(data, title, subtitle, caption, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_line_Nu.
#' Line plot
#' @name gg_line_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                       yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d


  data_graph <- data %>%
                dplyr::mutate(order = 1:nrow(data))

  graph <- ggplot(data_graph, aes(x=order, y=a)) + geom_line(color = "#009EE3") + geom_point(color = "#E5007D")
  graph <- graph + labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
  graph <- graph + theme_ds()

  return(graph)
}


#' gg_flip_line_Nu.
#' Line plot
#' @name gg_flip_line_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_flip_line_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                            yLabel = NULL, ...){

  graph <- gg_line_Nu.(data, title, subtitle, caption, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_scatter_Nu.
#' Line plot
#' @name gg_scatter_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                          yLabel = NULL, type = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
                dplyr::mutate(order = 1:nrow(data))

  graph <- ggplot(data_graph, aes(x=order, y=a)) + geom_point(shape = type, color = "#009EE3")
  graph <- graph + labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
  graph <- graph + theme_ds()

  return(graph)
}

#' gg_flip_scatter_Nu.
#' Line plot
#' @name gg_flip_scatter_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_flip_scatter_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, type = 0, ...){

  graph <- gg_scatter_Nu.(data, title, subtitle, caption, xLabel, yLabel, type)
  graph <- graph + coord_flip()

  return(graph)
}


#' gg_density_hist_Nu.
#' density histogram
#' @name gg_density_hist_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_density_hist_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x=a)) + geom_density(fill = "#009EE3")
  graph <- graph + labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
  graph <- graph + theme_ds()

  return(graph)
}

#' gg_box_Nu.
#' Box plot
#' @name gg_box_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_box_Nu. <- function(data, title = "", subtitle = "", caption = "", yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x=factor(""), y=a)) + geom_boxplot(color = "#009EE3")
  graph <- graph + labs(title = title, subtitle = subtitle, caption = caption, x = "", y = yLabel)
  graph <- graph + theme_ds()

  return(graph)
}


#' gg_flip_box_Nu.
#' Box plot
#' @name gg_flip_box_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_flip_box_Nu. <- function(data, title = "", subtitle = "", caption = "",
                         yLabel = NULL, ...){

  graph <- gg_box_Nu.(data, title, subtitle, caption, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

# Gauge media, moda (discutir con JP)

#' gg_violin_Nu.
#' Violin
#' @name gg_violin_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_violin_Nu. <- function(data, title = "", subtitle = "", caption = "", yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
              dplyr::mutate(order = rep(1, nrow(data)))

  graph <- ggplot(data_graph, aes(factor(order), a)) + geom_violin(color = "#009EE3")
  graph <- graph + labs(title = title, subtitle = subtitle, caption = caption, x = "", y = yLabel)
  graph <- graph + theme_ds()

  return(graph)
}

#' gg_flip_violin_Nu.
#' Violin
#' @name gg_flip_violin_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_flip_violin_Nu. <- function(data, title = "", subtitle = "", caption = "", yLabel = NULL, ...){

  graph <- gg_violin_Nu.(data, title, subtitle, caption, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_dot_Nu.
#' Violin
#' @name gg_dot_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(a)) + geom_dotplot(fill = "#009EE3")
  graph <- graph + labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
  graph <- graph + theme_ds()

  return(graph)
}

#' gg_flip_dot_Nu.
#' Violin
#' @name gg_flip_dot_Nu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_flip_dot_Nu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                           yLabel = NULL, ...){

  graph <- gg_dot_Nu.(data, title, subtitle, caption, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}
