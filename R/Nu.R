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
gg_horizon_Nu. <- function(data, titleLabel = "Report", xLabel = "Index",
                                   yLabel =  NULL, leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% mutate(xorder = 1:nrow(.))

  graph <- ggplot_horizon(data_graph, 'xorder', 'a')
  graph <- graph + scale_fill_continuous(low = 'green', high = 'red') + theme_minimal() +
    labs(tittle = titleLabel, x = xLabel, y = ylab)

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
gg_waterfall_Nu. <- function(data, titleLabel = "Report", xLabel = "Index",
                               yLabel =  NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% mutate(xorder = 1:nrow(.))
  graph <- ggplot_waterfall(data_graph, 'xorder', 'a') + theme_minimal() +
    labs(tittle = titleLabel, x = xLabel, y = ylab)

  return(graph)
}

hist_graph <- function(data, titleLabel = "Report", xLabel = "Values", yLabel = "Frequency"){

  graph <- ggplot(data, aes(x=a)) + geom_histogram()

  graph <- graph + geom_vline(aes(xintercept=mean(a)),
                              color="red", linetype="dashed", size=1)

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)
  graph <- graph + theme_minimal()

  return(graph)

}

hist_dens_graph <- function(data, titleLabel = "Report", xLabel = "Values", yLabel = "Frequency",
                            alfa = 0.5){

  graph <- ggplot(data, aes(x=a)) + geom_histogram(aes(y=..density..)) +
            geom_density(alpha=alfa, col="red")
  graph <- graph + geom_vline(aes(xintercept=mean(a)),
                              color="red", linetype="dashed", size=1)

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)
  graph <- graph + theme_minimal()

  return(graph)

}

cumm_prob_graph <- function(data, titleLabel = "Report", xLabel = "Values",
                            yLabel = "Cummulative Probability"){

  graph <- ggplot(data, aes(a)) + geom_step(aes(y=..y..),stat="ecdf")

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)
  graph <- graph + theme_minimal()

  return(graph)

}

flip_cumm_prob_graph <- function(data, titleLabel = "Report", xLabel = "Values",
                                 yLabel = "Cummulative Probability"){

  graph <- cumm_prob_graph(data, titleLabel, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}



line_graph <- function(data, titleLabel = "Report", xLabel = "Observations",
                       yLabel = "Values"){

  data_graph <- data %>%
                dplyr::mutate(order = 1:nrow(data))

  graph <- ggplot(data_graph, aes(x=order, y=a)) + geom_line() + geom_point()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)
  graph <- graph + theme_minimal()

  return(graph)
}

flip_line_graph <- function(data, titleLabel = "Report", xLabel = "Obsevations",
                            yLabel = "Values"){

  graph <- line_graph(data, titleLabel, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

scatter_graph <- function(data, titleLabel = "Report", xLabel = "Observations",
                          yLabel = "Values", type = 0){

  data_graph <- data %>%
                dplyr::mutate(order = 1:nrow(data))

  graph <- ggplot(data_graph, aes(x=order, y=a)) + geom_point(shape = type)
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)
  graph <- graph + theme_minimal()

  return(graph)
}

flip_scatter_graph <- function(data, titleLabel = "Report", xLabel = "Observations",
                               yLabel = "Values", type = 0){

  graph <- scatter_graph(data, titleLabel, xLabel, yLabel, type)
  graph <- graph + coord_flip()

  return(graph)
}

density_hist_graph <- function(data, titleLabel = "Report", xLabel = "Observations",
                               yLabel = "Values"){

  graph <- ggplot(data, aes(x=a)) + geom_density()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)
  graph <- graph + theme_minimal()

  return(graph)
}

box_graph <- function(data, titleLabel = "Report", yLabel = "Values"){

  graph <- ggplot(data,aes(x=factor(""),y=a))+geom_boxplot()+ xlab("")
  graph <- graph + labs(title = titleLabel, x = "", y = yLabel)
  graph <- graph + theme_minimal()

  return(graph)
}

flip_box_graph <- function(data, titleLabel = "Report",
                         yLabel = "Values"){

  graph <- box_graph(data, titleLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

# Gauge media, moda (discutir con JP)


violin_graph <- function(data, titleLabel = "Report", yLabel = "Values"){

  data_graph <- data %>%
              dplyr::mutate(order = rep(1, nrow(data)))

  graph <- ggplot(data_graph, aes(factor(order), a)) + geom_violin()
  graph <- graph + labs(title = titleLabel, x = "", y = yLabel)
  graph <- graph + theme_minimal()

  return(graph)
}

flip_violin_graph <- function(data, titleLabel = "Report", yLabel = "Values"){

  graph <- violin_graph(data, titleLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

dot_graph <- function(data, titleLabel = "Report", xLabel = "Values", yLabel = "Frequency"){

  graph <- ggplot(data, aes(a)) + geom_dotplot()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)
  graph <- graph + theme_minimal()

  return(graph)
}

flip_dot_graph <- function(data, titleLabel = "Report", xLabel = "Values",
                           yLabel = "Frequency"){

  graph <- dot_graph(data, titleLabel, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}
