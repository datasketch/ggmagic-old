#' gg_scatter_CaNuNu.: title.
#' pointlines
#' @name gg_scatter_CaNuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_scatter_CaNuNu. <- function(data,title = "", subtitle = "", caption = "",
                               xLabel = NULL,
                               yLabel=NULL, cLabel = NULL, angle = 45,
                               aggregation = "mean",
                               pointLabels = TRUE,
                               ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xLabel <- xLabel %||% nms[2]
  yLabel <- yLabel %||% nms[3]
  cLabel <- cLabel %||% nms[1]
  data <- f$d

  data <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b=agg(aggregation,b),c=agg(aggregation,c))

  graph <- ggplot(data, aes(x = b, y = c, colour = a, label = a)) +
    geom_point()
  if(pointLabels)
    graph <- graph + geom_text(show.legend = FALSE,hjust=-0.3, vjust=-0.5)
  graph <- graph + scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = title, subtitle = subtitle, caption = caption,
                      x= xLabel, y = yLabel, colour = cLabel) +
    theme(axis.text.x = element_text(angle = angle, hjust = 1))
  graph
}


#' gg_scatter_trend_CaNuNu.: title.
#' pointlines
#' @name gg_scatter_trend_CaNuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_trend_CaNuNu. <- function(data,title = "", subtitle = "", caption = "",
                                     xLabel = NULL,
                                     yLabel=NULL, cLabel = NULL, angle = 45,
                                     aggregation = "mean",
                                     pointLabels = FALSE,
                                     vjust = 0, hjust = 0, se = FALSE,
                                     ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xLabel <- xLabel %||% nms[2]
  yLabel <- yLabel %||% nms[3]
  cLabel <- cLabel %||% nms[1]
  data <- f$d

  data <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b=agg(aggregation,b),c=agg(aggregation,c))

  formula <- y ~ poly(x, 1, raw = TRUE)

  graph <- ggplot(data, aes(x = b, y = c,label = a)) +
    geom_point()
  graph <- graph + geom_smooth(method = "lm",
                               formula = formula, color = getPalette()[1],
                               se = se) +
    stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                 vjust = vjust,
                 hjust = hjust,
                 formula = formula, parse = TRUE)
  if(pointLabels)
    graph <- graph + geom_text(show.legend = FALSE,hjust=-0.3, vjust=-0.5)
  graph <- graph +
    #scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = title, subtitle = subtitle, caption = caption,
                      x= xLabel, y = yLabel, colour = cLabel) +
    theme(axis.text.x = element_text(angle = angle, hjust = 1))
  graph
}




#' gg_steam_CaNuNu.
#' Steamgraph
#' @name gg_steam_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CaNuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    tidyr::spread(b, c) %>% tidyr::gather(b, c, -a)

  data_graph[is.na(data_graph)] <- 0
  data_graph$b <- as.numeric(data_graph$b)
  data_graph$c <- as.numeric(data_graph$c)

  graph <- ggplot(data_graph, aes(x = b, y = c, group = a, fill = a)) +
    stat_steamgraph() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position=leg_pos) + theme_ds() +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  graph
}

#' gg_line_CaNuNu.
#' lines
#' @name gg_line_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_CaNuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                            yLabel = NULL, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data$a <- as.factor(data$a)
  graph <- ggplot(data) +  geom_line(aes(x = b, y = c, group = a, colour = a))  +
    theme(legend.position = leg_pos) +
    scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  return(graph)

}

#' gg_point_line_CaNuNu.
#' Point Lines
#' @name gg_point_line_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_line_CaNuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                  yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data$a <- as.factor(data$a)
  graph <- ggplot(data) +  geom_line(aes(x = b, y = c, group = a, colour = a))  +
    geom_point(aes(x = b, y = c, group = a, colour = a)) +
    theme(legend.position = leg_pos) +
    scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  return(graph)

}

#' gg_point_CaNuNu.
#' point
#' @name gg_point_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_CaNuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL, leg_pos="right",
                             shape_point = 1, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data) + theme(legend.position=leg_pos) +
    geom_point(aes(x = b, y = c, colour = a), shape = shape_point) +
    scale_color_manual(values = getPalette()) + theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  graph
}

#' gg_circle_CaNuNu.
#' Point
#' @name gg_circle_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
#'
gg_circle_CaNuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                              yLabel = NULL, leg_pos="right", size = 10, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data, aes(x = b , y = c, fill = a, color  = a))  +
    geom_point(size = size) +
    scale_color_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  return(graph)
}

