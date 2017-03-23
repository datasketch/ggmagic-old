#' Scatter + Aggregation on both numeric variables
#' pointlines
#' @name gg_scatter_agg_CaNuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_agg_CaNuNu. <- function(data,titleLabel = "", subtitle = "", caption = "",
                               xLabel = NULL,
                               yLabel=NULL, cLabel = NULL, angle_x = 0,
                               aggregation = "sum", shape_type = 19,
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
    geom_point(shape = shape_type)
  graph <- graph + scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption,
                      x= xLabel, y = yLabel, colour = cLabel) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}


#' Scatter + Aggregation on both numeric variables + trend
#' pointlines
#' @name gg_scatter_agg_trend_CaNuNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_agg_trend_CaNuNu. <- function(data,titleLabel = "", subtitle = "", caption = "",
                                     xLabel = NULL,
                                     yLabel=NULL, cLabel = NULL, angle_x = 0,
                                     aggregation = "sum",
                                     se = FALSE, shape_type = 19,
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

  graph <- ggplot(data, aes(x = b, y = c)) +
    geom_point(aes(color = a), shape = shape_type) + geom_text(aes(label = a, color = a), vjust = 1.2)
  graph <- graph + geom_smooth(method = "lm",
                               formula = formula, color = getPalette()[1],
                               se = se) +
    stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                 vjust = 1,
                 formula = formula, parse = TRUE)

  graph <- graph +
    scale_color_manual(values = getPalette()) + guides(color = FALSE) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption,
                      x= xLabel, y = yLabel, colour = cLabel) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}

#' Steam
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
                             yLabel = NULL, leg_pos="right", angle_x = 0, aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c) %>% tidyr::gather(b, c, -a)

  data_graph[is.na(data_graph)] <- 0
  data_graph$b <- as.numeric(data_graph$b)
  data_graph$c <- as.numeric(data_graph$c)

  graph <- ggplot(data_graph, aes(x = b, y = c, group = a, fill = a)) +
    stat_steamgraph() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos) + theme_ds() +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  graph
}

#' Line
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
                            yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data$a <- as.factor(data$a)
  graph <- ggplot(data) +  geom_line(aes(x = b, y = c, group = a, colour = a))  +
    theme(legend.position = leg_pos) +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  return(graph)

}

#' Line + point
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
                                  yLabel = NULL, leg_pos = "right", shape_type = 19, angle_x = 0,  ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data$a <- as.factor(data$a)
  graph <- ggplot(data) +  geom_line(aes(x = b, y = c, group = a, colour = a))  +
    geom_point(aes(x = b, y = c, group = a, colour = a), shape = shape_type) +
    theme(legend.position = leg_pos) +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  return(graph)
}

#' Scatter
#' Point
#' @name gg_scatter_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_CaNuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                              yLabel = NULL, leg_pos="right", shape_type = 19, angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data, aes(x = b , y = c, fill = a, color  = a))  +
    geom_point(shape = shape_type) +
    scale_color_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  return(graph)
}

#' Scatter + Trend
#' Point
#' @name gg_scatter_trend_CaNuNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_trend_CaNuNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, leg_pos="right", shape_type = 19, angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data, aes(x = b , y = c, fill = a, color  = a))  +
    geom_point(shape = shape_type)

  formula <- y ~ poly(x, 1, raw = TRUE)

  graph <- graph + geom_smooth(method = "lm",
                               formula = formula,
                               se = FALSE, aes(color = a))

  graph <- graph +
    scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption,
                      x= xLabel, y = yLabel) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  return(graph)
}

