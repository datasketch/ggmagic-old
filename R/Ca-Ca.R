

#' gg_bubble_CaCa.
#' Bubble
#' @name gg_bubble_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CaCa.  <- function(data, titleLabel = "Report", xLabel = NULL,
                             yLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(Count = n()) %>%
    dplyr::arrange(desc(Count))
  graph <- ggplot(data_graph, aes(x = a, y = b, size = Count))
  graph <- graph + geom_point()
  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab)
  graph <- graph + theme_minimal() + theme(legend.position="none")

  return(graph)
}

#' gg_coloured_bubble_CaCa.
#' Coloured Bubble
#' @name gg_coloured_bubble_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_coloured_bubble_CaCa.  <- function(data, titleLabel = "Report", xLabel = NULL,
                                      yLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(Count = n()) %>%
    dplyr::arrange(desc(Count))
  graph <- ggplot(data_graph, aes(x = a, y = b, size = Count))
  graph <- graph + geom_point(aes(color = a))
  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab)
  graph <- graph + theme_minimal() + theme(legend.position="none")

  return(graph)
}

#' gg_facet_dot_bar_ver_CaCa.
#' Facet Vertical Dot Bar
#' @name gg_facet_dot_bar_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_bar_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  data_graph <- data_graph %>%
    mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = order, fill = factor(a))) + geom_dotplot(method="histodot")

  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab,  fill = flabel)
  graph <- graph + theme_minimal() + scale_y_continuous(breaks = NULL) +
    theme(legend.position=leg_pos) + facet_grid(. ~b)


  return(graph)
}

#' gg_facet_dot_bar_hor_CaCa.
#' Facet Horizontal Dot Bar
#' @name gg_facet_dot_bar_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_dot_bar_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right"){

  graph <- gg_facet_dot_bar_ver_CaCa.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_pie_CaCa.
#' Facet Pie
#' @name gg_facet_pie_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_pie_CaCa. <- function(data, titleLabel = "Report", fillLabel = NULL,
                               leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(.~b)

  return(graph)
}

#Width debe de ser un parámetro.  0 < width < 1.
#' gg_facet_donut_CaCa.
#' Facet Donut
#' @name gg_facet_donut_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_donut_CaCa. <- function(data, titleLabel = "Report", fillLabel = NULL,
                               width = 0.3, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data=data, aes(x = factor(1), fill = factor(a))) +
    geom_bar(width = width) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(.~b)

  return(graph)
}

#' gg_facet_bullseye_CaCa.
#' Facet Bullseye
#' @name gg_facet_bullseye_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_bullseye_CaCa. <- function(data, titleLabel = "Report", fillLabel = NULL,
                            leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(.~b)

  return(graph)
}

#' gg_bar_facet_coloured_x_ver_CaCa.
#' Facet Vertical coloured bars
#' @name gg_bar_facet_coloured_x_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_x_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                    yLabel = 'Count', fillLabel = NULL,
                                    leg_pos = "right"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, fill = factor(a))) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel,  fill = flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos) + facet_grid(.~b)
  return(graph)
}

#' gg_bar_facet_coloured_x_hor_CaCa.
#' Facet Horizontal coloured Bars
#' @name gg_bar_facet_coloured_x_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_x_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                    yLabel = 'Count', fillLabel = NULL,
                                    leg_pos = "right"){

  graph <- gg_bar_facet_coloured_x_ver_CaCa.(data, titleLabel, xLabel,
                                   yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()
  return(graph)
}

#' gg_bar_facet_coloured_y_ver_CaCa.
#' Facet Vertical coloured bars
#' @name gg_bar_facet_coloured_y_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_y_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                              yLabel = 'Count', fillLabel = NULL,
                                              leg_pos = "right"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, fill = factor(b))) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel,  fill = flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos) + facet_grid(.~b)
  return(graph)
}

#' gg_bar_facet_coloured_y_hor_CaCa.
#' Facet Horizontal coloured Bars
#' @name gg_bar_facet_coloured_y_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_y_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                              yLabel = 'Count', fillLabel = NULL,
                                              leg_pos = "right"){

  graph <- gg_bar_facet_coloured_y_ver_CaCa.(data, titleLabel, xLabel,
                                             yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()
  return(graph)
}

#' gg_bar_facet_coloured_parameter_ver_ver_CaCa.
#' Facet Vertical coloured by parameter bars
#' @name gg_bar_facet_coloured_parameter_ver_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_parameter_ver_ver_CaCa. <- function(data, titleLabel = "Report",
                                                      xLabel = NULL, yLabel = 'Count',
                                                      parameter1 = NULL, parameter2 = NULL,
                                                      leg_pos = "right"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  p_a <-  parameter1 %||% sample(unique(data[,nms[1]]), length(unique(data[,nms[2]])))
  p_b <-  parameter2 %||% sample(unique(data[,nms[2]]), length(unique(data[,nms[2]])))
  data <- f$d
  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = n())

  list_df <- apply(cbind(p_a, p_b), 1, function(x){
    df <- data_graph%>% mutate(color = ifelse(a==x[1] & b==x[2], TRUE, FALSE))
    df[df$color,]
  })
  df <- bind_rows(list_df)
  data_graph <- left_join(data_graph, df, by = c("a", "b", "count"))
  data_graph[is.na(data_graph)] <- FALSE

  graph <- ggplot(data_graph, aes(a, weight = count)) +
    geom_bar(position ="dodge", aes(fill =  color == TRUE))
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel)
  graph <- graph + guides(fill=FALSE)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos) + facet_grid(.~b)
  return(graph)
}

#' gg_bar_facet_coloured_parameter_ver_hor_CaCa.
#' Facet Horizontal coloured by parameter Bars
#' @name gg_bar_facet_coloured_parameter_ver_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_parameter_ver_hor_CaCa. <- function(data, titleLabel = "Report",
                                                      xLabel = NULL, yLabel = 'Count',
                                                      parameter1 = NULL, parameter2 = NULL,
                                                      leg_pos = "right"){

  graph <- gg_bar_facet_coloured_parameter_ver_ver_CaCa.(data, titleLabel, xLabel,
                                             yLabel, parameter1, parameter2, leg_pos)

  graph <- graph + coord_flip()
  return(graph)
}

#' gg_bar_stacked_ver_CaCa.
#' Stacked vertical Bar
#' @name gg_bar_stacked_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                yLabel = 'Count', fillLabel = NULL, leg_pos = "top"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(a, fill=b)) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill=flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_bar_stacked_hor_CaCa.
#' Stacked horizontal Bar
#' @name gg_bar_stacked_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                  yLabel = 'Cpunt', fillLabel = NULL, leg_pos = "top"){

  graph <- gg_bar_stacked_ver_CaCa.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_bar_ordered_stacked_hor_CaCa.
#' Ordered Stacked horizontal Bar
#' @name gg_bar_ordered_stacked_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_stacked_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                      yLabel =  'Count', fillLabel = NULL,
                                      leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x=reorder(data$b, rep(1, length(data$b)), sum), fill = a)) +
            geom_bar()

  graph <- graph + labs(title = titleLabel, x = yLabel, y = xLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_bar_ordered_stacked_ver_CaCa.
#' Ordered Stacked Vertical Bar
#' @name gg_bar_ordered_stacked_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_stacked_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                            yLabel =  'Count', fillLabel = NULL,
                                            leg_pos = "right"){

  graph <- gg_bar_ordered_stacked_hor_CaCa(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_stacked_dot_bar_hor_CaCa.
#' Stacked horizontal dot Bar
#' @name gg_stacked_dot_bar_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_dot_bar_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                         yLabel = 'Count', fillLabel = NULL,
                                         leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data = data, aes(a, fill = factor(b))) +
                  geom_dotplot(stackgroups = TRUE, binpositions = "all")

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + scale_y_continuous(breaks = NULL) +
            theme(legend.position=leg_pos)

  return(graph)
}

#' gg_stacked_dot_bar_ver_CaCa.
#' Stacked vertical dot Bar
#' @name gg_stacked_dot_bar_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_dot_bar_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                         yLabel = 'Count', fillLabel = NULL,
                                         leg_pos = "right"){

  graph <- gg_stacked_dot_bar_hor_CaCa.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_bar_unstacked_coloured_hor_CaCa.
#' Unstacked Coloured horizontal Bar
#' @name gg_bar_unstacked_coloured_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_unstacked_coloured_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                          yLabel = "Count", fillLabel = NULL,
                                          leg_pos = "top"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=n()) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)

  graph <- ggplot(data_graph, aes(a, weight=count, fill=b)) + geom_bar(position = "dodge")
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_bar_unstacked_coloured_ver_CaCa.
#' Unstacked Coloured vertical Bar
#' @name gg_bar_unstacked_coloured_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_unstacked_coloured_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                        yLabel = "Count", fillLabel = NULL,
                                        leg_pos = "top"){
  graph <- gg_bar_unstacked_coloured_hor_CaCa.(data, titleLabel, xLabel, yLabel,
                                         fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_line_hor_CaCa.
#' Horizontal Line
#' @name gg_facet_line_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_line_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                 yLabel = "Count"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=b)) + geom_line() +
    facet_grid(. ~b)
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel)

  graph <- graph + theme_minimal()

  return(graph)
}

#' gg_facet_line_ver_CaCa.
#' Vertical Line
#' @name gg_facet_line_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_line_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                            yLabel = "Count"){

  graph <- gg_facet_line_hor_CaCa.(data, titleLabel, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_facet_line_point_hor_CaCa.
#' Horizontal Line Point
#' @name gg_facet_line_point_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_line_point_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                    yLabel = "Count"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=b)) + geom_line() +
    geom_point() + facet_grid(. ~b)
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel)

  graph <- graph + theme_minimal()

  return(graph)
}

#' gg_facet_line_point_ver_CaCa.
#' Vertical Line Point
#' @name gg_facet_line_point_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_line_point_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                    yLabel = "Count"){

  graph <- gg_facet_line_point_hor_CaCa.(data, titleLabel, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_bar_stacked_100_ver_CaCa.
#' Stacked 100pct vertical Bar
#' @name gg_bar_stacked_100_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                        yLabel = 'Percentage', fillLabel = NULL,
                                        leg_pos = "top"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(a, fill=b)) + geom_bar(position = "fill")
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_bar_stacked_100_hor_CaCa.
#' Stacked 100pct horizontal Bar
#' @name gg_bar_stacked_100_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                          yLabel = 'Percentage', fillLabel = NULL,
                                          leg_pos = "top"){


  graph <- gg_bar_stacked_100_ver_CaCa.(data, titleLabel, xLabel, yLabel,
                                       fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_area_stacked_hor_CaCa.
#' Stacked horizontal Area
#' @name gg_area_stacked_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = 'Count', fillLabel = NULL,
                                       leg_pos = "top"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d


  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=n()) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)
  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=count, group=b)) + geom_area(aes(fill = b), position = "stack")
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_area_stacked_ver_CaCa.
#' Stacked vertical Area
#' @name gg_area_stacked_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                      yLabel = 'Count', fillLabel = NULL,
                                      leg_pos = "top"){

  graph <- gg_area_stacked_hor_CaCa.(data, titleLabel, xLabel, yLabel,
                                        fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_area_stacked_100_hor_CaCa.
#' Stacked horizontal Area 100pct
#' @name gg_area_stacked_100_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                      yLabel = 'Count', fillLabel = NULL,
                                      leg_pos = "top"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d


  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=n()) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)
  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=count, group=b)) + geom_area(aes(fill = b), position = "fill")
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)
  return(graph)
}

#' gg_area_stacked_100_ver_CaCa.
#' Stacked vertical Area 100pct
#' @name gg_area_stacked_100_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                      yLabel = 'Count', fillLabel = NULL,
                                      leg_pos = "top"){

  graph <- gg_area_stacked_100_hor_CaCa.(data, titleLabel, xLabel, yLabel,
                                     fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_marimekko_ver_CaCa.
#' Vertical Marimekko
#' @name gg_marimekko_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_marimekko_ver_CaCa. <- function(data, titleLabel = "Report", xLabel = "Category",
                                   yLabel = "Frequency", fillLabel = "Types",
                                   leg_pos = "top"){
  f <- fringe(data)
  data <- f$d
  xvar <- deparse(substitute(a))
  yvar <- deparse(substitute(b))
  mytable <- table(data)
  widths <- c(0, cumsum(apply(mytable, 1, sum)))
  heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))})

  alldata <- data.frame()
  allnames <- data.frame()
  for(i in 1:nrow(mytable)){
    for(j in 1:ncol(mytable)){
      alldata <- rbind(alldata, c(widths[i], widths[i+1], heights[j, i], heights[j+1, i]))
    }
  }
  colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")

  alldata[[xvar]] <- rep(dimnames(mytable)[[1]],rep(ncol(mytable), nrow(mytable)))
  alldata[[yvar]] <- rep(dimnames(mytable)[[2]],nrow(mytable))

  graph <- ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
    geom_rect(color="black", aes_string(fill=yvar)) +
    xlab(paste(xvar, "(count)")) + ylab(paste(yvar, "(proportion)"))
  return(graph)
}

#' gg_marimekko_hor_CaCa.
#' Horizontal Marimekko
#' @name gg_marimekko_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_marimekko_hor_CaCa. <- function(data, titleLabel = "Report", xLabel = "Category",
                                   yLabel = "Frequency", fillLabel = "Types",
                                   leg_pos = "top"){
  graph <- gg_marimekko_ver_CaCa.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
  graph <- graph + coord_flip()
  return(graph)
}

#' gg_bar_stacked_polar_CaCa.
#' Stacked Polar bar
#' @name gg_bar_stacked_polar_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_polar_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                     yLabel = 'Count', fillLabel = NULL, leg_pos = "top"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a,b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(a, fill = b, weight = count)) + geom_bar(width = 1) +
    coord_polar()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill=flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_bar_stacked_polar_100_CaCa.
#' Stacked Polar  100pct
#' @name gg_bar_stacked_polar_100_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_polar_100_CaCa. <- function(data, titleLabel = "Report", xLabel = NULL,
                                           yLabel = 'Count', fillLabel = NULL, leg_pos = "top",
                                           width = 1){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a,b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(a, fill = b, weight = count)) +
    geom_bar(width = 1, position = "fill") + coord_polar()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill=flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}


#' gg_bar_facet_circular_CaCa.
#' Circular Bar
#' @name gg_bar_facet_circular_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_circular_CaCa. <- function(data, titleLabel = "Report", fillLabel = NULL,
                                leg_pos="right", width = 0.85){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(x = a, y = count , fill = a )) +
    geom_bar(width = width, stat="identity") + coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  return(graph)
}

#' gg_treemap_x_CaCa.
#' Treemap fill first Ca
#' @name gg_treemap_x_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CaCa. <- function(data, titleLabel = "Report", fillLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a", label = 'b'),
                     group.label.colour = "black", label.colour = "black") + #guides(fill=FALSE) +
    labs(title = titleLabel)

  return(graph)
}

#' gg_treemap_y_CaCa.
#' Treemap fill second Ca
#' @name gg_treemap_y_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CaCa. <- function(data, titleLabel = "Report", fillLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") + #guides(fill=FALSE) +
    labs(title = titleLabel)

  return(graph)
}
