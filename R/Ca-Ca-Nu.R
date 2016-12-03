#' gg_bubble_CaCaNu.
#' Bubble
#' @name gg_bubble_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CaCaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, size = c))
  graph <- graph + geom_point(aes(colour = ""))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph  + theme(legend.position="none") +
    theme_ds() + scale_color_manual(values = getPalette()) +
    guides(size = FALSE, colour = FALSE)

  return(graph)
}



#' gg_bar_facet_circular_CaCaNu.
#' Circular Bar
#' @name gg_bar_facet_circular_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_circular_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                        leg_pos="right", width = 0.85, ...){

  f <- fringe(data)
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = c , fill = a )) +
    geom_bar(width = width, stat="identity") + coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
           scale_fill_manual(values = getPalette())
  graph <- graph + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  return(graph)
}

#' gg_coloured_x_bubble_CaCaNu.
#' Coloured Bubble first Ca
#' @name gg_coloured_x_bubble_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_coloured_x_bubble_CaCaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, size = c))
  graph <- graph + geom_point(aes(color = a)) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + theme(legend.position="none")

  return(graph)
}

#' gg_coloured_y_bubble_CaCaNu.
#' Coloured Bubble second Ca
#' @name gg_coloured_y_bubble_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_coloured_y_bubble_CaCaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, size = c))
  graph <- graph + geom_point(aes(color = b)) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + theme(legend.position="none")

  return(graph)
}

#' gg_bar_facet_coloured_x_ver_CaCaNu.
#' vertical bar
#' @name gg_bar_facet_coloured_x_ver_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_x_ver_CaCaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data, aes(x = a, weight = c, fill = factor(a))) + geom_bar() +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid(. ~b)

  return(graph)
}


#' gg_bar_facet_coloured_x_hor_CaCaNu.
#' horizontal bar
#' @name gg_bar_facet_coloured_x_hor_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_x_hor_CaCaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right", ...){

  graph <- gg_bar_facet_coloured_x_ver_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_bar_facet_coloured_y_ver_CaCaNu.
#' vertical bar
#' @name gg_bar_facet_coloured_y_ver_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_y_ver_CaCaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data, aes(x = a, weight = c, fill = factor(b))) + geom_bar() +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid(. ~b)

  return(graph)
}


#' gg_bar_facet_coloured_y_hor_CaCaNu.
#' horizontal bar
#' @name gg_bar_facet_coloured_y_hor_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_y_hor_CaCaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, leg_pos = "right", ...){

  graph <- gg_bar_facet_coloured_y_ver_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_bar_facet_coloured_z_ver_CaCaNu.
#' Facet coloured vertical bar
#' @name gg_bar_facet_coloured_z_ver_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_z_ver_CaCaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, reverse = FALSE, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(suma=sum(c))

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = suma)) +
    geom_bar(stat = "identity") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds()

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid(. ~b)

  return(graph)
}

#' gg_bar_facet_coloured_z_hor_CaCaNu.
#' Facet Coloured horizontal bar
#' @name gg_bar_facet_coloured_z_hor_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_z_hor_CaCaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, reverse = FALSE, leg_pos = "right", ...){

  graph <- gg_bar_facet_coloured_z_ver_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, reverse, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_bar_facet_coloured_parameter_ver_ver_CaCaNu.
#' Facet Vertical coloured by parameter bars
#' @name gg_bar_facet_coloured_parameter_ver_ver_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_parameter_ver_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                      xLabel = NULL, yLabel = NULL,
                                                      parameter1 = NULL, parameter2 = NULL,
                                                      leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  p_a <-  parameter1 %||% sample(unique(data[,nms[1]]), length(unique(data[,nms[2]])))
  p_b <-  parameter2 %||% sample(unique(data[,nms[2]]), length(unique(data[,nms[2]])))
  data <- f$d
  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = sum(c))

  list_df <- apply(cbind(p_a, p_b), 1, function(x){
    df <- data_graph%>% mutate(color = ifelse(a==x[1] & b==x[2], TRUE, FALSE))
    df[df$color,]
  })
  df <- bind_rows(list_df)
  data_graph <- left_join(data_graph, df, by = c("a", "b", "count"))
  data_graph[is.na(data_graph)] <- FALSE

  graph <- ggplot(data_graph, aes(a, weight = count)) +
    geom_bar(position ="dodge", aes(fill =  color %in% TRUE))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + guides(fill=FALSE) +
    scale_fill_manual(values = getPalette())
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos) + facet_grid(.~b)
  return(graph)
}

#' gg_bar_facet_coloured_parameter_ver_hor_CaCaNu.
#' Facet Horizontal coloured by parameter Bars
#' @name gg_bar_facet_coloured_parameter_ver_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_coloured_parameter_ver_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                      xLabel = NULL, yLabel = NULL,
                                                      parameter1 = NULL, parameter2 = NULL,
                                                      leg_pos = "right", ...){

  graph <- gg_bar_facet_coloured_parameter_ver_ver_CaCaNu.(data, titleLabel, subtitle, caption, xLabel,
                                                     yLabel, parameter1, parameter2, leg_pos)

  graph <- graph + coord_flip()
  return(graph)
}


#' gg_bar_unstacked_hor_CaCaNu.
#' horizontal bar graph
#' @name gg_bar_unstacked_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_unstacked_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, leg_pos = "right", ...){

  graph <- gg_bar_unstacked_ver_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}




#' gg_bar_unstacked_ver_CaCaNu.
#' vertical unstacked bargraph
#' @name gg_bar_unstacked_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_unstacked_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=sum(c)) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)

  graph <- ggplot(data_graph, aes(a, weight=count, fill=b)) + geom_bar(position = "dodge")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(legend.position=leg_pos) + guides(text = FALSE)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette())

  return(graph)
}

#' gg_facet_pie_CaCaNu.
#' Facet Pie
#' @name gg_facet_pie_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_pie_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d
  graph <- ggplot(data=data, aes(x = factor(1), weight = c, fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph + theme(legend.position=leg_pos) + theme_ds() +
    theme_ds_clean() + scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  return(graph)
}

#Width debe de ser un parámetro.  0 < width < 1.

#' gg_facet_donut_CaCaNu.
#' Facet Donut
#' @name gg_facet_donut_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_donut_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                           width = 0.3, leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d
  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = c)) +
    geom_bar(width = width) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph + theme(legend.position=leg_pos) + theme_ds() +
    theme_ds_clean() + scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  return(graph)
}

#' gg_facet_bullseye_CaCaNu.
#' Facet Bullseye
#' @name gg_facet_bullseye_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_facet_bullseye_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = c)) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph + theme(legend.position=leg_pos) + theme_ds() +
    theme_ds_clean() + scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  return(graph)
}

#' gg_bar_stacked_polar_CaCaNu.
#' Stacked Polar Bar
#' @name gg_bar_stacked_polar_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_polar_CaCaNu. <- function(data, width = 0.95, titleLabel = "", subtitle = "", caption = "",
                                         leg_pos= "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, weight = c, fill = b)) +
    geom_bar(width = width, position = "stack") +
    coord_polar()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph + theme(legend.position=leg_pos) + theme_ds() +
    theme_ds_clean() + scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_bar_stacked_polar_100_CaCaNu.
#' Stacked Polar Bar 100
#' @name gg_bar_stacked_polar_100_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_polar_100_CaCaNu. <- function(data, width = 0.95, titleLabel = "", subtitle = "", caption = "",
                                         fillLabel = NULL, leg_pos= "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, weight = c, fill = b)) +
           geom_bar(width = width, position = "fill") +
           coord_polar() + theme(legend.position=leg_pos) + theme_ds() +
           theme_ds_clean() + scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption)
  return(graph)
}

#' horizontal_linegraphCCN
#' horizontal linegraph
#' @name horizontal_linegraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
horizontal_linegraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Types",
                                   yLabel = "Frequency", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(sum = sum(c)) %>%
    dplyr::arrange(desc(sum))

  graph <- ggplot(data = data_graph, aes(x = a, y = sum, group=b)) + geom_line() +
           geom_point() + facet_grid(. ~b)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)

  graph <- graph + theme_ds()

  return(graph)
}

#' vertical_linegraphCCN
#' vertical linegraph
#' @name vertical_linegraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
vertical_linegraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Types",
                                 yLabel = "Frequency", ...){

  graph <- horizontal_linegraphCCN(data, titleLabel, subtitle, caption, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}


#' gg_bar_stacked_ver_CaCaNu.
#' vertical stacked bar graph
#' @name gg_bar_stacked_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data, aes(a, y = c, fill=b)) + geom_bar(stat="identity", position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette())

  return(graph)
}

#' gg_bar_stacked_hor_CaCaNu.
#' horizontal stacked bar graph
#' @name gg_bar_stacked_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos = "right", ...){


  graph <- gg_bar_stacked_ver_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_bar_stacked_100_ver_CaCaNu.
#' 100 vertical stacked bar graph
#' @name gg_bar_stacked_100_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data, aes(a, y = c, fill=b)) +
    geom_bar(stat="identity", position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent)

  return(graph)
}

#' gg_bar_stacked_100_hor_CaCaNu.
#' 100 horizontal stacked bar graph
#' @name gg_bar_stacked_100_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right", ...){


  graph <- gg_bar_stacked_100_ver_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_area_stacked_hor_CaCaNu.
#' Stacked horizontal Area
#' @name gg_area_stacked_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d


  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=sum(c)) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)
  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=count, group=b)) + geom_area(aes(fill = b), position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette())

  return(graph)
}

#' gg_area_stacked_ver_CaCaNu.
#' Stacked area
#' @name gg_area_stacked_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos = "right", ...){


  graph <- gg_area_stacked_hor_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_area_stacked_100_hor_CaCaNu.
#' Stacked horizontal Area 100
#' @name gg_area_stacked_100_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d


  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=sum(c)) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)
  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=count, group=b)) + geom_area(aes(fill = b), position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent)

  return(graph)
}

#' gg_area_stacked_100_ver_CaCaNu.
#' Stacked area 100
#' @name gg_area_stacked_100_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos = "right", ...){


  graph <- gg_area_stacked_100_hor_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_treemap_x_CaCaNu.
#' Treemap Fill by first Ca
#' @name gg_treemap_x_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 label_size = 5, ...){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = sum(c))


  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a",
                                label = "b"), group.label.colour = "white",
                     label.colour = "white", label.size.factor = 2,
                     group.label.size.threshold = 1) + guides(fill = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean()

  return(graph)
}

#' gg_treemap_y_CaCaNu.
#' Treemap Fill by second Ca
#' @name gg_treemap_y_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = sum(c)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b',
                                group = "a", label = "b"), group.label.colour = "white",
                     label.colour = "white", label.size.factor = 2,
                     group.label.size.threshold = 1) + guides(fill = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean()

  return(graph)
}

#' gg_treemap_density_z_CaCaNu.
#' Treemap Density by Nu
#' @name gg_treemap_density_z_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_z_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", reverse = FALSE,
                                         fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(Sum = sum(c)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'Sum',
                                group = "a", label = "b"), group.label.colour = "white",
                     label.colour = "white", label.size.factor = 2,
                     group.label.size.threshold = 1) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean()

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  return(graph)
}



#' gg_pyramid_CaCaNu.
#' pyramid
#' @name gg_pyramid_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pyramid_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL,
                               leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[3]
  data <- f$d

  data$c <- ifelse(data$a %in% unique(data$a)[1], -data$c, data$c)

  graph <- ggplot(data, aes(x = b, y = c, fill = a)) +
    geom_bar(data = subset(data, a %in% unique(data$a)[1]), stat = "identity") +
    geom_bar(data = subset(data, a %in% unique(data$a)[2]), stat = "identity",
             position = "identity") +
    scale_y_continuous(labels = abs) + theme_ds() +
    scale_fill_manual(values=getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel) +
    scale_y_continuous(labels = comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()

  return(graph)
}

#' gg_multi_line_point_CaCaNu.
#' Grouped Line Color Point
#' @name gg_multi_line_point_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_point_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                      fillLabel = NULL, leg_pos="right", type = 1, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = as.factor(b), y = c, group = a)) + geom_point(aes(color = a), shape = type) + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette())

  return(graph)
}

#' gg_multi_line_CaNu.
#' Grouped Line Coloured
#' @name gg_multi_line_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                fillLabel = NULL, leg_pos="right", type = 1, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = as.factor(b), y = c, group = a))  + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette())

  return(graph)
}




