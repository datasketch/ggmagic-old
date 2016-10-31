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
gg_bubble_CaCaNu.  <- function(data, titleLabel = "", xLabel = NULL,
                             yLabel = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, size = c))
  graph <- graph + geom_point()
  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab)
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
gg_bar_facet_circular_CaCaNu. <- function(data, titleLabel = "Report", fillLabel = NULL,
                                        leg_pos="right", width = 0.85, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = c , fill = a )) +
    geom_bar(width = width, stat="identity") + coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel) +
           scale_fill_manual(values = getPalette())
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  return(graph)
}

#' gg_coloured_bubble_CaCaNu.
#' Coloured Bubble
#' @name gg_coloured_bubble_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_coloured_bubble_CaCaNu.  <- function(data, titleLabel = "Report", xLabel = NULL,
                                      yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, size = c))
  graph <- graph + geom_point(aes(color = a))
  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab)
  graph <- graph + theme_minimal() + theme(legend.position="none")

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
gg_bar_facet_coloured_x_ver_CaCaNu.<- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = a, weight = c, fill = factor(a))) + geom_bar() +
    labs(title = titleLabel, x = xlab, y = ylab, fill = flab) + theme_minimal() +
    theme(legend.position=leg_pos) + facet_grid(. ~b)

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
gg_bar_facet_coloured_x_hor_CaCaNu.<- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", ...){

  graph <- gg_bar_facet_coloured_x_ver_CaCaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
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
gg_bar_facet_coloured_y_ver_CaCaNu.<- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = a, weight = c, fill = factor(b))) + geom_bar() +
    labs(title = titleLabel, x = xlab, y = ylab, fill = flab) + theme_minimal() +
    theme(legend.position=leg_pos) + facet_grid(. ~b)

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
gg_bar_facet_coloured_y_hor_CaCaNu.<- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", ...){

  graph <- gg_bar_facet_coloured_y_ver_CaCaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
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
gg_bar_facet_coloured_z_ver_CaCaNu.<- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(suma=sum(c))

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = suma)) + geom_bar(stat = "identity") +
    labs(title = titleLabel, x = xlab, y = ylab, fill = flab) + theme_minimal() +
    theme(legend.position=leg_pos) + facet_grid(. ~b)

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
gg_bar_facet_coloured_z_hor_CaCaNu.<- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", ...){

  graph <- gg_bar_facet_coloured_z_ver_CaCaNu.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
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
gg_bar_facet_coloured_parameter_ver_ver_CaCaNu. <- function(data, titleLabel = "Report",
                                                      xLabel = NULL, yLabel = 'Count',
                                                      parameter1 = NULL, parameter2 = NULL,
                                                      leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
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
                                                      leg_pos = "right", ...){

  graph <- gg_bar_facet_coloured_parameter_ver_ver_CaCa.(data, titleLabel, xLabel,
                                                     yLabel, parameter1, parameter2, leg_pos)

  graph <- graph + coord_flip()
  return(graph)
}

#' vertical_bargraphCCN
#' vertical bar graph
#' @name vertical_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
vertical_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                yLabel = "Frequency", fillLabel = "Types", leg_pos = "top", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(a, fill=b, weights = c)) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}


#' ordered_vertical_bargraphCCN
#' ordered vertical bar graph
#' @name ordered_vertical_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
ordered_vertical_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Frequency",
                                        yLabel =  "Categories", fillLabel = "Types",
                                        leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x=reorder(data$b,rep(1,length(data$b)),sum),fill=a, weights = c)) +
    geom_bar()

  graph <- graph + labs(title = titleLabel, x = yLabel, y = xLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}



#' ordered_horizontal_bargraphCCN
#' ordered horizontal bar graph
#' @name ordered_horizontal_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
ordered_horizontal_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Frequency",
                                          yLabel =  "Categories", fillLabel = "Types",
                                          leg_pos = "right", ...){

  graph <- ordered_vertical_bargraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}


#' horizontal_bargraphCCN
#' horizontal bar graph
#' @name horizontal_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
horizontal_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                  yLabel = "Category", fillLabel = "Types", leg_pos = "top", ...){

  graph <- vertical_bargraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}


#' vertical_dotgraphCCN
#' vertical dot graph
#' @name vertical_dotgraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
vertical_dotgraphCCN <- function(data, titleLabel = "Report", xLabel = "Categories", yLabel = "Frequency",
                                fillLabel = "Types", leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data = data, aes(a, fill = factor(b), weights = c)) +
    geom_dotplot(stackgroups = TRUE, binpositions = "all")

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + scale_y_continuous(breaks = NULL) +
    theme(legend.position=leg_pos)

  return(graph)
}



#' horizontal_dotgraphCCN
#' horizontal dot graph
#' @name horizontal_dotgraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
horizontal_dotgraphCCN <- function(data, titleLabel = "Report", xLabel = "Categories", yLabel = "Frequency",
                                  fillLabel = "Types", leg_pos = "top", ...){

  graph <- vertical_dotgraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}




#' vertical_unstacked_bargraphCCN
#' vertical unstacked bargraph
#' @name vertical_unstacked_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
vertical_unstacked_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                          yLabel = "Frequency", fillLabel = "Types",
                                          leg_pos = "top", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(a, weights = c)) + geom_bar(aes(fill=data$b), position = "dodge")
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

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
gg_facet_pie_CaCaNu. <- function(data, titleLabel = "Report", fillLabel = NULL, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data=data, aes(x = factor(1), weight = c, fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  return(graph)
}

#Width debe de ser un parámetro.  0 < width < 1.

#' gg_donut_CaCaNu.
#' Facet Donut
#' @name gg_donut_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_CaCaNu. <- function(data, titleLabel = "Report", fillLabel = NULL,
                           width = 0.3, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = c)) +
    geom_bar(width = width) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  return(graph)
}

#' gg_bullseye_CaCaNu.
#' Facet Bullseye
#' @name gg_bullseye_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_CaCaNu. <- function(data, titleLabel = "Report", fillLabel = NULL,
                              leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = c)) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
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
gg_bar_stacked_polar_CaCaNu. <- function(data, width = 0.95, titleLabel = "Report",
                               fillLabel = NULL, leg_pos= "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, weight = c, fill = b)) +
    geom_bar(width = width, position = "stack") +
    coord_polar() + labs(title = titleLabel, fill = flabel, x = "", y = "") + theme_bw() +
    theme(legend.position=leg_pos)

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
gg_bar_stacked_polar_100_CaCaNu. <- function(data, width = 0.95, titleLabel = "Report",
                                         fillLabel = NULL, leg_pos= "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, weight = c, fill = b)) +
    geom_bar(width = width, position = "fill") +
    coord_polar() + labs(title = titleLabel, fill = flabel, x = "", y = "") + theme_bw() +
    theme(legend.position=leg_pos)

  return(graph)
}

#' horizontal_unstacked_bargraphCCN
#' horizontal unstacked bargraph
#' @name horizontal_unstacked_bargraphCCN
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
horizontal_unstacked_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                            yLabel = "Frequency", fillLabel = "Types",
                                            leg_pos = "top", ...){
  graph <- vertical_unstacked_bargraphCCN(data, titleLabel, xLabel, yLabel,
                                         fillLabel, leg_pos)

  graph <- graph + coord_flip()

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
horizontal_linegraphCCN <- function(data, titleLabel = "Report", xLabel = "Types",
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
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)

  graph <- graph + theme_minimal()

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
vertical_linegraphCCN <- function(data, titleLabel = "Report", xLabel = "Types",
                                 yLabel = "Frequency", ...){

  graph <- horizontal_linegraphCCN(data, titleLabel, xLabel, yLabel)
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
gg_bar_stacked_ver_CaCaNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                                        yLabel = NULL, fillLabel = NULL,
                                        leg_pos = "top", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(a, y = c, fill=b)) + geom_bar(stat="identity", position = "stack")
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill = flab)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

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
gg_bar_stacked_hor_CaCaNu. <- function(data, titleLabel = "Report", xLabel = "Category",
                                          yLabel = "Frequency", fillLabel = "Types",
                                          leg_pos = "top", ...){


  graph <- gg_bar_stacked_ver_CaCaNu.(data, titleLabel, xLabel, yLabel,
                                       fillLabel, leg_pos)
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
gg_bar_stacked_100_ver_CaCaNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL,
                                       leg_pos = "top", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(a, y = c, fill=b)) + geom_bar(stat="identity", position = "fill")
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, fill = flab)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

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
gg_bar_stacked_100_hor_CaCaNu. <- function(data, titleLabel = "Report", xLabel = "Category",
                                       yLabel = "Frequency", fillLabel = "Types",
                                       leg_pos = "top", ...){


  graph <- gg_bar_stacked_100_ver_CaCaNu.(data, titleLabel, xLabel, yLabel,
                                      fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#' horizontal_area_bargraphCC
#' horizontal area bar graph
#' @name horizontal_area_bargraphCC
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
horizontal_area_bargraphCC <- function(data, titleLabel = "Report", xLabel = "Category",
                                       yLabel = "Frequency", fillLabel = "Types",
                                       leg_pos = "top", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(sum = sum(c)) %>%
    dplyr::arrange(desc(sum))
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=sum, group=b)) + geom_area(aes(fill = b), position = "stack")
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

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
gg_treemap_x_CaCaNu. <- function(data, titleLabel = "Report", fillLabel = NULL, ...){

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

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel)

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
gg_treemap_y_CaCaNu. <- function(data, titleLabel = "", fillLabel = NULL){

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

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel)

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
gg_treemap_density_z_CaCaNu. <- function(data, titleLabel = "Report",
                                         fillLabel = NULL){

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

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'Sum', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel)

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
gg_pyramid_CaCaNu. <- function(data, titleLabel = "", xLabel = "Category",
                               yLabel = "Frequency", fillLabel = "Types",
                               leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data$c <- ifelse(data$a == unique(data$a)[1], -data$c, data$c)

  ggplot(data, aes(x = b, y = c, fill = a)) +
    geom_bar(data = subset(data, a == unique(data$a)[1]), stat = "identity") +
    geom_bar(data = subset(data, a == unique(data$a)[2]), stat = "identity",position = "identity") +
    scale_y_continuous(labels = abs) + theme_ds() +
    coord_flip()
}






