#' gg_scatter_hor_CaDaNu.: title.
#' pointlines
#' @name gg_scatter_hor_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_scatter_hor_CaDaNu. <- function(data,title = "", subtitle = "", caption = "", xlab = NULL,
                                   ylab=NULL, clab = NULL, angle = 45, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xlab %||% nms[2]
  ylab <- ylab %||% nms[3]
  clab <- clab %||% nms[1]
  d <- f$d

  d <- d %>% dplyr::arrange(b)

  graph <- ggplot(d, aes(x = as.Date(b, origin = data[1,2]), y = c, colour = a)) +
           geom_point() +
           scale_color_manual(values = getPalette()) +
           theme_ds() + labs(title = title, subtitle = subtitle, caption = caption, x= xlab, y = ylab) +
           theme(axis.text.x = element_text(angle = angle, hjust = 1))
  graph
}

#' gg_steam_CaDaNu.
#' Steam
#' @name gg_steam_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_steam_CaDaNu. <- function (data, title = "", subtitle = "", caption = "",
                              xLabel = NULL, yLabel = NULL, leg_pos = "right", ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d
  data$b <- lubridate::as_date(data$b)

  data_graph <- data %>%
                tidyr::drop_na(a,b) %>%
                dplyr::group_by(a) %>%
                tidyr::spread(b, c) %>%
                tidyr::gather(b, c, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data_graph, aes(x = as.Date(b, origin = data[1,2]), y = c, group = a, fill = a)) +
           stat_steamgraph() +
           theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
           scale_fill_manual(values = getPalette()) +
           theme_ds() +
           labs(title = title, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel) +
           theme(legend.position = leg_pos)
  graph
}



#' gg_area_stacked_ver_CaDaNu.
#' Stacked Vertical Area
#' @name gg_area_stacked_ver_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_ver_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, fillLabel = NULL,
                                            leg_pos = "top", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  data$b <- lubridate::as_date(data$b)

  data_graph <- data %>%
                tidyr::drop_na(a,b) %>%
                tidyr::spread(b, c) %>%
                tidyr::gather(b, c, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x = as.Date(b, origin = data[1,2]), y = c, group = a)) +
           geom_area(aes(fill = a), position = "stack")
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab, fill = flabel)
  graph <- graph +
           theme_ds() +
           theme(legend.position=leg_pos) +
           scale_fill_manual(values = getPalette())
  graph
}

#' gg_area_stacked_hor_CaDaNu.
#' Stacked Horizontal Area
#' @name gg_area_stacked_hor_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, fillLabel = NULL,
                                        leg_pos = "top", ...){

  graph <- gg_area_stacked_ver_CaDaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_area_stacked_100_ver_CaDaNu.
#' Stacked Vertical Area 100
#' @name gg_area_stacked_100_ver_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL,
                                          leg_pos = "top", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  data$b <- lubridate::as_date(data$b)

  data_graph <- data %>%
                tidyr::drop_na(a,b) %>%
                tidyr::spread(b, c) %>%
                tidyr::gather(b, c, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x = as.Date(b, origin = data[1,2]), y = c, group = a)) +
           geom_area(aes(fill = a), position = "fill")
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption =  caption, x = xLabel, y = ylab, fill = flabel)
  graph <- graph +
           theme_ds() +
           theme(legend.position=leg_pos) +
           scale_fill_manual(values = getPalette())
  graph
}

#' gg_area_stacked_100_hor_CaDaNu.
#' Stacked Horizontal Area 100
#' @name gg_area_stacked_100_hor_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, fillLabel = NULL,
                                            leg_pos = "top", ...){

  graph <- gg_area_stacked_100_ver_CaDaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_multi_line_point_CaDaNu.
#' Grouped Line Color Point
#' @name gg_multi_line_point_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_point_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                        fillLabel = NULL, leg_pos="right", type = 1,
                                        text_size = 8, angle_x = 45, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = b, y = c, group = a)) +
             geom_point(aes(color = a), shape = type) +
             geom_line(aes(color = a))
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph +
           theme_ds() +
           scale_color_manual(values = getPalette()) +
           scale_x_date() +
           theme(axis.text.x = element_text(size = text_size, angle = angle_x, hjust = 1))

  graph
}

#' gg_multi_line_CaDaNu.
#' Grouped Line Coloured
#' @name gg_multi_line_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                  fillLabel = NULL, leg_pos="right", type = 1,
                                  text_size = 8, angle_x = 45, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = b, y = c, group = a))  + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    scale_x_date() + theme(axis.text.x = element_text(size = text_size, angle = angle_x, hjust = 1))

  graph
}

#' gg_bar_stacked_ver_CaDaNu.
#' vertical stacked bar graph
#' @name gg_bar_stacked_ver_CaDaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right", angle_x = 45,
                                       text_size = 10, hline = NULL, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d
  data$b <- lubridate::as_date(data$b)

  data <- data %>%
          tidyr::drop_na(a,b)

  graph <- ggplot(data, aes(x = as.Date(b, origin = data[1,2]), y = c, fill = a, group = b)) +
           geom_bar(stat="identity", position = "stack")

  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph +
           theme_ds() +
           scale_fill_manual(values = getPalette()) +
           theme(axis.text.x = element_text(size = text_size, angle = angle_x, hjust = 1)) +
           scale_x_date()

  if(!is.null(hline)){
    graph <- graph + geom_hline(data = data.frame(valores = hline),
                                aes(yintercept = valores), linetype="dotted")
  }

  graph
}

#gg_scatter_trend_hor_CaDaNu.





###
#
# flip_circleAreaPlotCCN  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Category",
#                                     yLabel = "Category", leg_pos = "top", ...){
#
#   graph <- circleAreaPlotCCN(data, titleLabel, xLabel, yLabel, leg_pos)
#   graph <- graph + coord_flip()
#
#   return(graph)
# }
#
# vertical_bargraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Category",
#                                  yLabel = "Frequency", fillLabel = "Types", leg_pos = "top", ...){
#
#   graph <- ggplot(data, aes(a, fill=b, weights = c)) + geom_bar()
#   graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
#   graph <- graph + theme_ds() + theme(legend.position=leg_pos)
#
#   return(graph)
# }
#
# ordered_vertical_bargraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Frequency",
#                                          yLabel =  "Categories", fillLabel = "Types",
#                                          leg_pos = "right", ...){
#
#   graph <- ggplot(data, aes(x=reorder(data$b,rep(1,length(data$b)),sum),fill=a, weights = c)) +
#     geom_bar()
#
#   graph <- graph + labs(title = titleLabel, x = yLabel, y = xLabel,  fill = fillLabel)
#   graph <- graph + theme_ds() + theme(legend.position=leg_pos)
#
#   return(graph)
# }
#
# ordered_horizontal_bargraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Frequency",
#                                            yLabel =  "Categories", fillLabel = "Types",
#                                            leg_pos = "right", ...){
#
#   graph <- ordered_vertical_bargraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
#
#   graph <- graph + coord_flip()
#
#   return(graph)
# }
#
# horizontal_bargraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Category",
#                                    yLabel = "Category", fillLabel = "Types", leg_pos = "top", ...){
#
#   graph <- vertical_bargraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
#   graph <- graph + coord_flip()
#
#   return(graph)
# }
#
# vertical_dotgraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Categories", yLabel = "Frequency",
#                                  fillLabel = "Types", leg_pos = "right", ...){
#
#   graph <- ggplot(data = data, aes(a, fill = factor(b), weights = c)) +
#     geom_dotplot(stackgroups = TRUE, binpositions = "all")
#
#   graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel,  fill = fillLabel)
#   graph <- graph + theme_ds() + scale_y_continuous(breaks = NULL) +
#     theme(legend.position=leg_pos)
#
#   return(graph)
# }
#
# horizontal_dotgraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Categories", yLabel = "Frequency",
#                                    fillLabel = "Types", leg_pos = "top", ...){
#
#   graph <- vertical_dotgraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
#
#   graph <- graph + coord_flip()
#
#   return(graph)
# }
#
# vertical_grouped_bargraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Category",
#                                            yLabel = "Frequency", fillLabel = "Types",
#                                            leg_pos = "top", ...){
#
#   graph <- ggplot(data, aes(a, weights = c)) + geom_bar(aes(fill=data$b), position = "dodge")
#   graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
#   graph <- graph + theme_ds() + theme(legend.position=leg_pos)
#
#   return(graph)
# }
#
# horizontal_grouped_bargraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Category",
#                                              yLabel = "Frequency", fillLabel = "Types",
#                                              leg_pos = "top", ...){
#   graph <- vertical_grouped_bargraphCCN(data, titleLabel, xLabel, yLabel,
#                                           fillLabel, leg_pos)
#
#   graph <- graph + coord_flip()
#
#   return(graph)
# }
#
#
# horizontal_linegraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Types",
#                                     yLabel = "Frequency", ...){
#
#   data_graph <- data %>%
#     dplyr::group_by(a, b) %>%
#     dplyr::summarise(sum = sum(c)) %>%
#     dplyr::arrange(desc(sum))
#
#   graph <- ggplot(data = data_graph, aes(x = a, y = sum, group=b)) + geom_line() +
#     geom_point() + facet_grid(. ~b)
#   graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)
#
#   graph <- graph + theme_ds()
#
#   return(graph)
# }
#
# vertical_linegraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Types",
#                                   yLabel = "Frequency", ...){
#
#   graph <- horizontal_linegraphCCN(data, titleLabel, xLabel, yLabel)
#   graph <- graph + coord_flip()
#
#   return(graph)
# }
#
# vertical_stacked_bargraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Category",
#                                          yLabel = "Frequency", fillLabel = "Types",
#                                          leg_pos = "top", ...){
#
#   graph <- ggplot(data, aes(a, fill=b, weights = c)) + geom_bar(position = "fill")
#   graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
#   graph <- graph + theme_ds() + theme(legend.position=leg_pos)
#
#   return(graph)
# }
#
# horizontal_stacked_bargraphCCN <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Category",
#                                            yLabel = "Frequency", fillLabel = "Types",
#                                            leg_pos = "top", ...){
#
#
#   graph <- vertical_stacked_bargraphCCN(data, titleLabel, xLabel, yLabel,
#                                         fillLabel, leg_pos)
#   graph <- graph + coord_flip()
#
#   return(graph)
# }
#
# horizontal_area_bargraphCC <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Category",
#                                        yLabel = "Frequency", fillLabel = "Types",
#                                        leg_pos = "top", ...){
#
#   data_graph <- data %>%
#     dplyr::group_by(a, b) %>%
#     dplyr::summarise(sum = sum(c)) %>%
#     dplyr::arrange(desc(sum))
#   graph <- ggplot(data = data_graph,
#                   aes(x=a, y=sum, group=b)) + geom_area(aes(fill = b), position = "stack")
#   graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
#   graph <- graph + theme_ds() + theme(legend.position=leg_pos)
#
#   return(graph)
# }



