




#' gg_pie_facet_CaCaNu.
#' Facet Pie
#' @name gg_pie_facet_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), weight = c, fill = a)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph +
    theme(legend.position=leg_pos) +
    theme_ds() +
    theme_ds_clean() +
    scale_fill_manual(values = getPalette())
  graph <- graph +
    theme(legend.position=leg_pos) +
    facet_grid(. ~b)

  graph
}

#Width debe de ser un parámetro.  0 < width < 1.

#' gg_donut_facet_CaCaNu.
#' Facet Donut
#' @name gg_donut_facet_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                   width = 0.3, leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = c)) +
    geom_bar(width = width) +
    coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
    theme(legend.position=leg_pos) +
    theme_ds() +
    theme_ds_clean() +
    scale_fill_manual(values = getPalette())
  graph <- graph +
    theme(legend.position=leg_pos) +
    facet_grid(. ~b)

  graph
}

#' gg_bullseye_facet_CaCaNu.
#' Facet Bullseye
#' @name gg_bullseye_facet_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = c)) +
    geom_bar(width = 1) +
    coord_polar(theta = "x")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph +
    theme(legend.position=leg_pos) +
    theme_ds() +
    theme_ds_clean() +
    scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  graph
}



#' gg_bubble_CaCaNu.
#' Bubble
#' @name gg_bubble_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CaCaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, size = c))
  graph <- graph + geom_point(aes(colour = ""))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph  + theme(legend.position="none") +
    theme_ds() + scale_color_manual(values = getPalette()) +
    guides(size = FALSE, colour = FALSE)

  graph
}




#' gg_bubble_coloured_x_CaCaNu.
#' Coloured Bubble first Ca
#' @name gg_bubble_coloured_x_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_x_CaCaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, size = c))
  graph <- graph + geom_point(aes(color = a)) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + theme(legend.position="none")

  graph
}

#' gg_bubble_coloured_y_CaCaNu.
#' Coloured Bubble second Ca
#' @name gg_bubble_coloured_y_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_y_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, size = c)) +
    geom_point(aes(color = b)) +
    scale_color_manual(values = getPalette())
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph +
    theme_ds() +
    theme(legend.position="none")

  graph
}





#' gg_line_hor_facet_CaCaNu.
#' horizontal linegraph
#' @name gg_line_hor_facet_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Types",
                                      yLabel = "Frequency", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(sum = sum(c)) %>%
    dplyr::arrange(desc(sum))

  graph <- ggplot(data = data_graph, aes(x = a, y = sum, group=b)) +
    geom_line() +
    geom_point() + facet_grid(. ~b)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds()

  graph
}

#' gg_line_ver_facet_CaCaNu.
#' vertical linegraph
#' @name gg_line_ver_facet_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_ver_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Types",
                                      yLabel = "Frequency", ...){

  graph <- gg_line_hor_facet_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel)
  graph <- graph + coord_flip()

  graph
}


#' gg_area_stacked_hor_CaCaNu.
#' Stacked horizontal Area
#' @name gg_area_stacked_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d


  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count=sum(c)) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x=a, y=count, group=b)) +
    geom_area(aes(fill = b), position = "stack")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    scale_fill_manual(values = getPalette())

  graph
}




#' gg_area_stacked_ver_CaCaNu.
#' Stacked area
#' @name gg_area_stacked_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos = "right", ...){


  graph <- gg_area_stacked_hor_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_area_stacked_100_hor_CaCaNu.
#' Stacked horizontal Area 100
#' @name gg_area_stacked_100_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d


  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count=sum(c)) %>%
    tidyr::spread(b, count) %>%
    tidyr::gather(b, count, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x=a, y=count, group=b)) +
    geom_area(aes(fill = b), position = "fill")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph +
    theme_ds() +
    scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent)

  graph
}

#' gg_area_stacked_100_ver_CaCaNu.
#' Stacked area 100
#' @name gg_area_stacked_100_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, leg_pos = "right", ...){


  graph <- gg_area_stacked_100_hor_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_treemap_x_CaCaNu.
#' Treemap Fill by first Ca
#' @name gg_treemap_x_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
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

  graph
}

#' gg_treemap_y_CaCaNu.
#' Treemap Fill by second Ca
#' @name gg_treemap_y_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
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

  graph
}

#' gg_treemap_density_z_CaCaNu.
#' Treemap Density by Nu
#' @name gg_treemap_density_z_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_z_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", reverse = FALSE,
                                         fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
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

  graph
}



#' gg_pyramid_CaCaNu.
#' pyramid
#' @name gg_pyramid_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pyramid_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL,
                               leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[3]
  data <- f$d

  data$c <- ifelse(data$a %in% unique(data$a)[1], -data$c, data$c)

  graph <- ggplot(data, aes(x = b, y = c, fill = a)) +
    geom_bar(data = subset(data, a %in% unique(data$a)[1]), stat = "identity") +
    geom_bar(data = subset(data, a %in% unique(data$a)[2]), stat = "identity", position = "identity") +
    scale_y_continuous(labels = abs) +
    theme_ds() +
    scale_fill_manual(values=getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel) +
    scale_y_continuous(labels = comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()

  graph
}

#' gg_multi_line_point_CaCaNu.
#' Grouped Line Color Point
#' @name gg_multi_line_point_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_point_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                        fillLabel = NULL, leg_pos="right", type = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = as.factor(b), y = c, group = a)) + geom_point(aes(color = a), shape = type) + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette())

  graph
}

#' gg_multi_line_CaCaNu.
#' Grouped Line Coloured
#' @name gg_multi_line_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                  fillLabel = NULL, leg_pos="right", type = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(x = as.factor(b), y = c, group = a))  + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette())

  graph
}




