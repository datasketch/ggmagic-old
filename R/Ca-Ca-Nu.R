#' Pie facet by second variable
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
gg_pie_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", leg_pos="right",
                                 aggregation = "sum", text = TRUE, type = 'count', color_text = "black", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                    b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = cumsum(c) - c/2,
                  percent = 100 * round(c / total, 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c=agg(aggregation,c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data=data, aes(x = factor(1), weight = c, fill = a)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph +
    theme_ds() +
    theme_ds_clean() +
    scale_fill_manual(values = getPalette())
  graph <- graph +
    theme(legend.position=leg_pos) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = c, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = c, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  graph
}

#Width debe de ser un parámetro.  0 < width < 1.

#' Donut facet
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
                                   width = 0.3, leg_pos="right",
                                   aggregation = "sum", text = TRUE, type = 'count', color_text = "black", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = cumsum(c) - c/2,
                  percent = 100 * round(c / total, 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c=agg(aggregation,c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = c)) +
    geom_bar(width = width) +
    coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
    theme_ds() +
    theme_ds_clean() +
    scale_fill_manual(values = getPalette())
  graph <- graph +
    theme(legend.position=leg_pos) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = c, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = c, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  graph
}

#' Bullseye facet
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

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = c)) +
    geom_bar(width = 1) +
    coord_polar(theta = "x")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph +
    theme_ds() +
    theme_ds_clean() +
    scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) + facet_wrap(~b)

  graph
}

#' Bubble
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
                               yLabel = NULL, aggregation = "sum", angle_x = 0,
                               text = TRUE, color_text = "black", type = "count",
                               shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = count))
  graph <- graph + geom_point(aes(colour = ""), shape = shape_type)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph  + theme(legend.position="none") +
    theme_ds() + scale_color_manual(values = getPalette()) +
    guides(size = FALSE, colour = FALSE) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = b, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -0.5))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = b, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -0.5))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured by first variable
#' Coloured Bubble first Ca
#' @name gg_bubble_coloured_x_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_x_CaCaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, aggregation = "sum", angle_x = 0,
                                          text = TRUE, color_text = "black", type = "count",
                                          shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(suma = agg(aggregation, c)) %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(total = sum(suma)) %>%
    dplyr::mutate(pos = suma*9/10, percent = 100 * round(suma/total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  suma = ifelse(suma == 0, NA, suma))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = suma))
  graph <- graph + geom_point(aes(color = a), shape = shape_type) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = b, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text, vjust = -0.5))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = b, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -0.5))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured by second variable
#' Coloured Bubble second Ca
#' @name gg_bubble_coloured_y_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_y_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL,  aggregation = "sum", angle_x = 0,
                                         text = TRUE, color_text = "black", type = "count",
                                         shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(suma = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(suma)) %>%
    dplyr::mutate(pos = suma*9/10, percent = 100 * round(suma/total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  suma = ifelse(suma == 0, NA, suma))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = suma)) +
    geom_point(aes(color = b), shape = shape_type) +
    scale_color_manual(values = getPalette())
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph +
    theme_ds() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = b, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text, vjust = -0.5))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = b, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -0.5))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal line facet
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
                                      yLabel = "Frequency", aggregation = "sum", angle_x = 0,
                                      shape_type = 19, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(sum = agg(aggregation, c)) %>%
    dplyr::arrange(desc(sum))

  graph <- ggplot(data = data_graph, aes(x = a, y = sum, group=b, colour = "")) +
    geom_line(show.legend = FALSE) +
    geom_point(shape = shape_type, show.legend = FALSE) + scale_color_manual(values = getPalette()) +
      facet_wrap(~b)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab)
  graph <- graph + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical line facet
#' vertical linegraph
#' @name gg_line_ver_facet_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_ver_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = "Types",
                                      yLabel = "Frequency", aggregation = "sum", angle_x = 0, ...){

  graph <- gg_line_hor_facet_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, aggregation, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}


#' Horizontal stacked area
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
                                        yLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count=agg(aggregation, c)) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x=a, y=count, group=b)) +
    geom_area(aes(fill = b), position = "stack")

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette())

  graph
}

#' Vertical stacked area
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
                                        yLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0,...){


  graph <- gg_area_stacked_hor_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, aggregation, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal 100% stacked area
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
                                            yLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count=agg(aggregation,c)) %>%
    tidyr::spread(b, count) %>%
    tidyr::gather(b, count, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x=a, y=count, group=b)) +
    geom_area(aes(fill = b), position = "fill")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph +
    theme_ds() + theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical 100% stacked area
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
                                            yLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0, ...){


  graph <- gg_area_stacked_100_hor_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, aggregation, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Treemap coloured by first variable
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
                                 label_size = 5, aggregation = "sum", text = "TRUE", color_text = "black",
                                 leg_pos = "right", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c))


  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a",
                                  label = "b"), group.labels = FALSE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}

#' Treemap coloured by second variable
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
gg_treemap_y_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 aggregation = "sum", text = TRUE, color_text = "black",
                                 leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a",
                                  label = "a"), group.labels = FALSE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a",
                                  label = "a"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}

#' Treemap density by numeric variable
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
                                         fillLabel = NULL, text = TRUE, color_text = "black", aggregation = "sum", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }

  # graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count',
  #                               group = "a", label = "b"), group.label.colour = "white",
  #                    label.colour = "white", label.size.factor = 2,
  #                    group.label.size.threshold = 1) +

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph
}

#' Pyramid
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
gg_pyramid_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, aggregation = "sum",
                               yLabel = NULL, leg_pos = "right", angle_x = 0, text = TRUE, type = "count",
                               color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(suma = agg(aggregation, c)) %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(total = sum(suma)) %>%
    dplyr::mutate(pos = suma*9/10, percent = 100 * round(suma/total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  suma = ifelse(suma == 0, NA, suma)) %>%
    dplyr::filter(a %in% unique(.$a)[1:2])

  data_graph$suma <- ifelse(data_graph$a %in% unique(data_graph$a)[1], -data_graph$suma, data_graph$suma)
  data_graph$pos <- ifelse(data_graph$a %in% unique(data_graph$a)[1], -data_graph$pos, data_graph$pos)

  graph <- ggplot(data_graph, aes(x = b, y = suma, fill = a)) +
    geom_bar(data = subset(data_graph, a %in% unique(data_graph$a)[1]), stat = "identity")

  if(text == TRUE & type == 'count'){
    graph <- graph + geom_text(data = subset(data_graph, a %in% unique(data_graph$a)[1]), aes(y = pos, label = abs(round(suma,2))),
                                 check_overlap = TRUE, color = color_text)
  }else if(text == TRUE & type == 'percent'){
    graph <- graph + geom_text(data = subset(data_graph, a %in% unique(data_graph$a)[1]), aes(y = pos, label = paste(percent, "%", sep = "")),
                                 check_overlap = TRUE, color = color_text)
  }

  graph <- graph +
    geom_bar(data = subset(data_graph, a %in% unique(data_graph$a)[2]), stat = "identity", position = "identity")

  if(text == TRUE & type == 'count'){
    graph <- graph + geom_text(data = subset(data_graph, a %in% unique(data_graph$a)[2]), aes(y = pos, label = abs(round(suma,2))),
                               check_overlap = TRUE, color = color_text, position = "identity")
  }else if(text == TRUE & type == 'percent'){
    graph <- graph + geom_text(data = subset(data_graph, a %in% unique(data_graph$a)[2]), aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = "identity")
  }

  graph <- graph + theme_ds() +
    scale_fill_manual(values=getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel) +
    scale_y_continuous(labels = abs) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    coord_flip()

  graph
}

#' Grouped line + points by first variable
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
                                        fillLabel = NULL, leg_pos ="right", shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  graph <- ggplot(data, aes(x = as.factor(b), y = c, group = a)) + geom_point(aes(color = a), shape = shape_type) + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Grouped line by first variable
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
                                  fillLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  graph <- ggplot(data, aes(x = as.factor(b), y = c, group = a))  + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Sunburst
#' sunburst
#' @name gg_sunburst_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_sunburst_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                fillLabel = NULL, aggregation = "sum", ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  #angulos

  pred_ang <-  function(perc){
    angle = -1

    if(perc < 0.25) # 1st q [90,0]
      angle = 90 - (perc/0.25) * 90
    else if(perc < 0.5) # 2nd q [0, -90]
      angle = (perc-0.25) / 0.25 * -90
    else if(perc < 0.75) # 3rd q [90, 0]
      angle = 90 - ((perc-0.5) / 0.25 * 90)
    else if(perc < 1.00) # last q [0, -90]
      angle = ((perc -0.75)/0.25) * -90

    if(perc < 0.5) # 1st half [90, -90]
      angle = (180 - (perc/0.5) * 180) - 90
    else # 2nd half [90, -90]
      angle = (90 - ((perc - 0.5)/0.5) * 180)

    return(angle)
  }

  #primer nivel

  part1 <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(total1 = agg(aggregation, c))



  part1 <- part1 %>%
    mutate(running = cumsum(total1), pos = running - total1/2) %>%
    group_by(1:n()) %>%
    mutate(angle = pred_ang((pos)/total1))

  sunb0 <- ggplot(part1)
  sunb1 <- sunb0 +
    geom_bar(data = part1, aes(x=1, y = total1, fill = total1 ),stat = 'identity', color = 'white', position = 'stack') +
    geom_text(data = part1, aes(label=part1$a, x=1, y=pos, angle=angle), check_overlap = TRUE) +
    scale_fill_continuous(low = '#009EE3', high = '#E5007D')

  #segundo nivel

  cols_col <- data %>%
    dplyr::group_by(a,b) %>%
    dplyr::summarise(total1 = agg(aggregation, c))


  part2 <- cols_col %>%
    ungroup(a,b) %>%
    mutate(running = cumsum(total1), pos = running - total1/2) %>%
    group_by(1:n()) %>%
    mutate(angle = pred_ang((running - total1/2)/total1))

  sunb2 <- sunb1 +
    geom_bar(data = part2, aes(x=2, y = total1,  fill = total1),stat = 'identity', color = 'white', position = 'stack') +
    geom_text(data = part2, aes(label=part2$b, x=2, y=pos, angle=angle), check_overlap = TRUE)


  graph <- sunb2 + coord_polar('y') +  theme_ds_clean() + guides(fill = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)

  graph

}

