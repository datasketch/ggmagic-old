#' Vertical grouped bar by second variable
#' vertical unstacked bargraph
#' @name gg_bar_grouped_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                       xLabel = NULL, yLabel = NULL, leg_pos = "right", aggregation = "sum",
                                       text = TRUE, color_text = "black", type = "count", angle_x = 0,...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data, aes(x=a, y=c, fill=b)) +
    geom_bar(stat="identity", position = "dodge")
  graph <- graph +  theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) + guides(text = FALSE)
  graph <- graph + scale_fill_manual(values = getPalette())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_dodge(width=1)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_dodge(width=1)))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal grouped bar by second variable
#' horizontal bar graph
#' @name gg_bar_grouped_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                       xLabel = NULL, yLabel = NULL,
                                       leg_pos = "right",
                                       aggregation = "sum", text = TRUE, color_text = "black", type = "count",
                                       angle_x = 0,...){

  graph <- gg_bar_grouped_ver_CaCaNu.(data, titleLabel, subtitle, caption, fillLabel,
                                      xLabel, yLabel, leg_pos,
                                      aggregation, text, color_text, type, angle_x, ...)

  graph + coord_flip()

}

#' Vertical grouped bar by first variable
#' Barras grouped
#' @name gg_bar_grouped2_ver_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped2_ver_CaCaNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_grouped_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}

#' Horizontal grouped bar by first variable
#' Barras grouped
#' @name gg_bar_grouped2_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped2_hor_CaCaNu. <- function(data, ...){
  graph <- gg_bar_grouped2_ver_CaCaNu.(data,...) +
    coord_flip()
  graph
}


#' Vertical stacked bar by second variable
#' vertical stacked bar graph
#' @name gg_bar_stacked_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                       aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                       angle_x = 0,...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::mutate(percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data, aes(a, y = c, fill=b)) + geom_bar(stat="identity", position = "stack")
  graph <- graph + theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph  + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

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

#' Horizontal stacked bar by second variable
#' horizontal stacked bar graph
#' @name gg_bar_stacked_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                       aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                       angle_x = 0, ...){
  graph <- gg_bar_stacked_ver_CaCaNu.(data, titleLabel, subtitle, caption,
                                      xLabel, yLabel, fillLabel, leg_pos,
                                      aggregation, text, type, color_text, angle_x, ...)
  graph + coord_flip()
}


#' Vertical stacked bar by first variable
#' Barras grouped
#' @name gg_bar_stacked2_ver_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked2_ver_CaCaNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_stacked_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}

#' Horizontal stacked bar by first variable
#' Barras grouped
#' Tiene múltiples líneas
#' @name gg_bar_stacked2_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked2_hor_CaCaNu. <- function(data, ...){
  graph <- gg_bar_stacked2_ver_CaCaNu.(data,...) +
    coord_flip()
  graph
}

#' Vertical 100% stacked bar by second variable
#' 100 vertical stacked bar graph
#' @name gg_bar_stacked_100_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                           aggregation = "sum", text = TRUE, color_text = "black", type = "count",
                                           angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste("%", aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  data <- data %>% filter(!is.na(a),!is.na(b))

  graph <- ggplot(data, aes(a, y = c, fill=b)) +
    geom_bar(stat="identity", position = "fill") + theme_ds()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = percent/100, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = percent/100, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal 100% stacked bar by second variable
#' 100 horizontal stacked bar graph
#' @name gg_bar_stacked_100_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                           aggregation = "sum", text = TRUE, color_text = "black", type = "count",
                                           angle_x = 0, ...){


  graph <- gg_bar_stacked_100_ver_CaCaNu.(data, titleLabel, subtitle, caption,
                                          xLabel, yLabel, fillLabel, leg_pos,
                                          aggregation, text, color_text, type, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked bar by first variable
#' Barras grouped
#' @name gg_bar_stacked2_100_ver_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked2_100_ver_CaCaNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_stacked_100_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}

#' Horizontal 100% stacked bar by first variable
#' Barras grouped
#' @name gg_bar_stacked2_100_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked2_100_hor_CaCaNu. <- function(data, ...){
  graph <- gg_bar_stacked2_100_ver_CaCaNu.(data,...) +
    coord_flip()
  graph
}

#' Vertical facet bar
#' Facet vertical bargraph
#' @name gg_bar_facet_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                     xLabel = NULL,
                                     yLabel = NULL, leg_pos = "right", angle_x = 0,
                                     aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
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

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = "")) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    facet_wrap(~b) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  return(graph)
}


#' Horizontal facet bar
#' Horizontal bar graph
#' @name gg_bar_facet_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                     xLabel = NULL, yLabel = NULL, leg_pos = "right",
                                     angle_x = 0, aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  graph <- gg_bar_facet_ver_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, aggregation,
                                    text, color_text, type, ...)

  graph <- graph + coord_flip()

  graph

}

#' Vertical facet bar coloured by first variable
#' vertical bar
#' @name gg_bar_coloured_ver_x_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_ver_x_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                                aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
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

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = a)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme_ds() +
    #theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}


#' Horizontal facet bar coloured by first variable
#' horizontal bar
#' @name gg_bar_coloured_hor_x_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_hor_x_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                                aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  graph <- gg_bar_coloured_ver_x_facet_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos,
                                               angle_x, aggregation, text, color_text, type, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical facet bar coloured by second variable
#' vertical bar
#' @name gg_bar_coloured_ver_y_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_ver_y_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                                aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
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

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = b)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) + theme_ds() +
    #theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }

  graph
}


#' Horizontal facet bar coloured by second variable
#' horizontal bar
#' @name gg_bar_coloured_hor_y_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_hor_y_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                                aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  graph <- gg_bar_coloured_ver_y_facet_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x,
                                               aggregation, text, color_text, type, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical facet bar density by first numeric variable
#' Facet coloured vertical bar
#' @name gg_bar_coloured_ver_z_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_ver_z_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, fillLabel = NULL, reverse = FALSE, leg_pos = "right", angle_x = 0,
                                                aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% paste(aggregation, nms[3], sep = " ")
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

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = suma)) +
    geom_bar(stat = "identity") + theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) + facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal facet bar density by first numeric variable
#' Facet Coloured horizontal bar
#' @name gg_bar_coloured_hor_z_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_hor_z_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, fillLabel = NULL, reverse = FALSE, leg_pos = "right", angle_x = 0,
                                                aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  graph <- gg_bar_coloured_ver_z_facet_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, reverse, leg_pos,
                                               angle_x, aggregation, text, color_text, type, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical facet bar coloured highliting some parameter
#' Facet Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_ver_facet_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_ver_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                        xLabel = NULL, yLabel = NULL,
                                                        parameter1 = NULL, parameter2 = NULL,
                                                        leg_pos = "right", angle_x = 0, aggregation = "sum",
                                                        text = TRUE, type = "count", color_text = "black", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  parameters <- data %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>% dplyr::filter(c == max(c)) %>% dplyr::mutate(color = TRUE)
  p_a <-  parameter1 %||% parameters$a
  p_b <-  parameter2 %||% parameters$b

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = c*9/10, percent = 100 * round(c/total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c)) %>%
    dplyr::left_join(., parameters, by = c("a", "b", "c")) %>%
    dplyr::mutate(color = ifelse(is.na(color), FALSE, color),
                  percent = ifelse(percent == 0, NA, percent),
                  pos = ifelse(pos == 0, NA, pos),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data_graph, aes(x = a, y = c, fill = color)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(c,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal facet bar coloured highliting some parameter
#' Facet Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_hor_facet_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_hor_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                        xLabel = NULL, yLabel = NULL,
                                                        parameter1 = NULL, parameter2 = NULL,
                                                        leg_pos = "right", angle_x = 0, aggregation = "sum",
                                                        text = TRUE, type = "count", color_text = "black", ...){

  graph <- gg_bar_coloured_parameter_ver_facet_CaCaNu.(data, titleLabel, subtitle, caption, xLabel,
                                                       yLabel, parameter1, parameter2, leg_pos, angle_x, aggregation,
                                                       text, type, color_text)

  graph <- graph + coord_flip()
  graph
}

#' Circular bar facet
#' Circular Bar
#' @name gg_bar_circular_facet_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                          leg_pos="right", aggregation = "sum", width = 0.85,
                                          text = TRUE, type = "count", color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::arrange(desc(c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = c*9.7/10,
                  percent = 100 * round(c/total, 4),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data, aes(x = a, y = c , fill = a )) +
    geom_bar(width = width, stat="identity") +
    coord_polar(theta = "y")

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() +
    theme_ds_clean()

  graph <- graph + theme(legend.position=leg_pos) + facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(c,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Stacked polar bar
#' Stacked Polar Bar
#' @name gg_bar_stacked_polar_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_polar_CaCaNu. <- function(data, width = 0.95, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                         leg_pos= "right", aggregation = "sum", text = TRUE, color_text = "black",
                                         type = "count", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data = data, aes(x = a, y = c, fill = b)) +
    geom_bar(width = width, position = "stack", stat = "identity") +
    coord_polar()
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab)
  graph <- graph +
    theme_ds() +
    #theme_ds_clean() +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette()) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

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

#' Stacked 100% polar bar
#' Stacked Polar Bar 100
#' @name gg_bar_stacked_polar_100_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_polar_100_CaCaNu. <- function(data, width = 0.95, titleLabel = "", subtitle = "", caption = "",
                                             fillLabel = NULL, leg_pos= "right",
                                             aggregation = "sum", text = TRUE, color_text = "black",
                                             type = "count", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data = data, aes(x = a, y = c, fill = b)) +
    geom_bar(width = width, position = "fill", stat = "identity") +
    coord_polar() + theme_ds() +
    #theme_ds_clean() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)
  graph <- graph + theme(legend.position=leg_pos) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab, x = "", y = "") +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = percent/100, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = percent/100, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  graph
}
