#' Bubble
#' Bubble
#' @name gg_bubble_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CaCa.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL, text = TRUE, color_text = "black", type = "count",
                             angle_x = 0, shape_type = 19,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4))  %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = count, color = "")) +
    geom_point(shape = shape_type)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_color_manual(values = getPalette()) +
    guides(size = FALSE, colour = FALSE)

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
#' Coloured Bubble
#' @name gg_bubble_coloured_x_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_x_CaCa.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, text = TRUE, color_text = "black", type = "count",
                                        angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4))  %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = count)) +
    geom_point(aes(color = a), shape = shape_type)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_color_manual(values = getPalette()) +
    guides(colour = FALSE, size = FALSE)

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

#' Bubble coloured by second variable
#' Coloured Bubble
#' @name gg_bubble_coloured_y_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_y_CaCa.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, text = TRUE, color_text = "black", type = "count",
                                        angle_x = 0,  shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4))  %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = count)) +
    geom_point(aes(color = b), shape = shape_type)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_color_manual(values = getPalette()) +
    guides(colour = FALSE, size = FALSE)

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

#' Vertical dot bar facet
#' Facet Vertical Dot Bar
#' @name gg_dot_bar_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                       angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  data_graph <- data_graph %>%
    mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = order, fill = factor(a))) + geom_dotplot(method="histodot") +
    scale_fill_manual(values = getPalette())
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab,  fill = flabel)
  graph <- graph + theme_ds() + scale_y_continuous(breaks = NULL) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos) + facet_wrap(~b)


  graph
}

#' Horizontal dot bar facet
#' Facet Horizontal Dot Bar
#' @name gg_dot_bar_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle ="", caption = "", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_dot_bar_ver_facet_CaCa.(data, titleLabel, subtitle, caption,  xLabel, yLabel, fillLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Pie facet
#' Facet Pie
#' @name gg_pie_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                               leg_pos="right", text = TRUE, type = 'count', color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = cumsum(c) - c/2,
                  percent = 100 * round(c / total, 4))  %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data=data_graph, aes(x = factor(1), weight = c,  fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = flabel) +
    guides(text = FALSE)
  graph <- graph +
    theme_ds_clean() +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~b)
  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = c, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = c, label = paste(percent, "%", sep = "")),
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
#' @name gg_donut_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                 width = 0.3, leg_pos="right", text = TRUE, type = 'count', color_text = "black", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = cumsum(c) - c/2,
                  percent = 100 * round(c / total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data=data_graph, aes(x = factor(1), weight = c, fill = a)) +
    geom_bar(width = width) +
    coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
    guides(text = FALSE)
  graph <- graph +
    theme_ds() +
    theme_ds_clean() +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~b)
  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = c, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = c, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  graph
}

#' Bullseye facet
#' Facet Bullseye
#' @name gg_bullseye_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
    guides(text = FALSE)
  graph <- graph +
    theme_ds() +
    theme_ds_clean() +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~b)

  graph
}

#' Vertical bar facet coloured by first variable
#' Facet Vertical coloured bars
#' @name gg_bar_coloured_x_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, leg_pos = "right", angle_x = 0,
                                              text = TRUE, type = 'count', color_text = "black", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data = data_graph, aes(x = a, y = c, fill = a)) + geom_bar(stat = "identity")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    guides(text = FALSE)
  graph <- graph +
    theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph + facet_wrap(~b)

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

#' Horizontal bar facet coloured by first variable
#' Facet Horizontal coloured Bars
#' @name gg_bar_coloured_x_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, leg_pos = "right", angle_x = 0,
                                              text = TRUE, type = 'count', color_text = "black", ...){

  graph <- gg_bar_coloured_x_ver_facet_CaCa.(data, titleLabel, subtitle, caption, xLabel,
                                             yLabel, leg_pos, angle_x, text, type, color_text, ...)

  graph <- graph + coord_flip()
  graph
}

#' Vertical bar facet coloured by second variable
#' Facet Vertical coloured bars
#' @name gg_bar_coloured_y_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_y_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, leg_pos = "right", angle_x = 0,
                                              text = TRUE, type = 'count', color_text = "black", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data = data_graph, aes(x = a, y = c, fill = b)) + geom_bar(stat = "identity")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph + facet_wrap(~b)

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

#' Horizontal bar facet coloured by second variable
#' Facet Horizontal coloured Bars
#' @name gg_bar_coloured_y_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_y_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, leg_pos = "right", angle_x = 0,
                                              text = TRUE, type = 'count', color_text = "black", ...){

  graph <- gg_bar_coloured_y_ver_facet_CaCa.(data, titleLabel, subtitle, caption, xLabel,
                                             yLabel, leg_pos, angle_x, text, type, color_text, ...)

  graph <- graph + coord_flip()
  graph
}

#' Vertical bar facet highlighting some parameter
#' Facet Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                      xLabel = NULL, yLabel = NULL,
                                                      parameter1 = NULL, parameter2 = NULL,
                                                      leg_pos = "right", angle_x = 0,
                                                      text = TRUE, type = "count", color_text = "black", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  parameters <- data %>% dplyr::group_by(a,b) %>% dplyr::summarise(count = n()) %>%
    dplyr::group_by(b) %>% dplyr::filter(count == max(count)) %>% dplyr::mutate(color = TRUE)
  p_a <-  parameter1 %||% parameters$a
  p_b <-  parameter2 %||% parameters$b

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::left_join(., parameters, by = c("a", "b", "count")) %>%
    dplyr::mutate(color = ifelse(is.na(color), FALSE, color)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = color)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(count,2)),
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

#' Horizontal bar facet highlighting some parameter
#' Facet Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                      xLabel = NULL, yLabel = NULL,
                                                      parameter1 = NULL, parameter2 = NULL,
                                                      leg_pos = "right", angle_x = 0,
                                                      text = TRUE, type = "count", color_text = "black", ...){

  graph <- gg_bar_coloured_parameter_ver_facet_CaCa.(data, titleLabel, subtitle, caption, xLabel,
                                                     yLabel, parameter1, parameter2, leg_pos, angle_x, text, type, color_text, ...)

  graph <- graph + coord_flip()
  graph
}

#' Vertical stacked bar
#' Stacked vertical Bar
#' @name gg_bar_stacked_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, leg_pos = "right", text = TRUE, type = "count", color_text = "black",
                                     angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::arrange(c) %>%
    dplyr::mutate(percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::arrange(c) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = ifelse(percent == 0, NA, percent))

  graph <- ggplot(data_graph, aes(x = reorder(a, c), y = c, fill=b)) + geom_bar(stat = "identity", position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
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

#' Horizontal stacked bar
#' Stacked horizontal Bar
#' @name gg_bar_stacked_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_hor_CaCa. <- function(data, titleLabel = "", subtitle ="", caption = "", xLabel = NULL,
                                     yLabel = NULL, leg_pos = "right", text = TRUE, type = "count", color_text = "black",
                                     angle_x = 0,...){

  graph <- gg_bar_stacked_ver_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, text, type, color_text, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Ordered horizontal stacked bar
#' Ordered Stacked horizontal Bar
#' @name gg_bar_ordered_stacked_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_stacked_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel =  NULL, leg_pos = "right", text = TRUE, type = "count", color_text = "black",
                                             angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  # graph <- ggplot(data, aes(x=reorder(data$b, rep(1, length(data$b)), sum), fill = a)) +
  #           geom_bar()

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::mutate(percent = 100 * round(c / sum(c), 4))

  data_order <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = n()) %>%
    dplyr::group_by(a) %>% dplyr::summarise(sum = sum(count))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = ifelse(percent == 0, NA, percent))

  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data_graph, aes(x = reorder(a, c, sum), y = c, fill=b)) +
    geom_bar(stat = "identity", position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

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

#' Ordered vertical stacked bar
#' Ordered Stacked Vertical Bar
#' @name gg_bar_ordered_stacked_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_stacked_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel =  NULL, leg_pos = "right", text = TRUE, type = "count", color_text = "black",
                                             angle_x = 0,...){

  graph <- gg_bar_ordered_stacked_hor_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, text, type, color_text, angle_x)

  graph <- graph + coord_flip()

  graph
}

#' Horizontal stacked dot bar
#' Stacked horizontal dot Bar
#' @name gg_stacked_dot_bar_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_dot_bar_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  graph <- ggplot(data = data, aes(a, fill = factor(b))) +
    geom_dotplot(stackgroups = TRUE, binpositions = "all") +
    scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_y_continuous(breaks = NULL) +
    theme(legend.position=leg_pos)

  graph
}

#' Vertical stacked dot bar
#' Stacked vertical dot Bar
#' @name gg_stacked_dot_bar_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_dot_bar_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, leg_pos = "right", angle_x =0, ...){

  graph <- gg_stacked_dot_bar_hor_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x)

  graph <- graph + coord_flip()

  graph
}

#' Horizontal grouped bar
#' Unstacked Coloured horizontal Bar
#' @name gg_bar_grouped_coloured_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped_coloured_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, leg_pos = "right", text = TRUE, type = "count", color_text = "black",
                                              angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    tidyr::spread(b, count, fill = 0) %>%
    tidyr::gather(b, count, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "count")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent))

  graph <- ggplot(data, aes(a, weight=count, fill=b)) + geom_bar(position = "dodge")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    guides(text = FALSE)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(count,2)),
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

#' Vertical grouped bar
#' Unstacked Coloured vertical Bar
#' @name gg_bar_grouped_coloured_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped_coloured_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, leg_pos = "right",text = TRUE, type = "count", color_text = "black",
                                              angle_x = 0,...){
  graph <- gg_bar_grouped_coloured_hor_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos,
                                             text, type, color_text, angle_x, ... )

  graph <- graph + coord_flip()

  graph
}

#' Horizontal line facet
#' Horizontal Line
#' @name gg_line_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=b, colour = "")) + geom_line() +
    facet_wrap(~b)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + guides(color = FALSE)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  graph
}

#' Vertical line facet
#' Vertical Line
#' @name gg_line_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = NULL, angle_x = 0, ...){

  graph <- gg_line_hor_facet_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, angle_x = 0, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal line + point facet
#' Horizontal Line Point
#' @name gg_line_point_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=b, colour = "")) + geom_line() +
    geom_point(shape = shape_type) + facet_wrap(~b) + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    guides(color = FALSE)
  graph <- graph +
    scale_color_manual(values = getPalette())

  graph
}

#' Vertical line + point facet
#' Vertical Line Point
#' @name gg_line_point_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, angle_x = 0, shape_point = 19, ...){

  graph <- gg_line_point_hor_facet_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, angle_x, shape_point, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked bar
#' Stacked 100pct vertical Bar
#' @name gg_bar_stacked_100_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, leg_pos = "right",text = TRUE, type = "count", color_text = "black",
                                         angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Porcentaje"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::mutate(percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = ifelse(percent == 0, NA, percent))

  graph <- ggplot(data, aes(a, y = c, fill=b)) +
    geom_bar(stat="identity", position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette()) +
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

#' Horizontal 100% stacked bar
#' Stacked 100pct horizontal Bar
#' @name gg_bar_stacked_100_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, leg_pos = "right",text = TRUE, type = "count", color_text = "black",
                                         angle_x = 0,...){


  graph <- gg_bar_stacked_100_ver_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, text, type, color_text, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal stacked area
#' Stacked horizontal Area
#' @name gg_area_stacked_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=n()) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)
  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=count, group=b)) + geom_area(aes(fill = b), position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  graph
}

#' Vertical stacked area
#' Stacked vertical Area
#' @name gg_area_stacked_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_area_stacked_hor_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal 100% stacked area
#' Stacked horizontal Area 100pct
#' @name gg_area_stacked_100_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos = "right", angle_x = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Porcentaje"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=n()) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)
  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=count, group=b)) + geom_area(aes(fill = b), position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical 100% stacked area
#' Stacked vertical Area 100pct
#' @name gg_area_stacked_100_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_area_stacked_100_hor_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical marimekko
#' Vertical Marimekko
#' @name gg_marimekko_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_marimekko_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, leg_pos = "right", angle_x = 0, ...){
  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

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
    labs(x = xLabel, y = yLabel, title = titleLabel, subtitle = subtitle, caption = caption)
  graph <- graph + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph + scale_fill_manual(values = getPalette()) +
    theme(legend.position = leg_pos)

  graph
}

#' Horizontal marimekko
#' Horizontal Marimekko
#' @name gg_marimekko_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_marimekko_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, leg_pos = "right", angle_x = 0, ...){
  graph <- gg_marimekko_ver_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()
  graph
}

#' Polar stacked bar
#' Stacked Polar bar
#' @name gg_bar_polar_stacked_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_stacked_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", width = 0.95,
                                       fillLabel = NULL, text = TRUE, type = 'count', color_text = "black", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <-fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data = data, aes(x = a, y = c, fill = b)) +
    geom_bar(width = width, position = "stack", stat = "identity") +
    coord_polar()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab)
  graph <- graph + theme_ds() + #theme_ds_clean() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  return(graph)
}

#' Polar 100% stacked bar
#' Stacked Polar  100pct
#' @name gg_bar_polar_stacked_100_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_stacked_100_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                           leg_pos = "right", width = 0.95, text = TRUE, color_text = "black",
                                           type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
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
    scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
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



#' Circular bar facet
#' Circular Bar
#' @name gg_bar_circular_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                        leg_pos="right", width = 0.85,
                                        text = TRUE, type = "count", color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::mutate(pos = count*9.7/10,
                  percent = 100 * round(count/total, 4),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data, aes(x = a, y = count , fill = a )) +
    geom_bar(width = width, stat="identity") +
    coord_polar(theta = "y")

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() +
    theme_ds_clean()

  graph <- graph + theme(legend.position=leg_pos) + facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(count,2)),
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

#' Treemap coloured by first variable
#' Treemap fill first Ca
#' @name gg_treemap_x_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                               text = "TRUE", color_text = "black",
                               leg_pos = "right", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

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
#' Treemap fill second Ca
#' @name gg_treemap_y_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "",
                               text = TRUE, color_text = "black",
                               leg_pos = "right", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 0, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}
