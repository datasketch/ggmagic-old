#' Horizontal line
#' Horizontal Lines
#' @name gg_line_hor_YeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_YeNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                               xLabel = NULL, yLabel = NULL, angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a),!is.na(b))

  ggplot(data, aes(x= a,y=b,group=1)) +
    geom_line(stat = "identity", aes(colour = ""), show.legend = FALSE) +
    theme_ds() +
    scale_color_manual(values = getPalette()) +
    scale_y_continuous(labels = comma) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
}

#' Lollipop
#' lollipop. segment and point
#' gg_lollipop_YeNu.
#' @name gg_lollipop_YeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lollipop_YeNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                              xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = a, y = b)) +
    geom_segment(aes(xend=a, yend=0)) + geom_point(aes(color = ""), show.legend = FALSE, shape = shape_type) +
    theme_ds() +
    scale_color_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Waterfall
#' Waterfall
#' @name gg_waterfall_YeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_YeNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel =  NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot_waterfall(data,'a','b') + theme_ds() + theme(legend.position="none") +
           scale_color_manual(breaks = c("+",  "-", ""), values = getPalette()) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Vertical bar coloured by first variable
#' vertical coloured bar
#' @name gg_bar_coloured_x_ver_YeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_ver_YeNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,text = TRUE, type = 'percent', color_text = "black",
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation, b)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = factor(a))) + geom_bar(stat = "identity") +
           theme_ds() +
           scale_fill_manual(values = getPalette()) +
           theme(legend.position=leg_pos) +
           guides(fill = FALSE) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), color = color_text, position = position_dodge(0.9), check_overlap = TRUE))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), color = color_text, position = position_dodge(0.9), check_overlap = TRUE))
    }else{
      graph
    }
  }

}

#' Horizontal bar coloured by first variable
#' Horizontal coloured bar
#' @name gg_bar_coloured_x_hor_YeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_hor_YeNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,text = TRUE, type = 'percent', color_text = "black",
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0, ...){

  graph <- gg_bar_coloured_x_ver_YeNu.(data, titleLabel, subtitle, caption, xLabel, text, type, color_text, yLabel, fillLabel, leg_pos, aggregation, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical bar density by first numeric variable
#' vertical coloured bar
#' @name gg_bar_density_y_ver_YeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_density_y_ver_YeNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,text = TRUE, type = 'percent', color_text = "black",
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0,
                                      reverse = FALSE, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation, b)) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                  pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = count)) + geom_bar(stat = "identity", aes(fill = count)) +
    theme_ds()

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph +
    theme(legend.position=leg_pos) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), color = color_text, position = position_dodge(0.9), check_overlap = TRUE))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), color = color_text, position = position_dodge(0.9), check_overlap = TRUE))
    }else{
      graph
    }
  }

}

#' Horizontal bar density by first numeric variable
#' Horizontal coloured bar
#' @name gg_bar_density_y_hor_YeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_density_y_hor_YeNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,text = TRUE, type = 'percent', color_text = "black",
                                      yLabel = NULL, fillLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0,
                                      reverse = FALSE, ...){

  graph <- gg_bar_density_y_ver_YeNu.(data, titleLabel, subtitle, caption, xLabel, text, type, color_text, yLabel, fillLabel, leg_pos, aggregation, angle_x, reverse, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical bar
#' vertical bar
#' @name gg_bar_ver_YeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ver_YeNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,text = TRUE, type = 'count', color_text = "black",
                                       yLabel = NULL, leg_pos = "right", angle_x = 0, aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation, b)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = count)) + geom_bar(aes(fill = ""), show.legend = FALSE, stat = "identity") +
    theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    guides(fill = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), color = color_text, position = position_dodge(0.9), vjust = 0))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), color = color_text, position = position_dodge(0.9), vjust = 0))
    }else{
      graph
    }
  }

}

#' Horizontal bar
#' horizontal bar
#' @name gg_bar_hor_YeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_hor_YeNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, text = TRUE, type = 'count', color_text = "black",
                            yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_ver_YeNu.(data, titleLabel, subtitle, caption, xLabel, text, type, color_text, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical area
#' Area
#' @name gg_area_YeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_YeNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = as.character(a), y = b, group=1)) +
           geom_area(aes(fill = ""), show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) + theme_ds() +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizon
#' Horizon
#' @name gg_horizon_YeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_horizon_YeNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                           yLabel =  NULL, leg_pos = "right",reverse = FALSE, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot_horizon(data, 'a', 'b')
  graph <- graph + theme_ds() +
    labs(tittle = titleLabel, subtitle = subtitle, caption =caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
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
