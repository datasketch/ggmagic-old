#' Pie
#' Pie
#' @name gg_pie_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                         text = TRUE, type = 'count', color_text = "black",
                         leg_pos="right", aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data=data_graph, aes(x = factor(1), weight = count, fill = a)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab)
  graph <- graph + guides(text = FALSE)
  graph <- graph + theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
}

#' Vertical bar coloured by first variable
#' vertical bar
#' @name gg_bar_coloured_x_ver_CaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_ver_CaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL, line_mean = FALSE, text = TRUE,
                                       type = 'count', leg_pos = "right", aggregation = "sum", angle_x = 0, color_text = "black", ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[2], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                  pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = factor(a))) +
    geom_bar(stat = "identity") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette()) +
    guides(fill = FALSE) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(line_mean){
    graph <- graph + geom_hline(aes(yintercept= mean(data_graph$count)), linetype="dashed")
  }else{
    graph <- graph
  }

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }


}

#' Horizontal bar coloured by first variable
#' horizontal bar
#' @name gg_bar_coloured_x_hor_CaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_hor_CaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL, line_mean = FALSE, text = TRUE,
                                       type = 'count', leg_pos = "right", aggregation = "sum", angle_x = 0, color_text = "black", ...){


  graph <- gg_bar_coloured_x_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, line_mean, text, type, leg_pos, aggregation, angle_x, color_text) +
    coord_flip()
  graph
}

#' Vertical bar density by first numeric variable
#' vertical bar
#' @name gg_bar_coloured_y_ver_CaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_y_ver_CaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                       fillLabel = NULL, reverse = FALSE, line_mean = FALSE,
                                       text = TRUE, type = 'count', color_text = "black",
                                       leg_pos = "right", aggregation = "sum", angle_x = 0, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[2], sep = " ")
  clab <- fillLabel %||% paste(aggregation, nms[2], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                  pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = count)) +
    geom_bar(stat = "identity") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  if(line_mean){
    graph <- graph + geom_hline(aes(yintercept= mean(data_graph$count)), linetype="dashed")
  }else{
    graph <- graph
  }

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
}

#' Horizontal bar density by first numeric variable
#' horizontal bar
#' @name gg_bar_coloured_y_hor_CaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_y_hor_CaNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                       fillLabel = NULL, reverse = FALSE, line_mean = FALSE,
                                       text = TRUE, type = "count", color_text = "black",
                                       leg_pos = "right", aggregation = "sum", angle_x = 0, ...){

  graph <- gg_bar_coloured_y_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel,
                                       reverse, line_mean, text, type, color_text,
                                       leg_pos, aggregation, angle_x)
  graph + coord_flip()
}

#' Vertical bar highlighting some parameter
#' Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, order = FALSE, parameter = NULL,line_mean = FALSE, text = TRUE, type = 'count',
                                                color_text = "black", leg_pos = "right", aggregation = "sum", angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[2], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(pos = count*8/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  p <-  parameter %||% (data_graph %>% dplyr::filter(count == max(count)))$a

  if(order){
    graph <- ggplot(data_graph, aes( x = reorder(a, count), y = count))
  }else{
    graph <- ggplot(data_graph, aes( x = a, y = count))
  }


  graph <- graph + geom_bar(stat="identity", aes(fill = a %in% p ))

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph +
    guides(fill = FALSE) +
    theme_ds() +
    theme(legend.position = leg_pos) +
    scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(line_mean){
    graph <- graph + geom_hline(aes(yintercept= mean(data_graph$count)), linetype="dashed")
  }else{
    graph <- graph
  }

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

#' Horizontal bar highlighting some parameter
#' Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, order = FALSE, parameter = NULL,line_mean = FALSE, text = TRUE, type = 'count',
                                                color_text = "black", leg_pos = "right", aggregation = "sum", angle_x = 0, ...){

  graph <- gg_bar_coloured_parameter_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel,
                                               yLabel,order, parameter, line_mean, text, type, color_text, leg_pos = "right", aggregation, angle_x, ...)
  graph <- graph + coord_flip()
  graph
}

#' Bubble
#' Bubble
#' @name gg_bubble_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "",
                             xLabel = NULL, text = TRUE, type = 'count',
                             color_text = "black", leg_pos = "right", aggregation = "sum", angle_x = 0,
                             shape_type = 19,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = 0, size = count, color = ""))
  graph <- graph + geom_point(show.legend = FALSE, shape = shape_type)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = "") +
    scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = 0, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -1.3))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = 0, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -1.3))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured by first variable
#' Coloured Bubble
#' @name gg_bubble_coloured_x_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_x_CaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        text = TRUE, type = 'count', color_text = "black", leg_pos = "right",
                                        aggregation = "sum", angle_x = 0, shape_type = 19,...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(x = a, y = 0, size = count))
  graph <- graph + geom_point(aes(color = a), shape = shape_type)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = "")

  graph <- graph + scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = 0, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -1.3))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = 0, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -1.3))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured by first numeric variable
#' Coloured Bubble
#' @name gg_bubble_density_y_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_density_y_CaNu.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, fillLabel = NULL,
                                       text = TRUE, type = 'count', color_text = "black", leg_pos = "right",
                                       aggregation = "sum", angle_x = 0, shape_type = 19, reverse = FALSE, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% paste(aggregation, nms[2], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(x = a, y = 0, size = count))
  graph <- graph + geom_point(aes(color = count), shape = shape_type)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = "", color = clab) + guides(size = FALSE)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank()) +
    theme(legend.position=leg_pos)


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = 0, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -1.3)) + guides(text = FALSE, size = FALSE)
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = 0, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -1.3)) + guides(text = FALSE, size = FALSE)
    }else{
      return(graph + guides(text = FALSE, size = FALSE))
    }
  }
  return(graph + guides(text = FALSE, size = FALSE))
}

#' Polar bar
#' Polar Bar
#' @name gg_bar_polar_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_CaNu. <- function(data, width = 0.95, titleLabel = "", subtitle = "", aggregation = "sum",
                               caption = "", fillLabel = NULL, leg_pos = "right", text = TRUE, color_text = "black",
                               type = "count", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(count) %>%
    dplyr::mutate(pos = count*8/10,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(x = a, weight = count, fill = a)) + geom_bar(width = width) +
    coord_polar() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

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

#' Circular bar
#' Circular Bar
#' @name gg_bar_circular_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                  leg_pos="right", width = 0.85, aggregation = 'sum',  text = TRUE,
                                  type = "count", color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b))  %>%
    dplyr::arrange(count) %>%
    dplyr::mutate(pos = count*9.7/10,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = reorder(a,count), y = count , fill = a )) +
    geom_bar(width = width, stat="identity") + coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

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


#' Vertical stacked histogram
#' Stacked Vertical Histogram
#' @name gg_hist_stacked_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_stacked_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_histogram(aes(fill = a), binwidth = 10) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme_ds() + theme(legend.position=leg_pos) + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}

#' Density distribution
#' Coloured Density Distribution
#' @name gg_density_multi_dist_coloured_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_density_multi_dist_coloured_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                 xLabel = NULL, yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                                 angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab) +
    theme_ds() + theme(legend.position=leg_pos) + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Filled density distribution
#' Filled Density Distribution
#' @name gg_area_multi_density_dist_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_multi_density_dist_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = NULL, fillLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(fill = a)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical distribution facet
#' Facet Vertical Dist
#' @name gg_dist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a), show.legend = FALSE) + facet_wrap(~a) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds()
  graph <- graph + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal distribution facet
#' Facet Horizontal Dist
#' @name gg_dist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_dist_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram + mean facet
#' Facet Vertical Histogram + Mean
#' @name gg_hist_ver_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_ver_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, leg_pos='right', angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(prom = mean(b, na.rm = TRUE))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
    geom_vline(data = data_graph, aes(xintercept = prom, color = ""), linetype = "dotted", size = 1, show.legend = FALSE) +
    facet_wrap(~a)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2]) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal histogram + mean facet
#' Facet Horizontal Histogram + Mean
#' @name gg_hist_hor_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_hor_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_hist_ver_mean_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram facet
#' Facet Vertical Histogram
#' @name gg_hist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = NULL, leg_pos='right', angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) + facet_wrap(~a) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal histogram facet
#' Facet Horizontal Histogram
#' @name gg_hist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_hist_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram + distribution facet
#' Facet Vertical Histogram + Dist
#' @name gg_dist_hist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    theme_ds() + theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal histogram + distribution facet
#' Facet Horizontal Histogram + Dist
#' @name gg_dist_hist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_dist_hist_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram + distribution + mean facet
#' Facet Vertical Histogram + Dist + Mean
#' @name gg_dist_hist_ver_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_ver_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(prom = mean(b, na.rm = TRUE))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    geom_vline(data = data_graph, aes(xintercept = prom, color = "*"), linetype = "dotted", size = 1, show.legend = FALSE) +
    theme_ds() + theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal histogram + distribution + mean facet
#' Facet Horizontal Histogram + Dist + Mean
#' @name gg_dist_hist_hor_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_hor_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_dist_hist_ver_mean_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical density dot + distribution facet
#' Facet Vertical Dot Dist
#' @name gg_dot_dist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a), show.legend = FALSE) +
    geom_point(aes(y = 0), color = "#D55E00", alpha = alpha, show.legend = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph + facet_wrap(~a)

  graph
}

#' Horizontal density dot + distribution facet
#' Facet Horizontal Dot Dist
#' @name gg_dot_dist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  graph <- gg_dot_dist_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical density dot + histogram facet
#' Facet Vertical Dot Histogram
#' @name gg_dot_hist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos='right', alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
    geom_point(aes(y=0), alpha = alpha, color = "#D55E00") +
    facet_wrap(~a) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal density dot + histogram facet
#' Facet Horizontal Histogram + Dot
#' @name gg_dot_hist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  graph <- gg_dot_hist_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical density dot + histogram + mean facet
#' Facet Vertical Histogram + Mean + Dot
#' @name gg_dot_hist_ver_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_ver_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = NULL, leg_pos='right', alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(prom = mean(b, na.rm = TRUE))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
    facet_wrap(~a) +
    geom_vline(data = data_graph, aes(xintercept = prom, color = ""), linetype = "dotted", size = 1, show.legend = FALSE) +
    geom_point(aes(y = 0),  alpha = alpha, color = "#D55E00")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2]) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal density dot + histogram + mean facet
#' Facet Horizontal Histogram + Mean + Dot
#' @name gg_dot_hist_hor_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_hor_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  graph <- gg_dot_hist_ver_mean_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical density dot + histogram + distribution facet
#' Facet Vertical Histogram + Dist + Dot
#' @name gg_dot_dist_hist_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color=""), show.legend = FALSE) +
    geom_point(aes(y = 0), alpha = alpha, color = "#D55E00") +
    theme_ds() +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette())
  graph <- graph + facet_wrap(~a) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  graph
}

#' Horizontal density dot + histogram + distribution facet
#' Facet Horizontal Histogram + Dist + Dot
#' @name gg_dot_dist_hist_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  graph <- gg_dot_dist_hist_ver_facet_CaNu.(data, titleLabel,subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}


#' Vertical density dot + histogram + distribution + mean facet
#' Facet Vertical Histogram + Dist + Mean + Dot
#' @name gg_dot_dist_hist_ver_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_ver_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                                  leg_pos = "right", alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(prom = mean(b, na.rm = TRUE))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    geom_vline(data = data_graph, aes(xintercept = prom, colour = "*"), linetype = "dotted", size = 1, show.legend = FALSE) +
    geom_point(aes(y=0), alpha = alpha, color = "#D55E00") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme_ds() + scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette())
  graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal density dot + histogram + distribution + mean facet
#' Facet Horizontal Histogram + Dist + Mean + Dot
#' @name gg_dot_dist_hist_hor_mean_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_hor_mean_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  graph <- gg_dot_dist_hist_ver_mean_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Scatter facet
#' Facet Point
#' @name gg_point_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  graph <- ggplot(data_count, aes(x=xorder, y=b)) + geom_point(shape = shape_type, aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + facet_wrap(~a) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Line + point facet
#' Facet Line Point
#' @name gg_line_point_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  graph <- ggplot(data_count, aes(x = xorder, y = b)) + geom_point(shape = shape_type, aes(color = ""), show.legend = FALSE) +
    geom_line(aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + facet_wrap(~a) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Line facet
#' Facet Line
#' @name gg_line_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  graph <- ggplot(data_count, aes(x=xorder, y=b)) + geom_line(aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + facet_wrap(~a) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical area facet
#' Facet Vertical Area
#' @name gg_area_ver_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_ver_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  graph <- ggplot(data = data_count, aes(x=xorder, y=b, group=a)) + geom_area(aes(fill = ""), show.legend = FALSE) +
    scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + facet_wrap(~a) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Horizontal area facet
#' Facet Horizontal Area
#' @name gg_area_hor_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_hor_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = NULL, angle_x = 0, ...){

  graph <- gg_area_ver_facet_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked area
#' Stacked Vertical Area 100
#' @name gg_area_stacked_100_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% paste("%", nms[2])
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  data_graph <- data_count %>% arrange(xorder) %>%
    tidyr::spread(xorder, b) %>% tidyr::gather(xorder, b, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$xorder <- as.numeric(data_graph$xorder)

  graph <- ggplot(data = data_graph,
                  aes(x=xorder, y=b, group=a)) +
    geom_area(aes(fill = a), position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}

#' Horizontal 100% stacked area
#' Stacked Horizontal Area 100
#' @name gg_area_stacked_100_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_area_stacked_100_ver_CaNu.(data, titleLabel,subtitle, caption, xLabel, yLabel, fillLabel = NULL, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical stacked area
#' Stacked Vertical Area
#' @name gg_area_stacked_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                      fillLabel = NULL, leg_pos = "right", text = TRUE, color_text = "black",
                                      type = "count", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a)  %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  data_graph <- data_count %>% arrange(xorder) %>%
    tidyr::spread(xorder, b) %>% tidyr::gather(xorder, b, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$xorder <- as.numeric(data_graph$xorder)

  graph <- ggplot(data = data_graph,
                  aes(x=xorder, y=b, group=a)) +
    geom_area(aes(fill = a), position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}

#' Horizontal stacked area
#' Stacked Horizontal Area
#' @name gg_area_stacked_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, fillLabel = NULL, leg_pos = "right", text = TRUE, color_text = "black", type = "count", angle_x = 0, ...){

  graph <- gg_area_stacked_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, text, color_text, type, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Grouped scatter
#' Grouped Color Point
#' @name gg_point_grouped_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_grouped_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, fillLabel = NULL, leg_pos="right", shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  graph <- ggplot(data_count, aes(x=xorder, y=b)) + geom_point(aes(color = a), shape = shape_type) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
  graph <- graph + theme_ds() + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Grouped line + point
#' Grouped Line Color Point
#' @name gg_line_point_multi_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_multi_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                      fillLabel = NULL, leg_pos="right", shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))

  # data$xorder <- count

  graph <- ggplot(data_count, aes(x=xorder, y=b)) + geom_point(aes(color = a), shape = shape_type) + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Grouped line
#' Grouped Line Coloured
#' @name gg_line_multi_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_multi_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                fillLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  graph <- ggplot(data_count, aes(x=xorder, y=b)) + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Trend line facet
#' Facet Trend Line
#' @name gg_point_trend_line_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_trend_line_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, shape_type = 19, alpha = 0.3, se = FALSE, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a)  %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  graph <- ggplot(data_count, aes(x = xorder, y = b)) + geom_point(shape = shape_type, aes(color = ""), show.legend = FALSE) +
    geom_smooth(method=lm, se=se, aes(colour = "*", fill = "*"), alpha = alpha, show.legend = FALSE) + facet_wrap(~a) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}



#' Trend ribbon facet
#' Facet Trend ribbon
#' @name gg_trend_ribbon_facet_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_trend_ribbon_facet_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, shape_type = 19, alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a)  %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  graph <- ggplot(data_count, aes(x = xorder, y = b)) + geom_point(aes(color = ""), shape = shape_type, show.legend = FALSE) +
    geom_smooth(aes(colour="*", fill = "*"), alpha = alpha, show.legend = FALSE) + facet_wrap(~a) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#Width debe de ser un parámetro.  0 < width < 1.

#' Donut
#' dount
#' @name gg_donut_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                           width = 0.3, leg_pos="right", aggregation = 'sum',
                           text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation, b))  %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(x = factor(1), fill = a, y = count)) +
    geom_bar(stat = "identity", width = width) + coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() +
    theme_ds_clean()
  graph <- graph + theme(legend.position = leg_pos)

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

#' Vertical dot bar
#' Vertical Dot Bar
#' @name gg_dot_bar_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_ver_CaNu.<- function(data, titleLabel = "", subtitle = "", caption = "",
                                xLabel = NULL, yLabel = NULL, leg_pos="right", angle_x = 0, aggregation ="sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[2])
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(suma = agg(aggregation, b))

  data_graph <- data_graph %>%
    dplyr::mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = a, fill = a)) + geom_dotplot(method="histodot", show.legend = FALSE)

  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)
  graph <- graph + scale_y_continuous(breaks = NULL) +
    theme(legend.position=leg_pos) + theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal dot bar
#' Horizontal Dot Bar
#' @name gg_dot_bar_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, leg_pos = "right", angle_x = 0, aggregation = "sum", ...){

  graph <- gg_dot_bar_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, aggregation, ...)
  graph <- graph + coord_flip()

  graph
}


#' Bullseye
#' Bullseye
#' @name gg_bullseye_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                              leg_pos="right", aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation, b)) %>% dplyr::arrange(count)
  graph <- ggplot(data = data_graph,
                  aes(x = factor(1), fill = a, y = reorder(a, count))) +
    geom_bar(stat = "identity", width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  graph
}


#' Vertical stacked bar
#' Single Vertical Stacked Bar
#' @name gg_bar_single_stacked_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_single_stacked_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                            fillLabel = NULL, leg_pos="right", width = 0.3, aggregation = 'sum',
                                            text = TRUE, type = "count", color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = cumsum(count) - (count/2),
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data=data_graph, aes(x = factor(1), fill = a, weight = count)) +
    geom_bar(width = width)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}


#' Horizontal stacked bar
#' Single Horizontal Stacked Bar
#' @name gg_bar_single_stacked_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_single_stacked_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                            fillLabel = NULL, leg_pos="right", width = 0.3, aggregation = 'sum',
                                            text = TRUE, type = "count", color_text = "black", angle_x = 0, ...){

  graph <- gg_bar_single_stacked_ver_CaNu.(data, titleLabel, subtitle, caption,  fillLabel, leg_pos, width, aggregation,
                                           text, type, color_text, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}


#' Gauge
#' Gauge
#' @name gg_gauge_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_gauge_CaNu. <- function(data, ...){

  gg.gauge <- function(pos, breaks=c(0,30,70,100)) {
    require(ggplot2)
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
      th.start <- pi*(1-a/100)
      th.end   <- pi*(1-b/100)
      th       <- seq(th.start,th.end,length=100)
      x        <- c(r1*cos(th),rev(r2*cos(th)))
      y        <- c(r1*sin(th),rev(r2*sin(th)))
      return(data.frame(x,y))
    }
    graph <- ggplot()+
      geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill = getPalette()[1])+
      geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill = getPalette()[2])+
      geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill = getPalette()[3])+
      geom_polygon(data=get.poly(as.numeric(pos[[1]])-1,as.numeric(pos[[1]])+1,0.2),aes(x,y))+
      geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
                aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
      annotate("text",x=0,y=0,label=as.character(pos[[2]]),vjust=0,size=8,fontface="bold")+
      coord_fixed()+
      theme_bw()+
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    return(graph)
  }

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(sum = agg(aggregation, b)) %>%
    dplyr::arrange(desc(sum)) %>%
    dplyr::mutate(order = 1:nrow(.), prop = sum/sum(sum))

  newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
                    data_graph$a, SIMPLIFY=FALSE)

  graphList <- lapply(newList, gg.gauge)
  grid.newpage()
  grid.draw(arrangeGrob(grobs = graphList,ncol=2))
}

#' Dial gauge
#' Gauge
#' @name gg_gauge_dial_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_gauge_dial_CaNu. <- function(data, ...){

  gg.gauge <- function(pos, breaks=c(0,50,100)) {
    require(ggplot2)
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
      th.start <- pi*(1-a/100)
      th.end   <- pi*(1-b/100)
      th       <- seq(th.start,th.end,length=100)
      x        <- c(r1*cos(th),rev(r2*cos(th)))
      y        <- c(r1*sin(th),rev(r2*sin(th)))
      return(data.frame(x,y))
    }
    graph <- ggplot()+
      geom_polygon(data=get.poly(breaks[1],as.numeric(pos[[1]])),aes(x,y),fill = getPalette()[1]) +
      geom_polygon(data=get.poly(as.numeric(pos[[1]]),breaks[3]),aes(x,y),fill = getPalette()[2]) +
      annotate("text",x=0,y=0,label=as.character(pos[[2]]),vjust=0,size=8,fontface="bold")+
      coord_fixed()+
      theme_bw()+
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    return(graph)
  }

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(sum = agg(aggregation, b)) %>%
    dplyr::arrange(desc(sum)) %>%
    dplyr::mutate(order = 1:nrow(.), prop = sum/sum(sum))


  newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
                    data_graph$a, SIMPLIFY=FALSE)

  graphList <- lapply(newList, gg.gauge)
  grid.newpage()
  grid.draw(arrangeGrob(grobs = graphList,ncol=2))
}

#' Vertical boxplot
#' Boxplot
#' @name gg_boxplot_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) +
    geom_boxplot(show.legend = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal boxplot
#' Boxplot flipped
#' @name gg_boxplot_flip_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_flip_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                  yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){


  graph <- gg_boxplot_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical boxplot + jitter
#' Boxplot + dot jitter
#' @name gg_boxplot_dot_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_dot_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) + geom_jitter(color = "#D55E00", show.legend = FALSE) +
    geom_boxplot(show.legend = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal boxplot + jitter
#' Boxplot + dot jitter flipped
#' @name gg_boxplot_dot_flip_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_dot_flip_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){


  graph <- gg_boxplot_dot_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical violin
#' Violin
#' @name gg_violin_mult_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_violin_mult_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) +
    geom_violin(show.legend = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal violin
#' Violin multi flipped
#' @name gg_violin_mult_flip_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_violin_mult_flip_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos = 'right', angle_x = 0,...){

  graph <- gg_violin_mult_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical violin + jitter
#' Violin + dot jitter
#' @name gg_violin_dot_mult_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_violin_dot_mult_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) +
    geom_jitter(color = "#D55E00", show.legend = FALSE) + geom_violin(show.legend = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal violin + jitter
#' Violin + dot jitter flipped
#' @name gg_violin_dot_mult_flip_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_violin_dot_mult_flip_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){

  graph <- gg_violin_dot_mult_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  return(graph)

}

#' Ordered vertical bar
#' Ordered vertical Bars
#' @name gg_bar_ordered_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel =  NULL, leg_pos = "right",  text = TRUE, type = "count",
                                     color_text = "black", aggregation = "sum", angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[2], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = reorder(a, count), y = count, fill = "")) +
    geom_bar(stat = "identity")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    scale_fill_manual(values = getPalette()) + theme_ds() + guides(fill = FALSE)
  graph <- graph + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

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

#' Ordered horizontal bar
#' Ordered horizontal Bars
#' @name gg_bar_ordered_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel =  NULL, leg_pos = "right",  text = TRUE, type = "count",
                                     color_text = "black", aggregation = "sum", angle_x = 0, ...){

  graph <- gg_bar_ordered_ver_CaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos,
                                    text, type, color_text, aggregation, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical bar
#' Vertical Bars
#' @name gg_bar_ver_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ver_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL, line_mean = FALSE, leg_pos = "right", text = TRUE, type = "count",
                             color_text = "black", aggregation = "sum",angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[2], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = "")) +
    geom_bar(stat = "identity")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    scale_fill_manual(values = getPalette()) + theme_ds() + guides(fill = FALSE)
  graph <- graph +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(line_mean){
    graph <- graph + geom_hline(aes(yintercept= mean(data_graph$count)), linetype="dashed")
  }else{
    graph <- graph
  }

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

#' Horizontal bar
#' Horizontal Bars
#' @name gg_bar_hor_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_hor_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL,line_mean = FALSE, leg_pos = "right", text = TRUE, type = "count",
                             color_text = "black", aggregation = "sum", angle_x = 0, ...){

  graph <- gg_bar_ver_CaNu.(data, titleLabel = titleLabel, subtitle = subtitle,
                            caption = caption,
                            xLabel = xLabel, yLabel = yLabel, line_mean = line_mean,
                            leg_pos = leg_pos, text = text, type = type,
                            color_text, aggregation = aggregation, angle_x = angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Steam
#' Steam
#' @name gg_steam_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                           yLabel =  NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% "Índice"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n()) %>%
    tidyr::spread(xorder, b) %>% tidyr::gather(xorder, b, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$xorder <- as.numeric(data_graph$xorder)

  graph <- ggplot(data_graph, aes(x = xorder, y = b, group = a, fill = a)) +
    stat_steamgraph() +
    labs(tittle = titleLabel, x = xlab, y = ylab, fill = clab) +
    scale_fill_manual(values = getPalette()) + theme_ds()
  graph <- graph + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Treemap coloured by first variable
#' Treemap fill by first Ca
#' @name gg_treemap_x_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                               text = TRUE, color_text = "black", aggregation = "sum", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a"),
                       group.label.colour = color_text, group.label.size = 30, group.label.min.size = 10)

  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a"), group.labels = FALSE)
  }

  graph <- graph + guides(fill=FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette())

  graph
}

#' Treemap density by first numeric variable
#' Treemap Density by Nu
#' @name gg_treemap_density_y_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_y_CaNu. <- function(data, titleLabel = "", subtitle = "",
                                       caption = "", fillLabel = NULL, reverse = FALSE,
                                       text = TRUE, color_text = "black", aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a"),
                       group.label.colour = color_text, group.label.size = 30, group.label.min.size = 10)

  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a"), group.labels = FALSE)
  }

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) +
    guides(fill = guide_legend(clab))

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph
}

#' Bubbles
#' Bubbles
#' @name gg_bubble_CaNu2.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CaNu2. <- function(data, titleLabel = "", subtitle = "", caption = "",  sep = 3, lim_inf =-80,
                             lim_sup = 80, xLabel = NULL, aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data <- data %>% dplyr::group_by(a) %>% dplyr::summarise(b = agg(aggregation,b))
  data$b <- rescale(data$b, to = c(5, 30))
  ncircles <- dim(data)[1]
  limits <- c(lim_inf , lim_sup)
  inset <- diff(limits) / sep

  set.seed(7321)
  xyr <- data.frame(
    x = runif(ncircles, min(limits) + inset, max(limits) - inset),
    y = runif(ncircles, min(limits) + inset, max(limits) - inset),
    r = (data$b)) %>% arrange(desc(r))

  res <- circleLayout(xyr, limits, limits, maxiter = 1000)

  dat.after <- circlePlotData(res$layout)

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(id = 1:n(), categoria = a) %>%
    dplyr::select(id, categoria)

  fi <- data.frame(id = 1:dim(data)[1], categoria = data$a)
  fi <- inner_join(data_graph, dat.after)

  cent <- fi %>% dplyr::group_by(categoria) %>%
    dplyr::summarise(x = mean(x), y = mean(y))


  graph <- ggplot(fi) +
    geom_polygon(aes(x, y, group=id, fill = categoria)) +
    scale_fill_manual(values = getPalette()) +
    coord_equal(xlim=limits, ylim=limits) +
    geom_text(data=cent, aes(x, y, label=categoria)) +
    theme_ds() + theme_ds_clean() +
    labs(title=titleLabel) + guides(fill = FALSE)

  return(graph)

}

#' Slope
#' Slope
#' @name gg_slope_CaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_slope_CaNu. <-  function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                            leg_pos="right", overlap = TRUE, text_size = 6,
                            size_point = 3, size_line = 1,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>% group_by(a) %>% dplyr::mutate(xorder = 1:n())

  graph <- ggplot(data_graph) +
    geom_line(aes(x = as.factor(xorder), y = b, group = a, color = a), size = size_line) +
    geom_point(aes(x = as.factor(xorder), y = b, group = a, color = a), size = size_point)+
    theme_ds_clean() +  labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    geom_text(aes(x = as.factor(xorder), y = min(b) - mean(b), label = xorder),
              size = text_size, show.legend = FALSE, check_overlap = TRUE) +
    annotate("text", x = filter(data_graph,xorder == 1)$xorder-.15, y = filter(data_graph,xorder == 1)$b,
             label = filter(data_graph,xorder == 1)$b, check_overlap = overlap) +
    annotate("text", x = filter(data_graph,xorder == 2)$xorder+.15, y = filter(data_graph,xorder == 2)$b,
             label = filter(data_graph,xorder == 2)$b, check_overlap = overlap)+
    scale_color_manual(values = getPalette()) + theme(legend.position = leg_pos)

  return(graph)

}

