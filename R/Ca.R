#' Waffle
#' Waffle
#' @name gg_waffle_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waffle_Ca. <- function(data, square_size = 1, rows_number = 5, titleLabel = "", fillLabel = NULL,
                          subtitle = "", caption = "", leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  parts <- as.vector(data_graph$count)
  graph <- waffle(parts / square_size, rows = rows_number, colors=getPalette()) + theme_ds() +
                  theme_ds_clean()  +
                  labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab)

  graph <- graph + scale_fill_manual(values = getPalette(),
                                     breaks = LETTERS[1:length(unique(data_graph$a))],
                                     labels = unique(data_graph$a)) + theme(legend.position = leg_pos)

  graph
}

#' Vertical coloured bar
#' Vertical coloured bars
#' @name gg_bar_coloured_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_ver_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                    xLabel = NULL, yLabel = NULL,
                                    text = TRUE, type = 'count', color_text = "black",
                                    leg_pos = "right", angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
                dplyr::group_by(a) %>%
                dplyr::summarise(count = n()) %>%
                dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                              pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(y = count , x = a, fill = factor(a))) + geom_bar(stat = 'identity')
  graph <- graph +
           guides(fill = FALSE) +
           theme(legend.position = leg_pos) +
           theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
           labs(title = titleLabel, x= xlab, y = ylab, subtitle = subtitle, caption = caption)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), color = color_text, check_overlap = TRUE))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), color = color_text, check_overlap = TRUE))
    }else{
      graph
    }
  }
}


#' Horizontal coloured bar
#' Horizontal coloured Bars
#' @name gg_bar_coloured_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_hor_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                    xLabel = NULL, yLabel = NULL,
                                    text = TRUE, type = 'count', color_text = "black",
                                    leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_coloured_ver_Ca.(data, titleLabel, subtitle, caption, xLabel,
                                   yLabel, text, type, color_text, leg_pos, angle_x, ...) +  coord_flip()
  return(graph)


}

#' Vertical bar highlighting some parameter
#' Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_ver_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                              xLabel = NULL, yLabel = NULL, parameter = NULL,
                                              text = TRUE, type = 'count', color_text = "black",
                                              leg_pos = "right", angle_x = 0, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  #p <-  parameter %||% sample(unique(data[,nms[1]]), 1)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
                dplyr::group_by(a) %>%
                dplyr::summarise(count = n()) %>%
                dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                              pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  p <- parameter %||% (data_graph %>% filter(count == max(count)))$a

  graph <- ggplot(data_graph, aes(x = a, y = count)) +
           geom_bar(stat="identity", aes(fill = a %in% p ))
  graph <- graph +
           labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)
  graph <- graph + guides(fill = FALSE) + theme(legend.position = leg_pos) +
           theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


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

#' Horizontal bar highlighting some parameter
#' Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_hor_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                              xLabel = NULL, yLabel = NULL, parameter = NULL,
                                              text = TRUE, type = 'count', color_text = "black",
                                              leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_coloured_parameter_ver_Ca.(data, titleLabel, subtitle, caption, xLabel, yLabel,
                                             parameter, text, type, color_text, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()
  graph
}

#' Vertical bar
#' Vertical bars
#' @name gg_bar_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ver_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                           text = TRUE, type = 'count', color_text = "black",
                           leg_pos = "right", angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
                dplyr::group_by(a) %>%
                dplyr::summarise(count = n()) %>%
                dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                              pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph , aes(y = count ,x = factor(a), fill = "")) + geom_bar(stat ='identity') +
           labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)
  graph <- graph + theme(legend.position=leg_pos) + guides(fill = FALSE) +
           theme_ds() +
           scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

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

#' Horizontal bar
#' Horizontal Bars
#' @name gg_bar_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_hor_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                           text = TRUE, type = 'count', color_text = "black",
                           leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_ver_Ca.(data, titleLabel, subtitle, caption, xLabel, yLabel, text, type,
                          color_text, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()
  graph
}

#' Ordered vertical bar
#' Ordered vertical Bars
#' @name gg_bar_ordered_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_ver_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, text = TRUE, type = 'count', color_text = "black",
                                   leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
                dplyr::group_by(a) %>%
                dplyr::summarise(count = n()) %>%
                dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                              pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = reorder(a, count), y = count, fill = "")) +
           geom_bar(stat = "identity")
  graph <- graph +
           labs(title = titleLabel, y = ylab, x = xlab, subtitle = subtitle, caption = caption) +
           theme(legend.position=leg_pos) +
           theme_ds() + scale_fill_manual(values = getPalette()) + guides(fill = FALSE) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

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

#' Ordered horizontal bar
#' Ordered horizontal Bars
#' @name gg_bar_ordered_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_hor_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, text = TRUE, type = 'count', color_text = "black",
                                   leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_ordered_ver_Ca.(data, titleLabel, subtitle, caption, xLabel, yLabel, text, type, color_text,
                                  leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Pie
#' Pie
#' @name gg_pie_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                       fillLabel = NULL, text = TRUE, type = 'count', color_text = "black", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data=data_graph, aes(x = factor(1), y = count, fill = a)) +
           geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = clab,  subtitle = subtitle, caption = caption) +
    guides(text = FALSE)
  graph <- graph  + theme_ds_clean() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
}

#Width debe de ser un parámetro.  0 < width < 1.
#' Donut
#' Donut
#' @name gg_donut_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                         width = 0.3, text = TRUE, type = 'count', color_text = "black", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data=data_graph, aes(x = factor(1), y = count, fill = a)) +
           geom_bar(stat = "identity", width = width) + coord_polar(theta = "y")
  graph <- graph +
           labs(title = titleLabel, x = "", y = "", fill = clab, subtitle = subtitle, caption = caption)
  graph <- graph + theme_ds_clean() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(y = pos, label = round(count,2)), check_overlap = TRUE, stat = "identity", position = "identity",  color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
}

#' Vertical dot bar
#' Vertical Dot Bar
#' @name gg_dot_bar_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_ver_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
                dplyr::group_by(a) %>%
                dplyr::summarise(count = n())
  data_graph <- data_graph %>%
                mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = a, fill = a)) + geom_dotplot(method="histodot", show.legend = FALSE) +
           labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)
  graph <- graph +
           scale_y_continuous(breaks = NULL) +
           theme_ds() + theme(legend.position=leg_pos) +
           scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal dot bar
#' Horizontal Dot Bar
#' @name gg_dot_bar_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_hor_Ca. <- function(data, titleLabel = "", xLabel = NULL, yLabel = NULL,
                               subtitle = "", caption = "", leg_pos = "right", angle_x = 0, ...){

  graph <- gg_dot_bar_ver_Ca.(data, titleLabel, xLabel, yLabel, subtitle = subtitle, caption = caption,
                              leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Horizontal line
#' Horizontal Line
#' @name gg_line_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_Ca. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                            caption = "", yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(order = c(1:nrow(.)))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=1, colour = "")) + geom_line()
  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)

  graph <- graph + theme_minimal() + theme_ds() + theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) + guides(colour = FALSE) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical line
#' Vertical Line
#' @name gg_line_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_ver_Ca. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                            caption = "", yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_line_hor_Ca.(data, titleLabel, xLabel, subtitle, caption, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal line + point
#' Horizontal Line Point
#' @name gg_line_point_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_hor_Ca. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                                  caption = "", yLabel = NULL, leg_pos = "right",
                                  shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(order = c(1:nrow(.)))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=1, colour = "")) + geom_line() + geom_point(type = shape_type)
  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)

  graph <- graph + theme_minimal() + theme_ds() + theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) + guides(colour =FALSE) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical line + point
#' Vertical Line Point
#' @name gg_line_point_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_ver_Ca. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                                  caption = "", yLabel = NULL, leg_pos = "right",
                                  shape_type = 19, angle_x = 0, ...){

  graph <- gg_line_point_hor_Ca.(data, titleLabel, xLabel, subtitle, caption, yLabel, leg_pos, shape_type, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Gauge
#' Gauge
#' @name gg_gauge_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_gauge_Ca. <- function(data, titleLabel = '', subtitle = '', caption = '', ncol = 2, ...){

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
      geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y), fill = getPalette()[1])+
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

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(order = 1:nrow(.), prop = count/(sum(count)))


  newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
                    data_graph$a, SIMPLIFY=FALSE)

  graphList <- lapply(newList, gg.gauge)
  grid.newpage()
  grid.draw(arrangeGrob(grobs = graphList,ncol= ncol))
}

#' Dial gauge
#' Gauge
#' @name gg_gauge_dial_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_gauge_dial_Ca. <- function(data, ...){

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
      geom_polygon(data=get.poly(breaks[1],as.numeric(pos[[1]])),aes(x,y),fill = getPalette()[1])+
      geom_polygon(data=get.poly(as.numeric(pos[[1]]),breaks[3]),aes(x,y),fill = getPalette()[2])+
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

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(order = 1:nrow(.), prop = count/(sum(count)))


  newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
                    data_graph$a, SIMPLIFY=FALSE)

  graphList <- lapply(newList, gg.gauge)
  grid.newpage()
  grid.draw(arrangeGrob(grobs = graphList,ncol=2))
}

#' Vertical stacked bar
#' Single Vertical Stacked Bar
#' @name gg_bar_single_stacked_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_single_stacked_ver_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                          fillLabel = NULL, leg_pos = "right", width = 0.3,
                                          text = TRUE, type = "count", color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(x = factor(1), y = count, fill = a)) +
    geom_bar(stat = "identity", width = width)

  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = clab, subtitle = subtitle, caption = caption)
  graph <- graph + theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette())
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
#' @name gg_bar_single_stacked_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_single_stacked_hor_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                          fillLabel = NULL, leg_pos = "right", width = 0.3,
                                          text = TRUE, type = "count", color_text = "black", ...){

  graph <- gg_bar_single_stacked_ver_Ca.(data, titleLabel, subtitle, caption, fillLabel, leg_pos, width,
                                         text, type, color_text)
  graph <- graph + coord_flip()

  graph
}

#' Bubble
#' Bubble
#' @name gg_bubble_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_Ca.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                           shape_type = 19, angle_x = 0, text = TRUE, type = 'count',
                           color_text = "black",  ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = 0, size = count, color = ""))
  graph <- graph + geom_point(show.legend = FALSE, shape = shape_type)
  graph <- graph + labs(title = titleLabel, x = xlab, y = "", subtitle = subtitle, caption = caption) +
    scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.line=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.grid.major=element_blank()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

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

#' Bubble coloured
#' Coloured Bubble
#' @name gg_bubble_coloured_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_Ca.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    fillLabel = NULL, shape_type = 19, angle_x = 0,  text = TRUE, type = 'count',
                                    color_text = "black",  ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = 0, size = count))
  graph <- graph + geom_point(aes(color = a), show.legend = FALSE, shape = shape_type)
  graph <- graph + labs(title = titleLabel, x = xlab, y = "", fill = flab, subtitle = subtitle, caption = caption)

  graph <- graph + scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

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

#' Polar bar
#' Polar Bar
#' @name gg_bar_polar_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_Ca. <- function(data, width = 0.95, titleLabel = "", subtitle = "", caption = "",
                             fillLabel = NULL, leg_pos= "right", text = TRUE, type = "count", color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(pos = count*8/10,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data=data_graph, aes(a, fill = a, weight = count)) +
            geom_bar(width = width) + coord_polar()
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = clab, subtitle = subtitle, caption = caption)
  graph <- graph + scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
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

#' Bullseye
#' Bullseye
#' @name gg_bullseye_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                            leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = clab, subtitle = subtitle, caption = caption)

  graph <- graph + scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  graph
}

#' Circular bar
#' Circular Bar
#' @name gg_bar_circular_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                leg_pos="right", width = 0.85, text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = count*9.7/10,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = reorder(a, count), y = count , fill = a )) +
    geom_bar(width = width, stat="identity") + coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = clab, subtitle = subtitle, caption = caption)
  graph <- graph + scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
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

#' Treemap coloured by first variable
#' Treemap fill by first Ca
#' @name gg_treemap_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                           text = TRUE, color_text = "black", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a"),
                       group.label.colour = color_text, group.label.size = 20, group.label.min.size = 10)

  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a"), group.labels = FALSE)
  }

  graph <- graph + guides(fill=FALSE) +
    scale_fill_manual(values = getPalette()) + labs(title = titleLabel, subtitle = subtitle, caption =  caption)

  graph
}

#' Bubble
#' bubbles
#' @name gg_bubble_Ca2.
#' @param x A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_Ca2. <- function(data, titleLabel = "", subtitle = "", caption = "",  sep = 3, lim_inf =-40,
                             lim_sup = 40, fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data <- data %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())
  data$b <- rescale(data$b, to = c(5,15))
  ncircles <- dim(data)[1]
  limits <- c(lim_inf , lim_sup)
  inset <- diff(limits) / sep

  set.seed(7321)
   xyr <- data.frame(
     x = runif(ncircles, min(limits) + inset, max(limits) - inset),
     y = runif(ncircles, min(limits) + inset, max(limits) - inset),
     r = (data$b)) %>% arrange(desc(r))

  res <- circleLayout(xyr, limits, limits, maxiter = 1000)

  dat.after <- circlePlotData(res$layout, npoints = 1000)

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
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
    labs(title=titleLabel, subtitle = subtitle, caption = caption) + guides(fill = FALSE)

  return(graph)

}
